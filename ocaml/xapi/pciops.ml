(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
module D = Debug.Make (struct let name = "pciops" end)

open D
open Xapi_stdext_threads.Threadext

let m = Mutex.create ()

(* http://wiki.xen.org/wiki/Bus:Device.Function_%28BDF%29_Notation *)
(* It might be possible to refactor this but attempts so far have failed. *)
let bdf_fmt = format_of_string "%04x:%02x:%02x.%01x"

let bdf_fmt_ignore = format_of_string "%_4x:%_2x:%_2x.%_1x%!"

(* with end-of-input match *)
let slash_bdf_scan_fmt = format_of_string "%d/%04x:%02x:%02x.%01x"

let slash_bdf_prnt_fmt = format_of_string "%d/%04x:%02x:%02x.%01x"

let bdf_paren_prnt_fmt = format_of_string "(%04x:%02x:%02x.%01x)"

let bdf_paren_scan_fmt = format_of_string "(%04x:%02x:%02x.%01x)"

let bdf_paren_list_fmt = format_of_string "(%04x:%02x:%02x.%01x)%s"

let pcidev_of_pci ~__context pci =
  let bdf_str = Db.PCI.get_pci_id ~__context ~self:pci in
  Scanf.sscanf bdf_str bdf_fmt (fun a b c d -> (a, b, c, d))

let bdf_paren_of_pcidev (a, b, c, d) = Printf.sprintf bdf_paren_prnt_fmt a b c d

(* Confusion: the n/xxxx:xx:xx.x syntax originally meant PCI device
   xxxx:xx:xx.x should be plugged into bus number n. HVM guests don't have
   multiple PCI buses anyway. We reinterpret the 'n' to be a hotplug ordering *)
let sort_pcidevs devs =
  let ids = List.sort_uniq compare (List.map fst devs) in
  List.map
    (fun id -> (id, List.map snd (List.filter (fun (x, _) -> x = id) devs)))
    ids

let of_string dev =
  Scanf.sscanf dev slash_bdf_scan_fmt (fun id a b c d -> (id, (a, b, c, d)))

let to_string (id, (a, b, c, d)) = Printf.sprintf slash_bdf_prnt_fmt id a b c d

let other_pcidevs_of_vm ~__context other_config =
  let devs =
    try
      let oc = List.assoc "pci" other_config in
      String.split_on_char ',' oc
    with Not_found -> []
  in
  List.fold_left
    (fun acc dev -> try of_string dev :: acc with _ -> acc)
    [] devs

let pci_hiding_key = "xen-pciback.hide"

let pci_hiding_key_eq = pci_hiding_key ^ "="

let get_pci_hidden_raw_value () =
  let cmd = !Xapi_globs.xen_cmdline_path ^ " --get-dom0 " ^ pci_hiding_key in
  let raw_kv_string = Helpers.get_process_output cmd in
  (* E.g. "xen-pciback.hide=(0000:00:02.0)(0000:00:02.1)\n" or just "\n" *)
  Scanf.ksscanf raw_kv_string
    (fun _ _ -> None)
    "xen-pciback.hide=%s\n" Option.some

let get_hidden_pcidevs () =
  let rec read_dev devs raw =
    match raw with
    | "" ->
        devs
    | _ ->
        Scanf.sscanf raw bdf_paren_list_fmt (fun a b c d rest ->
            read_dev ((a, b, c, d) :: devs) rest)
  in
  Option.fold ~none:[] ~some:(read_dev []) (get_pci_hidden_raw_value ())

let _is_pci_hidden ~__context pci hidden_pcidevs =
  let pcidev = pcidev_of_pci ~__context pci in
  List.mem pcidev hidden_pcidevs

(** Check whether a PCI device will be hidden from the dom0 kernel on boot. *)
let is_pci_hidden ~__context pci =
  Mutex.execute m (fun () ->
      _is_pci_hidden ~__context pci (get_hidden_pcidevs ()))

let _hide_pci ~__context pci =
  let hidden_pcidevs = get_hidden_pcidevs () in
  if _is_pci_hidden ~__context pci hidden_pcidevs then
    let p = pcidev_of_pci ~__context pci in
    let devs = p :: hidden_pcidevs in
    let valstr = String.concat "" (List.rev_map bdf_paren_of_pcidev devs) in
    let cmd =
      Printf.sprintf "%s --set-dom0 %s%s"
        !Xapi_globs.xen_cmdline_path
        pci_hiding_key_eq valstr
    in
    let (_ : string) = Helpers.get_process_output cmd in
    ()

(** Hide a PCI device from the dom0 kernel. (Takes effect after next boot.) *)
let hide_pci ~__context pci =
  Mutex.execute m (fun () -> _hide_pci ~__context pci)

let _unhide_pci ~__context pci =
  let hidden_pcidevs = get_hidden_pcidevs () in
  if _is_pci_hidden ~__context pci hidden_pcidevs then
    let pci_dev = pcidev_of_pci ~__context pci in
    let new_value =
      List.filter (fun x -> x <> pci_dev) hidden_pcidevs
      |> List.rev_map bdf_paren_of_pcidev
      |> String.concat ""
    in
    let cmd =
      match new_value with
      | "" ->
          Printf.sprintf "%s --delete-dom0 %s"
            !Xapi_globs.xen_cmdline_path
            pci_hiding_key
      | _ ->
          Printf.sprintf "%s --set-dom0 %s%s"
            !Xapi_globs.xen_cmdline_path
            pci_hiding_key_eq new_value
    in
    let (_ : string) = Helpers.get_process_output cmd in
    ()

(** Unhide a PCI device from the dom0 kernel. (Takes effect after next boot.) *)
let unhide_pci ~__context pci =
  Mutex.execute m (fun () -> _unhide_pci ~__context pci)

(** Return the id of a PCI device *)
let id_of (id, (domain, bus, dev, fn)) = id

(** Return the domain of a PCI device *)
let domain_of (id, (domain, bus, dev, fn)) = domain

(** Return the bus of a PCI device *)
let bus_of (id, (domain, bus, dev, fn)) = bus

(** Return the device of a PCI device *)
let dev_of (id, (domain, bus, dev, fn)) = dev

(** Return the function of a PCI device *)
let fn_of (id, (domain, bus, dev, fn)) = fn

(** Find a free virtual function given a physical function (SR-IOV) *)
let reserve_free_virtual_function ~__context vm pf =
  let rec search = function
    | [] ->
        None
    | (vf, _) :: vfs ->
        let attached = Db.PCI.get_attached_VMs ~__context ~self:vf <> [] in
        let scheduled =
          Db.PCI.get_scheduled_to_be_attached_to ~__context ~self:vf <> Ref.null
        in
        if attached || scheduled then
          search vfs
        else (
          Db.PCI.set_scheduled_to_be_attached_to ~__context ~self:vf ~value:vm ;
          Some vf
        )
  in
  Db.PCI.get_virtual_functions ~__context ~self:pf
  |> List.map (fun vf -> (vf, pcidev_of_pci ~__context vf))
  |> List.sort (fun (_, a) (_, b) -> compare a b) (* prefer low BDF numbers *)
  |> search
