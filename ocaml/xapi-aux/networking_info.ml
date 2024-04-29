(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
module Net = Network_client.Client

module L = Debug.Make (struct let name = __MODULE__ end)

let get_hostname () = try Unix.gethostname () with _ -> ""

exception Unexpected_address_type of string

(* Try to get all FQDNs, avoid localhost *)
let dns_names () =
  let hostname = get_hostname () in
  let fqdns =
    Unix.getaddrinfo hostname "" [Unix.AI_CANONNAME]
    |> List.map (fun x -> x.Unix.ai_canonname)
  in
  hostname :: fqdns
  |> List.filter_map (fun x ->
         let x = Astring.String.trim x in
         if
           String.equal "" x
           || String.equal "localhost" x
           || Ipaddr.of_string x |> Stdlib.Result.is_ok
         then
           None
         else
           Some x
     )
  |> Astring.String.uniquify

let ipaddr_to_cstruct = function
  | Ipaddr.V4 addr ->
      Cstruct.of_string (Ipaddr.V4.to_octets addr)
  | Ipaddr.V6 addr ->
      Cstruct.of_string (Ipaddr.V6.to_octets addr)

let get_management_ip_addrs ~dbg =
  let iface = Inventory.lookup Inventory._management_interface in
  try
    if iface = "" || (not @@ Net.Interface.exists dbg iface) then
      ([], [])
    else
      let addrs =
        let ipv4 = Net.Interface.get_ipv4_addr dbg iface in
        let ipv6 = Net.Interface.get_ipv6_addr dbg iface in
        match
          String.lowercase_ascii
            (Inventory.lookup Inventory._management_address_type ~default:"ipv4")
        with
        | "ipv4" ->
            (ipv4, ipv6)
        | "ipv6" ->
            (ipv6, ipv4)
        | s ->
            let msg = Printf.sprintf "Expected 'ipv4' or 'ipv6', got %s" s in
            L.error "%s: %s" __FUNCTION__ msg ;
            raise (Unexpected_address_type msg)
      in
      (* Filter out link-local addresses *)
      let no_local (addr, _) =
        let addr = Ipaddr_unix.of_inet_addr addr in
        if Ipaddr.scope addr <> Ipaddr.Link then
          Some addr
        else
          None
      in
      ( List.filter_map no_local (fst addrs)
      , List.filter_map no_local (snd addrs)
      )
  with _ -> ([], [])

let get_management_ip_addr ~dbg =
  let preferred, _ = get_management_ip_addrs ~dbg in
  List.nth_opt preferred 0
  |> Option.map (fun addr -> (Ipaddr.to_string addr, ipaddr_to_cstruct addr))

let get_host_certificate_subjects ~dbg =
  let ( let* ) = Result.bind in
  let* ips, preferred_ip =
    match get_management_ip_addrs ~dbg with
    | [], [] ->
        Error "Could not get the management IP address"
    | preferred, others ->
        let ips = List.(rev_append (rev preferred) others) in
        Ok (List.map ipaddr_to_cstruct ips, List.nth ips 0)
  in
  let dns_names = dns_names () in
  let name =
    match dns_names with [] -> Ipaddr.to_string preferred_ip | dns :: _ -> dns
  in
  Ok (name, dns_names, ips)
