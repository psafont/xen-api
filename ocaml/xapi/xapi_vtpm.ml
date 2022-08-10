(*
   Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

(** Don't allow unless VTPM is enabled as an experimental feature *)
let assert_not_restricted ~__context =
  let pool = Helpers.get_pool ~__context in
  let restrictions = Db.Pool.get_restrictions ~__context ~self:pool in
  let feature = "restrict_vtpm" in
  match List.assoc_opt feature restrictions with
  | Some "false" ->
      ()
  | _ ->
      raise Api_errors.(Server_error (feature_restricted, [feature]))

let assert_no_vtpm_associated ~__context vm =
  match Db.VM.get_VTPMs ~__context ~self:vm with
  | [] ->
      ()
  | vtpms ->
      let amount = List.length vtpms |> Int.to_string in
      raise Api_errors.(Server_error (vtpm_max_amount_reached, [amount]))

let choose_sr ~__context ~vM ~sR =
  if sR <> Ref.null then
    sR
  else
    let pool = Helpers.get_pool ~__context in
    let sr = Db.Pool.get_suspend_image_SR ~__context ~self:pool in
    if sr <> Ref.null then
      sr
    else
      let snapshot = Db.VM.get_record ~__context ~self:vM in
      let hosts =
        Xapi_vm_helpers.get_possible_hosts_for_vm ~__context ~vm:vM ~snapshot
      in
      match
        List.filter_map
          (fun host ->
            let sr = Db.Host.get_suspend_image_sr ~__context ~self:host in
            if sr <> Ref.null then Some sr else None
          )
          hosts
      with
      | sr :: _ ->
          sr
      | _ ->
          Xapi_vm_helpers.list_required_SRs ~__context ~self:vM |> List.hd
(* TODO: perhaps chose SR with largest amount of free space? *)

let nv_memory_size = (128 * 1024) + (65 * 704) (* NV_MEMORY_SIZE from libtpms *)

let introduce ~__context ~vM ~vDI ~is_unique =
  let ref = Ref.make () in
  let uuid = Uuidx.(to_string (make ())) in
  let backend = Ref.null in
  let persistence_backend = `vdi in
  Db.VTPM.create ~__context ~ref ~uuid ~vM ~backend ~persistence_backend
    ~is_unique ~is_protected:false ~vDI ~contents:Ref.null ;
  ref

(** Contents from unique vtpms cannot be copied! *)
let copy_contents_or_new ~__context ?from () =
  let create () = Xapi_secret.create ~__context ~value:"" ~other_config:[] in
  let copy ref =
    let contents = Db.VTPM.get_contents ~__context ~self:ref in
    Xapi_secret.copy ~__context ~secret:contents
  in
  let maybe_copy ref =
    if Db.VTPM.get_is_unique ~__context ~self:ref then
      create ()
    else
      copy ref
  in
  Option.fold ~none:(create ()) ~some:maybe_copy from

let create ~__context ~vM ~sR ~is_unique =
  assert_not_restricted ~__context ;
  assert_no_vtpm_associated ~__context vM ;
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vM
    ~expected:`Halted ;
  let vm_uuid = Db.VM.get_uuid ~__context ~self:vM in
  let name_label = "VTPM state for " ^ vm_uuid in
  let vDI =
    Xapi_vdi.create ~__context ~_type:`vtpm_state
      ~sR:(choose_sr ~__context ~vM ~sR)
      ~name_label ~name_description:name_label ~sharable:false ~read_only:false
      ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]
      ~virtual_size:(Int64.of_int nv_memory_size)
  in
  introduce ~__context ~vM ~vDI ~is_unique

let copy ~__context ~vM ref =
  let vtpm = Db.VTPM.get_record ~__context ~self:ref in
  let vDI =
    match vtpm.vTPM_persistence_backend with
    | `vdi ->
        (* TODO pick the cloned VDI *)
        Ref.null
    | `xapi ->
        (* TODO properly copy the contents to a new VDI *)
        let _contents = copy_contents_or_new ~__context ~from:ref () in
        Ref.null
  in
  let is_unique = vtpm.vTPM_is_unique in
  introduce ~__context ~vM ~vDI ~is_unique

let destroy ~__context ~self =
  let record = Db.VTPM.get_record ~__context ~self in
  let vm = record.vTPM_VM in
  Xapi_vm_lifecycle.assert_initial_power_state_is ~__context ~self:vm
    ~expected:`Halted ;
  let () =
    match record.vTPM_persistence_backend with
    | `xapi ->
        let secret = Db.VTPM.get_contents ~__context ~self in
        Db.Secret.destroy ~__context ~self:secret
    | `vdi ->
        Xapi_vdi.destroy ~__context ~self:record.vTPM_VDI
  in
  Db.VTPM.destroy ~__context ~self

let get_contents ~__context ~self =
  let secret = Db.VTPM.get_contents ~__context ~self in
  Db.Secret.get_value ~__context ~self:secret

let set_contents ~__context ~self ~contents =
  let previous_secret = Db.VTPM.get_contents ~__context ~self in
  let _ =
    (* verify contents to be already base64-encoded *)
    try Base64.decode contents
    with Invalid_argument err ->
      raise Api_errors.(Server_error (internal_error, [err]))
  in
  let secret = Xapi_secret.create ~__context ~value:contents ~other_config:[] in
  Db.VTPM.set_contents ~__context ~self ~value:secret ;
  Db.Secret.destroy ~__context ~self:previous_secret
