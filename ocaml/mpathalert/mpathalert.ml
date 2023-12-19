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
module Xstringext = Xapi_stdext_std.Xstringext
module Listext = Xapi_stdext_std.Listext
open Client
open Event_types

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let print_debug = ref false

let delay = ref 120.

let lock = Mutex.create ()

let with_global_lock (f : unit -> unit) = with_lock lock f

let time_of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ" (time.Unix.tm_year + 1900)
    (time.Unix.tm_mon + 1) time.Unix.tm_mday time.Unix.tm_hour time.Unix.tm_min
    time.Unix.tm_sec

let stdout_m = Mutex.create ()

let debug (fmt : ('a, unit, string, unit) format4) =
  if !print_debug then
    with_lock stdout_m (fun () ->
        Printf.kprintf
          (fun s ->
            Printf.printf "%s [%d] %s\n"
              (time_of_float (Unix.gettimeofday ()))
              (Thread.id (Thread.self ()))
              s ;
            flush stdout
          )
          fmt
    )
  else
    Printf.kprintf (Fun.const ()) fmt

type t = {
    host: [`host] Uuidx.t
  ; host_name: string
  ; pbd: [`pbd] Uuidx.t
  ; timestamp: float
  ; scsi_id: string
  ; current: int
  ; max: int
}

let to_string alert =
  if alert.pbd <> Uuidx.null then
    Printf.sprintf
      "[%s] host=%s; host-name=\"%s\"; pbd=%s; scsi_id=%s; current=%d; max=%d"
      (time_of_float alert.timestamp)
      (String.escaped (Uuidx.to_string alert.host))
      alert.host_name
      (Uuidx.to_string alert.pbd)
      alert.scsi_id alert.current alert.max
  else
    Printf.sprintf
      "[%s] host=%s; host-name=\"%s\"; root=true; current=%d; max=%d"
      (time_of_float alert.timestamp)
      (String.escaped (Uuidx.to_string alert.host))
      alert.host_name alert.current alert.max

(* execute f within an active session *)
let rec retry_with_session f rpc x =
  let session_id =
    let rec aux () =
      try
        Client.Session.login_with_password ~rpc ~uname:"" ~pwd:"" ~version:"1.4"
          ~originator:"mpathalert"
      with _ -> Thread.delay !delay ; aux ()
    in
    aux ()
  in
  try f rpc session_id x
  with e ->
    (try Client.Session.logout ~rpc ~session_id with _ -> ()) ;
    debug "Got '%s', trying with a new session ..." (Printexc.to_string e) ;
    Thread.delay !delay ;
    retry_with_session f rpc x

let keep_mpath =
  List.filter (fun (key, _) -> String.starts_with ~prefix:"mpath-" key)

let create_alert ~host_uuid_string ~host_name ~pbd_uuid_string key value
    timestamp scsi_id =
  let current, max =
    Scanf.sscanf value "[%d, %d]" (fun current max -> (current, max))
  in
  let host_maybe = Uuidx.of_string host_uuid_string in
  let pbd_maybe = Uuidx.of_string pbd_uuid_string in
  let host_error : ('a -> 'b -> 'c -> 'd, unit, string, unit) format4 =
    "Retrieved malformed UUID for host (%s) while creating PBD alert from \
     %s=%s; ignoring entry"
  in
  let pbd_error : ('a -> 'b -> 'c -> 'd, unit, string, unit) format4 =
    "Retrieved malformed UUID for pbd (%s) while creating PBD alert from \
     %s=%s; ignoring entry"
  in
  match (host_maybe, pbd_maybe) with
  | None, None ->
      debug host_error host_uuid_string key value ;
      debug pbd_error host_uuid_string key value ;
      None
  | None, _ ->
      debug host_error host_uuid_string key value ;
      None
  | _, None ->
      debug pbd_error host_uuid_string key value ;
      None
  | Some host, Some pbd ->
      let alert = {host; host_name; pbd; timestamp; scsi_id; current; max} in
      debug "Alert '%s' created from %s=%s" (to_string alert) key value ;
      Some alert

(* create a list of alerts from a PBD event *)
let create_pbd_alerts rpc session_id snapshot (_pbd_ref, pbd_rec, timestamp) =
  let aux (key, value) =
    let scsi_id = Xstringext.String.sub_to_end key 6 in
    let host_uuid_string =
      Client.Host.get_uuid ~rpc ~session_id ~self:pbd_rec.API.pBD_host
    in
    let host_name =
      Client.Host.get_name_label ~rpc ~session_id ~self:pbd_rec.API.pBD_host
    in
    let pbd_uuid_string = pbd_rec.API.pBD_uuid in
    create_alert ~host_uuid_string ~host_name ~pbd_uuid_string key value
      timestamp scsi_id
  in
  let diff =
    Listext.List.set_difference
      (keep_mpath pbd_rec.API.pBD_other_config)
      snapshot
  in
  List.filter_map aux diff

(* create a list of alerts from a host event *)
let create_host_alerts _rpc _session snapshot (_, host_rec, timestamp) =
  let aux (key, value) =
    let scsi_id = "n/a" in
    let host_uuid_string = host_rec.API.host_uuid in
    let host_name = host_rec.API.host_name_label in
    let pbd_uuid_string = Uuidx.(to_string null) in
    create_alert ~host_uuid_string ~host_name ~pbd_uuid_string key value
      timestamp scsi_id
  in
  let diff =
    Listext.List.set_difference
      (keep_mpath host_rec.API.host_other_config)
      snapshot
  in
  List.filter_map aux diff

let listener rpc session_id queue =
  let snapshot = Hashtbl.create 48 in
  let update_snapshot r other_config =
    let r = Ref.string_of r in
    if Hashtbl.mem snapshot r then
      debug "Update an entry of the snapshot table: %s" r
    else
      debug "Add a new entry to the snapshot table: %s" r ;
    Hashtbl.replace snapshot r other_config
  in
  let remove_from_snapshot r =
    let r = Ref.string_of r in
    debug "Remove an entry to the snapshot table: %s" r ;
    Hashtbl.remove snapshot r
  in
  let get_snapshot r = Hashtbl.find snapshot (Ref.string_of r) in
  Client.Event.register ~rpc ~session_id ~classes:["pbd"; "host"] ;
  (* populate the snapshot cache *)
  let pbds = Client.PBD.get_all_records ~rpc ~session_id in
  List.iter
    (fun (pbd_ref, pbd_rec) ->
      update_snapshot pbd_ref (keep_mpath pbd_rec.API.pBD_other_config)
    )
    pbds ;
  let hosts = Client.Host.get_all_records ~rpc ~session_id in
  List.iter
    (fun (host_ref, host_rec) ->
      update_snapshot host_ref (keep_mpath host_rec.API.host_other_config)
    )
    hosts ;
  (* proceed events *)
  let proceed event =
    match Event_helper.record_of_event event with
    | Event_helper.PBD (pbd_ref, pbd_rec_opt) -> (
      match (event.op, pbd_rec_opt) with
      | `add, Some pbd_rec ->
          debug "Processing an ADD event" ;
          update_snapshot pbd_ref (keep_mpath pbd_rec.API.pBD_other_config)
      | `del, _ ->
          debug "Processing a DEL event" ;
          remove_from_snapshot pbd_ref
      | `_mod, Some pbd_rec ->
          let alerts =
            create_pbd_alerts rpc session_id (get_snapshot pbd_ref)
              (pbd_ref, pbd_rec, float_of_string event.ts)
          in
          debug "Processing a MOD event" ;
          List.iter
            (fun alert -> with_global_lock (fun () -> Queue.push alert queue))
            alerts ;
          update_snapshot pbd_ref (keep_mpath pbd_rec.API.pBD_other_config)
      | _ ->
          () (* this should never happens *)
    )
    | Event_helper.Host (host_ref, host_rec_opt) -> (
      match (event.op, host_rec_opt) with
      | `add, Some host_rec ->
          debug "Processing an ADD event" ;
          update_snapshot host_ref (keep_mpath host_rec.API.host_other_config)
      | `del, _ ->
          debug "Processing a DEL event" ;
          remove_from_snapshot host_ref
      | `_mod, Some host_rec ->
          debug "Processing a MOD event" ;
          let alerts =
            create_host_alerts rpc session_id (get_snapshot host_ref)
              (host_ref, host_rec, float_of_string event.ts)
          in
          List.iter
            (fun alert -> with_global_lock (fun () -> Queue.push alert queue))
            alerts ;
          update_snapshot host_ref (keep_mpath host_rec.API.host_other_config)
      | _ ->
          () (* this should never happens *)
    )
    | _ ->
        ()
    (* this should never happen *)
  in
  (* infinite loop *)
  while true do
    let events =
      Event_types.events_of_rpc (Client.Event.next ~rpc ~session_id)
    in
    List.iter proceed events
  done

let state_of_the_world rpc session_id =
  debug "Generating the current state of the world" ;
  let pbds = Client.PBD.get_all_records ~rpc ~session_id in
  let pbd_alerts =
    List.flatten
      (List.map
         (fun (pbd_ref, pbd_rec) ->
           create_pbd_alerts rpc session_id []
             (pbd_ref, pbd_rec, Unix.gettimeofday ())
         )
         pbds
      )
  in
  let hosts = Client.Host.get_all_records ~rpc ~session_id in
  let host_alerts =
    List.flatten
      (List.map
         (fun (host_ref, host_rec) ->
           create_host_alerts rpc session_id []
             (host_ref, host_rec, Unix.gettimeofday ())
         )
         hosts
      )
  in
  let alerts =
    List.filter
      (fun alert -> alert.current <> alert.max)
      (pbd_alerts @ host_alerts)
  in
  debug "State of the world generated" ;
  alerts

let sender rpc session_id (delay, msg, queue) =
  debug "Start sender with delay=%.0f seconds" delay ;
  let pool_uuid =
    let _, pool_rec = List.hd (Client.Pool.get_all_records ~rpc ~session_id) in
    pool_rec.API.pool_uuid
  in
  let tmp = Buffer.create 1024 in
  (* Hashtable containing all the broken scsi_id saw since the last wake up *)
  let broken_history = Hashtbl.create 32 in
  let update_broken_history alert =
    if alert.max <> alert.current then (
      debug "Updating '%s' in the broken history" (to_string alert) ;
      Hashtbl.replace broken_history (alert.pbd, alert.scsi_id) ()
    ) else (
      debug "Removing '%s' of the broken history" (to_string alert) ;
      Hashtbl.remove broken_history (alert.pbd, alert.scsi_id)
    )
  in
  let remember_broken_history state_of_the_world =
    debug "Cleaning and re-populating the broken history" ;
    Hashtbl.clear broken_history ;
    List.iter update_broken_history state_of_the_world
  in
  let was_broken pbd scsi_id = Hashtbl.mem broken_history (pbd, scsi_id) in
  (* if the alert scsi_id was broken or is broken, generates the alert; then, update the history of broken scsi_id *)
  let interesting_alert = ref false in
  let proceed alert =
    if was_broken alert.pbd alert.scsi_id || alert.current <> alert.max then (
      debug "Adding '%s' to the temp buffer as was_broken=%b and is_broken=%b"
        (to_string alert)
        (was_broken alert.pbd alert.scsi_id)
        (alert.current <> alert.max) ;
      interesting_alert := true ;
      Buffer.add_string tmp (to_string alert ^ "\n")
    ) else
      debug "Ignoring '%s' as was_broken=%b and is_broken=%b" (to_string alert)
        (was_broken alert.pbd alert.scsi_id)
        (alert.current <> alert.max) ;
    update_broken_history alert
  in
  while true do
    debug "Wake up" ;
    let state_of_the_world = state_of_the_world rpc session_id in
    with_global_lock (fun () ->
        if not (Queue.is_empty queue) then (
          (* write everything on a tempary buffer *)
          Buffer.clear tmp ;
          (* update the state of the world *)
          ( if state_of_the_world <> [] then
              let alert_msgs = List.map to_string state_of_the_world in
              Buffer.add_string tmp
                (Printf.sprintf "Unhealthy paths:\n%s\n"
                   (String.concat "\n" alert_msgs)
                )
          ) ;
          (* update the received events *)
          Buffer.add_string tmp
            (Printf.sprintf "Events received during the last %.0f seconds:\n"
               delay
            ) ;
          interesting_alert := false ;
          while not (Queue.is_empty queue) do
            proceed (Queue.pop queue)
          done ;
          (* if an intersting alert had been proceeded, then commit our changes to the msg buffer *)
          if !interesting_alert then
            Buffer.add_buffer msg tmp
        )
    ) ;
    if Buffer.length msg <> 0 then (
      let name, priority = Api_messages.multipath_periodic_alert in
      let (_ : API.ref_message) =
        Client.Message.create ~rpc ~session_id ~name ~priority ~cls:`Pool
          ~obj_uuid:pool_uuid ~body:(Buffer.contents msg)
      in
      remember_broken_history state_of_the_world ;
      Buffer.clear msg
    ) ;
    Thread.delay delay
  done

let _ =
  Arg.parse
    (Arg.align
       [
         ("-debug", Arg.Set print_debug, " Print debug messages")
       ; ( "-delay"
         , Arg.Set_float delay
         , Printf.sprintf
             " Set the delay, in seconds, between 2 consecutive alerts \
              (default is %.0f)"
             !delay
         )
       ]
    )
    (fun _ -> failwith "Invalid argument")
    "Usage: mpathalert [-debug] [-delay time to wait between alerts]" ;
  let rpc xml =
    let open Xmlrpc_client in
    let http = xmlrpc ~version:"1.0" "/" in
    XMLRPC_protocol.rpc ~srcstr:"mpathalert" ~dststr:"xapi"
      ~transport:(Unix (Filename.concat "/var/lib/xcp" "xapi"))
      ~http xml
  in
  let queue = Queue.create () in
  let msg = Buffer.create 1024 in
  let (t1 : Thread.t) = Thread.create (retry_with_session listener rpc) queue in
  let (t2 : Thread.t) =
    Thread.create (retry_with_session sender rpc) (!delay, msg, queue)
  in
  Thread.join t1 ; Thread.join t2
