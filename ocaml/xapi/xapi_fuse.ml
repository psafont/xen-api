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
(* Xapi_fuse: Code to cause Xapi to commit not-completely-terminal hara-kiri *)
(* The watchdog catches the exit()s and restarts us *)

module D = Debug.Make (struct let name = "xapi_fuse" end)

module Delay = Xapi_stdext_threads.Threadext.Delay
open D
module Rrdd = Rrd_client.Client

type fuse_kind = Exit | Reboot

type fuse_state = {m: Mutex.t; mutable already_lit: fuse_kind list}

let kind_to_string = function Exit -> "Exit" | Reboot -> "Reboot"

let fuses = {m= Mutex.create (); already_lit= []}

let once kind f =
  Xapi_stdext_threads.Threadext.Mutex.execute fuses.m (fun () ->
      if List.mem kind fuses.already_lit then
        debug "%s: the %s fuse is already lit: no-op" __FUNCTION__
          (kind_to_string kind)
      else (
        fuses.already_lit <- kind :: fuses.already_lit ;
        f ()
      )
  )

let delay fuse = ignore (Delay.(wait (make ()) fuse) : bool)

let light_fuse_and_run ?(fuse_length = !Constants.fuse_time) () =
  once Exit @@ fun () ->
  let fuse = Mtime_clock.counter () in
  debug "%s: calling Rrdd.backup_rrds to save current RRDs locally" __FUNCTION__ ;
  log_and_ignore_exn Xapi_stats.stop ;
  log_and_ignore_exn (Rrdd.backup_rrds None) ;
  debug "%s: current RRDs have been saved" __FUNCTION__ ;
  let delay_so_far = Mtime_clock.count fuse in
  let fuse_length =
    Mtime.Span.abs_diff fuse_length delay_so_far
    |> Clock.Timer.span_longest Mtime.Span.(5 * s)
  in
  ( Thread.create Tracing_export.(flush_and_exit ~max_wait:fuse_length) ()
    : Thread.t
    )
  |> ignore ;
  ( Thread.create
      (fun () ->
        let open Xapi_database in
        delay fuse_length ;
        debug "%s: calling flush and exit" __FUNCTION__ ;
        (* CA-16368: If the database hasn't been initialised *at all* we can
           exit immediately. This happens if someone calls flush_and_exit
           before the db conf has been parsed, the connections initialised and
           the database "mode" set. *)
        try
          let dbconn = Db_connections.preferred_write_db () in
          let lock_db =
            if Pool_role.is_master () then
              Db_lock.with_lock
            else
              fun f -> f ()
          in
          lock_db (fun () ->
              Db_cache_impl.flush_and_exit dbconn Xapi_globs.restart_return_code
          )
        with e ->
          warn
            "Caught an exception flushing database (perhaps it hasn't been \
             initialised yet): %s; restarting immediately"
            (ExnHelper.string_of_exn e) ;
          exit Xapi_globs.restart_return_code
      )
      ()
    : Thread.t
    )
  |> ignore

let light_fuse_and_reboot_after_eject () =
  once Reboot @@ fun () ->
  ( Thread.create
      (fun () ->
        delay !Constants.fuse_time ;
        (* this activates firstboot script and reboots the host *)
        ignore
          (Forkhelpers.execute_command_get_output
             "/opt/xensource/libexec/reset-and-reboot" []
          ) ;
        ()
      )
      ()
    : Thread.t
    )
  |> ignore

let light_fuse_and_reboot ?(fuse_length = !Constants.fuse_time) () =
  once Reboot @@ fun () ->
  ( Thread.create
      (fun () ->
        delay fuse_length ;
        ignore (Sys.command "shutdown -r now")
      )
      ()
    : Thread.t
    )
  |> ignore

let light_fuse_and_dont_restart ?(fuse_length = !Constants.fuse_time) () =
  once Exit @@ fun () ->
  ( Thread.create
      (fun () ->
        let open Xapi_database in
        debug "%s: calling Rrdd.backup_rrds to save current RRDs locally"
          __FUNCTION__ ;
        log_and_ignore_exn Xapi_stats.stop ;
        log_and_ignore_exn (Rrdd.backup_rrds None) ;
        delay fuse_length ;
        let lock_db =
          if Pool_role.is_master () then Db_lock.with_lock else fun f -> f ()
        in
        lock_db (fun () ->
            Db_cache_impl.flush_and_exit
              (Db_connections.preferred_write_db ())
              0
        )
      )
      ()
    : Thread.t
    )
  |> ignore ;
  (* This is a best-effort attempt to use the database. We must not block the flush_and_exit above, hence
     the use of a background thread. *)
  Helpers.log_exn_continue "setting Host.enabled to false"
    (fun () ->
      Server_helpers.exec_with_new_task "Setting Host.enabled to false"
        (fun __context ->
          debug "About to set Host.enabled to false" ;
          let localhost = Helpers.get_localhost ~__context in
          Db.Host.set_enabled ~__context ~self:localhost ~value:false ;
          Helpers.call_api_functions ~__context (fun rpc session_id ->
              Db_gc.send_one_heartbeat ~__context rpc ~shutting_down:true
                session_id
          )
      )
    )
    ()
