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

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module Delay = Xapi_stdext_threads.Threadext.Delay

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

type func_ty = OneShot | Periodic of Mtime.Span.t

type t = {func: unit -> unit; ty: func_ty; name: string}

let delay = Delay.make ()

let queue_default = {func= (fun () -> ()); ty= OneShot; name= ""}

let (pending_event : t option ref) = ref None

let (queue : t Ipq.t) = Ipq.create 50 queue_default

let lock = Mutex.create ()

let elapsed = Mtime_clock.counter ()

let add_to_queue name ty start newfunc =
  let time = Mtime.Span.add (Mtime_clock.count elapsed) start in
  with_lock lock (fun () ->
      Ipq.(add queue {ev= {func= newfunc; ty; name}; time})
  ) ;
  Delay.signal delay

let remove_from_queue name =
  with_lock lock @@ fun () ->
  match !pending_event with
  | Some ev when ev.name = name ->
      pending_event := None
  | Some _ | None ->
      let index = Ipq.find_p queue (fun {name= n; _} -> name = n) in
      if index > -1 then
        Ipq.remove queue index

let add_periodic_pending () =
  with_lock lock @@ fun () ->
  match !pending_event with
  | Some ({ty= Periodic timer; _} as ev) ->
      let now = Mtime_clock.count elapsed in
      let item = {Ipq.ev; Ipq.time= Mtime.Span.add now timer} in
      Ipq.add queue item ;
      pending_event := None
  | Some {ty= OneShot; _} ->
      pending_event := None
  | None ->
      ()

let loop () =
  debug "%s started" __MODULE__ ;
  try
    while true do
      let now = Mtime_clock.count elapsed in
      let deadline, item =
        with_lock lock @@ fun () ->
        (* empty: wait till we get something *)
        if Ipq.is_empty queue then
          (Mtime.Span.(add now (10 * s)), None)
        else
          let next = Ipq.maximum queue in
          if Mtime.Span.is_longer next.Ipq.time ~than:now then
            (* not expired: wait till time or interrupted *)
            (next.Ipq.time, None)
          else (
            (* remove expired item *)
            Ipq.pop_maximum queue |> ignore ;
            (* save periodic to be scheduled again *)
            if next.Ipq.ev.ty <> OneShot then pending_event := Some next.Ipq.ev ;
            (now, Some next.Ipq.ev)
          )
      in
      match item with
      | Some todo ->
          (try todo.func () with _ -> ()) ;
          add_periodic_pending ()
      | None -> (
          (* Sleep until next event. *)
          let sleep = Mtime.Span.abs_diff deadline now |> Mtime.Span.(add ms) in
          try ignore (Delay.wait delay sleep)
          with e ->
            let detailed_msg =
              match e with
              | Unix.Unix_error (code, _, _) ->
                  Unix.error_message code
              | _ ->
                  "unknown error"
            in
            error
              "Could not schedule interruptable delay (%s). Falling back to \
               normal delay. New events may be missed."
              detailed_msg ;
            Thread.delay (Clock.Timer.span_to_s sleep)
        )
    done
  with _ ->
    error
      "Scheduler thread died! This daemon will no longer function well and \
       should be restarted."
