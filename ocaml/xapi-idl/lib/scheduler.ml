(*
 * Copyright (C) Citrix Systems Inc.
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

module D = Debug.Make (struct let name = "scheduler" end)

open D

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

module Delay = Xapi_stdext_threads.Threadext.Delay

type handle = Mtime.span * int

type handle_compat = int64 * int [@@deriving rpc]

let rpc_of_handle (s, id) = rpc_of_handle_compat (Mtime.Span.to_uint64_ns s, id)

let handle_of_rpc rpc =
  let i64, id = handle_compat_of_rpc rpc in
  (Mtime.Span.of_uint64_ns i64, id)

module HandleMap = Map.Make (struct
  type t = handle

  let compare (x1, id1) (x2, id2) =
    let c = Mtime.Span.compare x1 x2 in
    if c = 0 then
      id2 - id1
    else
      c
end)

type item = {name: string; fn: unit -> unit}

type t = {
    mutable schedule: item HandleMap.t
  ; delay: Delay.t
  ; mutable next_id: int
  ; m: Mutex.t
}

let span_to_s span =
  Mtime.Span.to_uint64_ns span |> Int64.to_float |> fun ns -> ns /. 1e9

let time_of_span span = span_to_s span |> ceil |> Int64.of_float

let mtime_sub time now = Mtime.Span.abs_diff time now |> time_of_span

let elapsed = Mtime_clock.counter ()

module Dump = struct
  type u = {time: int64; thing: string} [@@deriving rpc]

  type dump = u list [@@deriving rpc]

  let make s =
    let now = Mtime_clock.count elapsed in
    with_lock s.m (fun () ->
        HandleMap.fold
          (fun (time, _) i acc ->
            {time= mtime_sub time now; thing= i.name} :: acc
          )
          s.schedule []
    )
end

let one_shot s dt name f =
  let time = Mtime.Span.add dt (Mtime_clock.count elapsed) in
  with_lock s.m (fun () ->
      let id = s.next_id in
      s.next_id <- s.next_id + 1 ;
      let item = {name; fn= f} in
      let handle = (time, id) in
      s.schedule <- HandleMap.add handle item s.schedule ;
      Delay.signal s.delay ;
      handle
  )

let cancel s handle =
  with_lock s.m (fun () -> s.schedule <- HandleMap.remove handle s.schedule)

let process_expired s =
  let t = Mtime_clock.count elapsed in
  let expired =
    with_lock s.m (fun () ->
        let expired, eq, unexpired = HandleMap.split (t, max_int) s.schedule in
        assert (eq = None) ;
        s.schedule <- unexpired ;
        expired |> HandleMap.to_seq |> Seq.map snd
    )
  in
  (* This might take a while *)
  Seq.iter
    (fun i ->
      try i.fn ()
      with e ->
        debug "Scheduler ignoring exception: %s\n%!" (Printexc.to_string e)
    )
    expired ;
  expired () <> Seq.Nil

(* true if work was done *)

let rec main_loop s =
  while process_expired s do
    ()
  done ;
  let sleep_until =
    with_lock s.m (fun () ->
        try HandleMap.min_binding s.schedule |> fst |> fst
        with Not_found ->
          let now = Mtime_clock.count elapsed in
          Mtime.Span.(add now (1 * hour))
    )
  in
  let now = Mtime_clock.count elapsed in
  let period =
    if Mtime.Span.compare sleep_until now > 0 then
      (* be careful that this is absolute difference,
         it is never negative! *)
      Mtime.Span.abs_diff sleep_until now
    else
      Mtime.Span.zero
  in
  let (_ : bool) = Delay.wait s.delay period in
  main_loop s

let make_scheduler () =
  let s =
    {
      schedule= HandleMap.empty
    ; delay= Delay.make ()
    ; next_id= 0
    ; m= Mutex.create ()
    }
  in
  let (_ : Thread.t) = Thread.create main_loop s in
  s

let make = make_scheduler
