(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let finally' f g =
  try
    let result = f () in
    g () ; result
  with e -> g () ; raise e

module Mutex = struct
  include Mutex

  let execute m f =
    lock m ;
    finally' f (fun () -> unlock m)
end

module WaitMap = Map.Make (Mtime.Span)

module Delay = Xapi_stdext_threads.Threadext.Delay

type item = {id: int; name: string; fn: unit -> unit}

let schedule = ref WaitMap.empty

let delay = Delay.make ()

let next_id = ref 0

let m = Mutex.create ()

type time = Delta of Mtime.Span.t

type t = Mtime.Span.t * int

let elapsed = Mtime_clock.counter ()

let nano = 1_000_000_000L

let mtime_sub time now =
  let floor span =
    Int64.div (Mtime.Span.to_uint64_ns span) nano |> Int64.mul nano
  in
  Mtime.Span.(abs_diff time now |> floor |> of_uint64_ns)

(*module Dump = struct
    type u = {time: Mtime.Span.t; thing: string}

    type t = u list

    let make () =
      let now = Mtime_clock.count elapsed in
      Mutex.execute m @@ fun () ->
      WaitMap.fold
        (fun time xs acc ->
          List.map
            (fun i ->
              {time= mtime_sub time now; thing= i.name}
            )
            xs
          @ acc
        )
        !schedule []
  end
*)
let one_shot (Delta dt) (name : string) f =
  let deadline = Mtime.Span.add dt (Mtime_clock.count elapsed) in
  let id =
    Mutex.execute m (fun () ->
        let existing =
          WaitMap.find_opt deadline !schedule |> Option.value ~default:[]
        in
        let id = !next_id in
        incr next_id ;
        let item = {id; name; fn= f} in
        schedule := WaitMap.add deadline (item :: existing) !schedule ;
        Delay.signal delay ;
        id
    )
  in
  (deadline, id)

let cancel (time, id) =
  Mutex.execute m (fun () ->
      let existing =
        WaitMap.find_opt time !schedule |> Option.value ~default:[]
      in
      schedule :=
        WaitMap.add time (List.filter (fun i -> i.id <> id) existing) !schedule
  )

let process_expired () =
  let t = Mtime_clock.count elapsed in
  let expired =
    Mutex.execute m (fun () ->
        let expired, eq, unexpired = WaitMap.split t !schedule in
        schedule :=
          Option.fold ~none:unexpired
            ~some:(fun eq -> WaitMap.add t eq unexpired)
            eq ;
        WaitMap.to_seq expired |> Seq.map snd
    )
  in
  (* This might take a while *)
  Seq.iter
    (fun li -> List.rev li |> List.iter (fun i -> try i.fn () with _e -> ()))
    expired ;
  expired () <> Seq.Nil

(* true if work was done *)

let rec main_loop () =
  while process_expired () do
    ()
  done ;
  let sleep_until =
    Mutex.execute m (fun () ->
        try WaitMap.min_binding !schedule |> fst
        with Not_found ->
          Mtime.Span.(add (Mtime_clock.count elapsed) (1 * hour))
    )
  in
  let seconds =
    Mtime.Span.abs_diff sleep_until (Mtime_clock.count elapsed)
    |> Mtime.Span.to_uint64_ns
    |> Int64.div 1_000_000_000L
    |> Int64.to_float
  in
  let (_ : bool) = Delay.wait delay seconds in
  main_loop ()

let start =
  let t = ref None in
  fun () ->
    match !t with
    | None ->
        t := Some (Thread.create main_loop ())
    | Some _ ->
        ()
