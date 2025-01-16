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

module M = Mutex

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

module Mutex = struct
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock ;
    finally f (fun () -> Mutex.unlock lock)
end

module Semaphore = struct
  let execute s f =
    let module Semaphore = Semaphore.Counting in
    Semaphore.acquire s ;
    finally f (fun () -> Semaphore.release s)
end

(** Parallel List.iter. Remembers all exceptions and returns an association list mapping input x to an exception.
    Applications of x which succeed will be missing from the returned list. *)
let thread_iter_all_exns f xs =
  let exns = ref [] in
  let m = M.create () in
  List.iter Thread.join
    (List.map
       (fun x ->
         Thread.create
           (fun () ->
             try f x
             with e -> Mutex.execute m (fun () -> exns := (x, e) :: !exns)
           )
           ()
       )
       xs
    ) ;
  !exns

(** Parallel List.iter. Remembers one exception (at random) and throws it in the
    error case. *)
let thread_iter f xs =
  match thread_iter_all_exns f xs with [] -> () | (_, e) :: _ -> raise e

module Delay = struct
  type t

  external make : unit -> t = "caml_xapi_delay_create"

  external signal : t -> unit = "caml_xapi_delay_signal"

  external wait : t -> int64 -> bool = "caml_xapi_delay_wait"

  let wait d t =
    if t = Mtime.Span.zero then
      true
    else
      (* Against Mtime docs, we use the absolute value of the monotonic clock,
         because that's what pthread_cond_timedwait has been set up to use *)
      let now = Mtime_clock.now () in
      let deadline =
        Mtime.add_span now t |> Option.value ~default:Mtime.max_stamp
      in
      wait d (Mtime.to_uint64_ns deadline)
end

let wait_timed_read fd timeout =
  match Xapi_stdext_unix.Unixext.select [fd] [] [] timeout with
  | [], _, _ ->
      false
  | [fd'], _, _ ->
      assert (fd' = fd) ;
      true
  | _ ->
      assert false

let wait_timed_write fd timeout =
  match Xapi_stdext_unix.Unixext.select [] [fd] [] timeout with
  | _, [], _ ->
      false
  | _, [fd'], _ ->
      assert (fd' = fd) ;
      true
  | _ ->
      assert false
