(*
 * Copyright (C) 2006-2024 Citrix Systems Inc.
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

module Delay = Xapi_stdext_threads.Threadext.Delay

let span_between ~at_least ~at_most actual =
  Mtime.Span.is_longer actual ~than:at_least
  && Mtime.Span.is_shorter actual ~than:at_most

let delay_test delay wait_for ~at_least ~at_most ~times_out =
  let c = Mtime_clock.counter () in
  let timed_out = Delay.wait delay wait_for in
  let waited = Mtime_clock.count c in

  let msg =
    Fmt.str "Value %a must be between %a and %a" Mtime.Span.pp waited
      Mtime.Span.pp at_least Mtime.Span.pp at_most
  in
  let actual = span_between ~at_least ~at_most waited in
  Alcotest.(check' bool) ~msg ~expected:true ~actual ;

  let msg =
    if timed_out then
      "Must have been signaled"
    else
      "Must have timed\n  out"
  in
  Alcotest.(check' bool) ~msg ~expected:times_out ~actual:timed_out

(*
Single simple signal stored
- signal
- wait on same thread should succeed quickly
*)
let signal () =
  let d = Delay.make () in
  let wait_for = Mtime.Span.(1 * s) in
  let at_least = Mtime.Span.zero in
  let at_most = Mtime.Span.(5 * ms) in
  Delay.signal d ;
  delay_test d wait_for ~at_least ~at_most ~times_out:false

(*
No signal
- wait on same thread should timeout more or less on delay
*)
let no_signal () =
  let d = Delay.make () in
  let wait_for = Mtime.Span.(200 * ms) in
  let at_least = wait_for in
  let at_most = Mtime.Span.(250 * ms) in
  delay_test d wait_for ~at_least ~at_most ~times_out:true

(*
Signal twice, collapsed
- signal
- signal
- wait on same thread should succeed quickly
- wait on same thread should timeout
*)
let collapsed () =
  let d = Delay.make () in
  Delay.signal d ;
  Delay.signal d ;

  let wait_for = Mtime.Span.(200 * ms) in

  let at_least = Mtime.Span.zero in
  let at_most = Mtime.Span.(50 * ms) in
  delay_test d wait_for ~at_least ~at_most ~times_out:false ;
  let at_least = Mtime.Span.(200 * ms) in
  let at_most = Mtime.Span.(250 * ms) in
  delay_test d wait_for ~at_least ~at_most ~times_out:true

(*
Signal from another thread
- signal on another thread after a while
- wait on same thread should succeed more or less on other thread sleep
*)
let other_thread () =
  let d = Delay.make () in
  let wait_for = Mtime.Span.(1 * s) in
  let at_least = Mtime.Span.(200 * ms) in
  let at_most = Mtime.Span.(250 * ms) in
  let th = Thread.create (fun d -> Thread.delay 0.2 ; Delay.signal d) d in
  delay_test d wait_for ~at_least ~at_most ~times_out:false ;
  Thread.join th

let tests =
  [
    ("simple", `Quick, signal)
  ; ("no_signal", `Quick, no_signal)
  ; ("collapsed", `Quick, collapsed)
  ; ("other_thread", `Quick, other_thread)
  ]

let () = Alcotest.run "Threadext" [("Delay", tests)]
