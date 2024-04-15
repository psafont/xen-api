let span eps =
  let same x y = Mtime.Span.(compare (abs_diff x y) eps <= 0) in
  Alcotest.testable Mtime.Span.pp same

(* allowed fluctuation when mesuring time *)
let eps = Mtime.Span.(20 * ms)

let half_second = Mtime.Span.(500 * ms)

let one_second = Mtime.Span.(1 * s)

let one_minute = Mtime.Span.(1 * min)

(* Tests the basic delay functionality. *)
let test_delay () =
  let x = Scheduler.Delay.make () in
  let expected = half_second in
  let timer = Mtime_clock.counter () in
  let full_wait = Scheduler.Delay.wait x expected in
  let elapsed = Mtime_clock.count timer in
  Alcotest.(check bool) "Wait was not interrupted" full_wait true ;
  Alcotest.(check @@ span eps)
    "Didn't wait for as long as expected" elapsed expected

(* Tests that 'wait' can be cancelled *)
let test_delay_cancel () =
  let open Scheduler.Delay in
  let x = make () in
  let full_wait = ref false in
  let waiter () = full_wait := wait x half_second in
  let expected = eps in
  let timer = Mtime_clock.counter () in
  let th = Thread.create waiter () in
  signal x ;
  Thread.join th ;
  let elapsed = Mtime_clock.count timer in
  Alcotest.(check bool) "Wait was interrupted" !full_wait false ;
  Alcotest.(check @@ span eps)
    "Didn't wait for as long as expected" elapsed expected

let timed_wait_callback ~msg ?(not_before = Mtime.Span.zero)
    ?(not_after = one_minute) f =
  let rd, wr = Unix.pipe () in
  let finally () = Unix.close rd ; Unix.close wr in
  Fun.protect ~finally (fun () ->
      let timer = Mtime_clock.counter () in
      let elapsed = ref None in
      let callback () =
        elapsed := Some Mtime_clock.(count timer) ;
        let (_ : int) = Unix.write_substring wr " " 0 1 in
        ()
      in
      f callback ;
      let for_seconds = Clock.Timer.span_to_s not_after in
      let ready =
        Xapi_stdext_threads.Threadext.wait_timed_read rd for_seconds
      in
      match (ready, !elapsed) with
      | true, None ->
          Alcotest.fail "pipe ready to read, but elapsed is not set"
      | false, None ->
          Alcotest.fail
            (Fmt.str "%s: callback not invoked within %a" msg Mtime.Span.pp
               not_after
            )
      | _, Some t ->
          let after = Mtime.Span.compare not_before t <= 0 in
          Alcotest.(check @@ bool)
            (Printf.sprintf "%s: callback invoked earlier than expected" msg)
            after true
  )

(* Test the injection of a one-shot function at a time in the future *)
let test_one_shot () =
  timed_wait_callback ~msg:"one_shot_success" ~not_before:one_second
    (fun callback ->
      ignore Scheduler.(one_shot (make ()) one_second "test_one_shot" callback)
  )

(* Tests that the scheduler still works even after a failure occurs in the
   injected function *)
let test_one_shot_failure () =
  let x = Scheduler.make () in
  timed_wait_callback ~msg:"one_show_failure" ~not_before:one_second
    (fun callback ->
      let _ =
        Scheduler.one_shot x Mtime.Span.zero "test_one_shot" (fun () ->
            failwith "Error"
        )
      in
      ignore @@ Scheduler.one_shot x one_second "test_one_shot" callback
  )

(* Checks that one-shot functions can cancelled and are then not executed *)
let test_one_shot_cancel () =
  let sch = Scheduler.make () in
  let changed = ref false in
  let x =
    Scheduler.one_shot sch one_second "test_one_shot_cancel" (fun () ->
        changed := true
    )
  in
  Scheduler.cancel sch x ;
  Thread.delay 2.0 ;
  Alcotest.(check @@ bool) "one_shot_cancelled" !changed false

(* Check that dumping the state of the scheduler contains a reference to a test
   function that has been injected *)
let test_dump () =
  let sch = Scheduler.make () in
  let thing = "test_dump" in
  let _ = Scheduler.one_shot sch one_second thing (fun () -> ()) in
  let dump = Scheduler.Dump.make sch in
  Alcotest.(check @@ bool)
    "Dump contains scheduled task"
    (List.exists (fun x -> x.Scheduler.Dump.thing = thing) dump)
    true

let tests =
  [
    ("Test Delay", `Slow, test_delay)
  ; ("Test Delay cancellation", `Quick, test_delay_cancel)
  ; ("Test One shot", `Slow, test_one_shot)
  ; ("Test One shot failure", `Slow, test_one_shot_failure)
  ; ("Test One shot cancellation", `Slow, test_one_shot_cancel)
  ; ("Test dump", `Quick, test_dump)
  ]
