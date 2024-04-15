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

open Lwt
open Message_switch_lwt.Protocol_lwt

let path = ref "/var/run/message-switch/sock"

let name = ref "server"

let payload = ref "hello"

let timeout = ref None

let shutdown = "shutdown"

let nthreads = ref 1

let ( >>|= ) m f =
  m >>= function
  | Ok x ->
      f x
  | Error y ->
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      Client.pp_error fmt y ;
      Format.pp_print_flush fmt () ;
      fail (Failure (Buffer.contents b))

let main () =
  Client.connect ~switch:!path () >>|= fun t ->
  let counter = ref 0 in
  let one () =
    incr counter ;
    Client.rpc ~t ~queue:!name ~body:!payload () >>|= fun _ -> return ()
  in
  let start = Mtime_clock.counter () in
  let rec ints = function 0 -> [] | n -> n :: ints (n - 1) in
  ( match !timeout with
  | None ->
      one ()
  | Some timeout ->
      let rec thread n =
        let elapsed = Mtime_clock.count start in
        if Mtime.Span.is_shorter ~than:timeout elapsed then
          one () >>= fun () -> thread n
        else
          return ()
      in
      let threads = List.map thread (ints !nthreads) in
      Lwt.join threads
  )
  >>= fun () ->
  let time = Mtime_clock.count start in
  Printf.printf "Finished %d RPCs in %s\n" !counter
    (Fmt.to_to_string Mtime.Span.pp time) ;
  Client.rpc ~t ~queue:!name ~body:shutdown () >>|= fun _ -> return ()

let _ =
  Arg.parse
    [
      ( "-path"
      , Arg.Set_string path
      , Printf.sprintf "path broker listens on (default %s)" !path
      )
    ; ( "-name"
      , Arg.Set_string name
      , Printf.sprintf "name to send message to (default %s)" !name
      )
    ; ( "-payload"
      , Arg.Set_string payload
      , Printf.sprintf "payload of message to send (default %s)" !payload
      )
    ; ( "-secs"
      , Arg.String
          (fun x ->
            let t = 1000. *. float_of_string x |> Float.to_int in
            timeout := Some Mtime.Span.(t * ms)
          )
      , "number of seconds to repeat the same message for"
      )
    ; ( "-threads"
      , Arg.Set_int nthreads
      , Printf.sprintf "number of parallel threads (default %d)" !nthreads
      )
    ]
    (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
    "Send a message to a name, optionally waiting for a response" ;
  Lwt_main.run (main ())
