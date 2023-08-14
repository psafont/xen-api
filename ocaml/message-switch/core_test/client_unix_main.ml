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

open Message_switch_unix.Protocol_unix

let path = ref "/var/run/message-switch/sock"

let name = ref "server"

let payload = ref "hello"

let timeout = ref None

(* Sent to the server to initiate a shutdown *)
let shutdown = "shutdown"

let ( >>|= ) m f =
  match m with
  | Ok x ->
      f x
  | Error y ->
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      Client.pp_error fmt y ;
      Format.pp_print_flush fmt () ;
      raise (Failure (Buffer.contents b))

let is_shorter ~than s = Mtime.Span.compare s than < 0

let main () =
  Client.connect ~switch:!path () >>|= fun c ->
  let counter = ref 0 in
  let one () =
    incr counter ;
    Client.rpc ~t:c ~queue:!name ~body:!payload () >>|= fun _ -> ()
  in
  let start = Mtime_clock.counter () in
  ( match !timeout with
  | None ->
      one ()
  | Some t ->
      while is_shorter ~than:t (Mtime_clock.count start) do
        one ()
      done
  ) ;
  let time = Mtime_clock.count start in
  Printf.printf "Finished %d RPCs in %s\n" !counter
    (Fmt.to_to_string Mtime.Span.pp time) ;
  Client.rpc ~t:c ~queue:!name ~body:shutdown () >>|= fun _ -> ()

let _ =
  Arg.parse
    [
      ( "-path"
      , Arg.Set_string path
      , Printf.sprintf "path switch listens on (default %s)" !path
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
    ]
    (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s" x)
    "Send a message to a name, optionally waiting for a response" ;
  main ()
