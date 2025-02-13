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
(**
 * @group Command-Line Interface (CLI)
*)

module Xstringext = Xapi_stdext_std.Xstringext
open Cli_protocol

type record = (string * string) list

type printval =
  | PMsg of string
  | PTable of record list
  | PList of string list
  | PStderr of string

type print_fn = printval -> unit

let pad_string s len =
  let n = len - String.length s in
  String.make (if n > 0 then n else 0) ' ' ^ s

let pad_rhs s len =
  let n = len - String.length s in
  s ^ String.make (if n > 0 then n else 0) ' '

let rec multi_line_record r =
  let maxlen =
    4 + List.fold_left max 0 (List.map (fun (a, _) -> String.length a) r)
  in
  let indent fs = List.map (fun (f, v) -> (pad_string f maxlen, v)) fs in
  let r =
    match r with (k, v) :: fs -> (pad_rhs k maxlen, v) :: indent fs | _ -> r
  in
  String.concat "\n" (List.map (fun (f, v) -> f ^ ": " ^ v) r) ^ "\n"

(* Used to escape commas in --minimal mode *)
let escape_commas =
  let rules = [(',', "\\,"); ('\\', "\\\\")] in
  Xstringext.Char.Map.of_seq (List.to_seq rules)

let escaped_commas x = Xstringext.String.escaped ~rules:escape_commas x

let make_printer sock minimal =
  let buffer = ref [] in
  let multi_line_xapi_minimal pval =
    match pval with
    | PTable rs ->
        (* Check that all the sublists aren't empty before calling List.hd *)
        if rs <> [] && List.for_all (fun r -> r <> []) rs then
          let names = List.map (fun r -> snd (List.hd r)) rs in
          let escaped_names = List.map escaped_commas names in
          buffer := String.concat "," escaped_names :: !buffer
    | PList ss ->
        let escaped_ss = List.map escaped_commas ss in
        buffer := String.concat "," escaped_ss :: !buffer
    | _ ->
        ()
  in
  let multi_line_xapi pval =
    match pval with
    | PTable rs ->
        List.iter
          (fun l -> marshal sock (Command (Print (l ^ "\n"))))
          (List.map multi_line_record rs)
    | PList ss ->
        List.iter (fun l -> marshal sock (Command (Print l))) ss
    | PMsg ss ->
        marshal sock (Command (Print ss))
    | PStderr ss ->
        marshal sock (Command (PrintStderr (ss ^ "\n")))
  in
  let minimal_flush () =
    marshal sock (Command (Print (String.concat "," !buffer)))
  in
  let flush () = () in
  if minimal then
    (multi_line_xapi_minimal, minimal_flush)
  else
    (multi_line_xapi, flush)
