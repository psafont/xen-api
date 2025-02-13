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
module Char = struct module Map = Map.Make (Char) end

module String = struct
  let sub_to_end s start =
    let length = String.length s in
    String.sub s start (length - start)

  let replaced ~replaceable ~get_replacement string =
    if String.exists replaceable string then (
      let replaced = Buffer.create (String.length string + 10) in
      String.iter
        (fun c ->
          match get_replacement c with
          | Some str ->
              Buffer.add_string replaced str
          | None ->
              Buffer.add_char replaced c
        )
        string ;
      Buffer.contents replaced
    ) else
      string

  let escaped ~rules string =
    let replaceable ch = Char.Map.mem ch rules in
    let get_replacement ch = Char.Map.find_opt ch rules in
    replaced ~replaceable ~get_replacement string

  let rec split ~limit sep s =
    match (String.index_opt s sep, limit < 2) with
    | None, _ | _, true ->
        [s]
    | Some pos, false ->
        let first = String.sub s 0 pos in
        let rest = sub_to_end s (pos + 1) in
        first :: split ~limit:(limit - 1) sep rest

  let rtrim s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\n' then
      String.sub s 0 (n - 1)
    else
      s

  (** find all occurences of needle in haystack and return all their respective index *)
  let find_all needle haystack =
    let m = String.length needle and n = String.length haystack in
    if m > n then
      []
    else
      let i = ref 0 and found = ref [] in
      while !i < n - m + 1 do
        if String.sub haystack !i m = needle then (
          found := !i :: !found ;
          i := !i + m
        ) else
          incr i
      done ;
      List.rev !found

  (* replace all @f substring in @s by @t *)
  let replace f t s =
    let indexes = find_all f s in
    let n = List.length indexes in
    if n > 0 then (
      let len_f = String.length f and len_t = String.length t in
      let new_len = String.length s + (n * len_t) - (n * len_f) in
      let new_b = Bytes.make new_len '\000' in
      let orig_offset = ref 0 and dest_offset = ref 0 in
      List.iter
        (fun h ->
          let len = h - !orig_offset in
          Bytes.blit_string s !orig_offset new_b !dest_offset len ;
          Bytes.blit_string t 0 new_b (!dest_offset + len) len_t ;
          orig_offset := !orig_offset + len + len_f ;
          dest_offset := !dest_offset + len + len_t
        )
        indexes ;
      Bytes.blit_string s !orig_offset new_b !dest_offset
        (String.length s - !orig_offset) ;
      Bytes.unsafe_to_string new_b
    ) else
      s

  let map_unlikely s f =
    let changed = ref false in
    let m = ref 0 in
    let buf = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      match f s.[i] with
      | None ->
          ()
      | Some n ->
          changed := true ;
          Buffer.add_substring buf s !m (i - !m) ;
          Buffer.add_string buf n ;
          m := i + 1
    done ;
    if !changed then (
      Buffer.add_substring buf s !m (String.length s - !m) ;
      Buffer.contents buf
    ) else
      s
end
