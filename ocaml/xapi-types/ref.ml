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

type without_secret = Uuidx.without_secret

type not_secret =
  [ without_secret
  | `session of [`use_make_secret_or_ref_of_secret_string_instead] ]

type secret = Uuidx.secret

type all = Uuidx.all

type 'a t =
  | Real of string
  (* ref to an object in the database *)
  | Dummy of string * string
  (* ref to an object that is not in the database, with its name *)
  | Other of string
  (* ref used for other purposes (it doesn't have one of the official prefixes) *)
  | Null
  constraint 'a = [< all]

(* ref to nothing at all *)

let ref_prefix = "OpaqueRef:"

let ref_prefix_length = String.length ref_prefix

let dummy_prefix = "DummyRef:"

let dummy_sep = "|"

let ref_null = ref_prefix ^ "NULL"

let make () =
  let uuid = Uuidx.(to_string (make ())) in
  Real uuid

let make_secret () =
  let uuid = Uuidx.(to_string (make_uuid_urnd ())) in
  Real uuid

let null = Null

(* a dummy reference is a reference of an object which is not in database *)
let make_dummy name =
  let uuid = Uuidx.(to_string (make ())) in
  Dummy (uuid, name)

let is_real = function Real _ -> true | _ -> false

let is_dummy = function Dummy _ -> true | _ -> false

let compare (a : 'a t) (b : 'a t) =
  match (a, b) with
  | Real a, Real b ->
      String.compare a b
  | Dummy (a1, a2), Dummy (b1, b2) ->
      let c = String.compare a1 b1 in
      if c = 0 then String.compare a2 b2 else c
  | Other a, Other b ->
      String.compare a b
  | Null, Null ->
      0
  | Null, _ ->
      -1 (* Null < Other < Dummy < Real *)
  | _, Null ->
      1
  | Other _, _ ->
      -1 (* Other < Dummy < Real *)
  | _, Other _ ->
      1
  | Dummy _, _ ->
      -1 (* Dummy < Real *)
  | Real _, Dummy _ ->
      1

let equal x y = Int.equal (compare x y) 0

let string_of = function
  | Real uuid ->
      ref_prefix ^ uuid
  | Dummy (uuid, name) ->
      dummy_prefix ^ dummy_sep ^ uuid ^ dummy_sep ^ name
  | Other x ->
      x
  | Null ->
      ref_null

let short_string_of = function
  | Real x | Dummy (x, _) | Other x ->
      Astring.String.with_range ~len:8 x
  | Null ->
      "NULL"

let of_string x =
  if x = ref_null then
    Null
  else if String.starts_with ~prefix:ref_prefix x then
    Real String.(sub x ref_prefix_length (length x - ref_prefix_length))
  else if String.starts_with ~prefix:dummy_prefix x then
    match Astring.String.cuts ~sep:dummy_sep x with
    | _prefix :: uuid :: name ->
        Dummy (uuid, String.concat dummy_sep name)
    | _ ->
        Other x
  else
    Other x

let of_secret_string = of_string

let to_option = function Null -> None | ref -> Some ref

let name_of_dummy = function
  | Real x | Other x ->
      failwith
        (Printf.sprintf "Ref.name_of_dummy: %s is not a dummy reference" x)
  | Null ->
      failwith "Ref.name_of_dummy: NULL is not a dummy reference"
  | Dummy (_, name) ->
      name

(* we do not show the name when we pretty print the dummy reference *)
let really_pretty_and_small x =
  let small_uuid s =
    try
      let r = Bytes.create 12 in
      for i = 0 to 7 do
        Bytes.set r i s.[i]
      done ;
      for i = 0 to 3 do
        Bytes.set r (i + 8) s.[8 + 1 + i]
      done ;
      Bytes.unsafe_to_string r
    with _ -> s
  in
  match x with
  | Dummy (uuid, _) ->
      "D:" ^ small_uuid uuid
  | Real uuid ->
      "R:" ^ small_uuid uuid
  | Other x ->
      "O:" ^ Astring.String.with_range ~len:12 x
  | Null ->
      "NULL"

let pp ppf x = Format.fprintf ppf "%s" (string_of x)

let rpc_of_t _ x = Rpc.rpc_of_string (string_of x)

let t_of_rpc _ x = of_string (Rpc.string_of_rpc x)
