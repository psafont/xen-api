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
open Sexplib.Std
module Protocol = Message_switch_core.Protocol

module MtimeSpanMap = struct
  include Map.Make (Mtime.Span)

  type 'a t' = (Protocol.Mtime_ext.Span.t * 'a) list [@@deriving sexp]

  let t_of_sexp a sexp =
    let t' = t'_of_sexp a sexp in
    List.fold_left (fun acc (x, y) -> add x y acc) empty t'

  let sexp_of_t a t = sexp_of_t' a (bindings t)
end

module Lwt_condition = struct
  include Lwt_condition

  type t' = string [@@deriving sexp]

  let _t_of_sexp _ _ = Lwt_condition.create ()

  let _sexp_of_t _ _ = sexp_of_t' "Lwt_condition.t"
end

module Lwt_mutex = struct
  include Lwt_mutex

  type t' = string [@@deriving sexp]

  let _t_of_sexp _ = Lwt_mutex.create ()

  let _sexp_of_t _ = sexp_of_t' "Lwt_mutex.t"
end

type waiter = {mutable next_id: int64} [@@deriving sexp]

type t = {
    q: Protocol.Entry.t MtimeSpanMap.t
  ; name: string
  ; length: int
  ; owner: string option
  ; (* if transient, name of the owning connection *)
    waiter: waiter
}
[@@deriving sexp]

let t_of_sexp sexp =
  let t = t_of_sexp sexp in
  (* compute a valid next_id *)
  let highest_id =
    try fst (MtimeSpanMap.max_binding t.q) |> Mtime.Span.to_uint64_ns
    with Not_found -> -1L
  in
  t.waiter.next_id <- Int64.succ highest_id ;
  t

let get_owner t = t.owner

let make owner name =
  let waiter = {next_id= 0L} in
  {q= MtimeSpanMap.empty; name; length= 0; owner; waiter}

module StringMap = struct
  include Map.Make (String)

  type 'a t' = (string * 'a) list [@@deriving sexp]

  let t_of_sexp a sexp =
    let t' = t'_of_sexp a sexp in
    List.fold_left (fun acc (x, y) -> add x y acc) empty t'

  let sexp_of_t a t = sexp_of_t' a (bindings t)
end

module StringSet = struct
  include Set.Make (String)

  type t' = string list [@@deriving sexp]

  let t_of_sexp sexp =
    let t' = t'_of_sexp sexp in
    List.fold_left (fun acc x -> add x acc) empty t'

  let sexp_of_t t = sexp_of_t' (elements t)
end

type queues = {queues: t StringMap.t; by_owner: StringSet.t StringMap.t}
[@@deriving sexp]

let empty = {queues= StringMap.empty; by_owner= StringMap.empty}

let owned_queues queues owner =
  StringMap.find_opt owner queues.by_owner
  |> Option.value ~default:StringSet.empty

module Lengths = struct
  open Message_switch_core.Measurable

  let d x = Description.{description= "length of queue " ^ x; units= ""}

  let _list_available queues =
    StringMap.fold (fun name _ acc -> (name, d name) :: acc) queues.queues []

  let _measure queues name =
    StringMap.find_opt name queues.queues
    |> Option.map (fun q -> Measurement.Int q.length)
end

module Internal = struct
  module Directory = struct
    let exists queues name = StringMap.mem name queues.queues

    let find_opt queues name = StringMap.find_opt name queues.queues

    let find queues name =
      match find_opt queues name with
      | Some queue ->
          queue
      | None ->
          make None name

    let add queues ?owner name =
      if not (exists queues name) then
        let queues' = StringMap.add name (make owner name) queues.queues in
        let by_owner =
          match owner with
          | None ->
              queues.by_owner
          | Some owner ->
              let existing = owned_queues queues owner in
              StringMap.add owner (StringSet.add name existing) queues.by_owner
        in
        {queues= queues'; by_owner}
      else
        queues

    let remove queues name =
      let by_owner =
        if not (exists queues name) then
          queues.by_owner
        else
          let q = StringMap.find name queues.queues in
          match q.owner with
          | None ->
              queues.by_owner
          | Some owner ->
              let owned =
                StringMap.find owner queues.by_owner |> StringSet.remove name
              in
              if StringSet.is_empty owned then
                StringMap.remove owner queues.by_owner
              else
                StringMap.add owner owned queues.by_owner
      in
      let queues = StringMap.remove name queues.queues in
      {queues; by_owner}

    let list queues prefix =
      StringMap.fold
        (fun name _ acc ->
          if String.starts_with ~prefix name then
            name :: acc
          else
            acc
        )
        queues.queues []
  end

  let ack queues (name, id) =
    match Directory.find_opt queues name with
    | Some q when MtimeSpanMap.mem id q.q ->
        let q' = {q with length= q.length - 1; q= MtimeSpanMap.remove id q.q} in
        {queues with queues= StringMap.add name q' queues.queues}
    | _ ->
        queues

  let send queues origin name id data =
    (* If a queue doesn't exist then drop the message *)
    match Directory.find_opt queues name with
    | Some q ->
        let elapsed = Mtime.Span.of_uint64_ns (Clock.elapsed_ns ()) in
        let q' =
          {
            q with
            length= q.length + 1
          ; q=
              MtimeSpanMap.add id
                (Message_switch_core.Protocol.Entry.make elapsed origin data)
                q.q
          }
        in
        {queues with queues= StringMap.add name q' queues.queues}
    | None ->
        queues

  let get_next_id queues name =
    let q = Directory.find queues name in
    let id = q.waiter.next_id in
    q.waiter.next_id <- Int64.succ id ;
    Some (Mtime.Span.of_uint64_ns id)
end

(* operations which need to be persisted *)
module Op = struct
  type directory_operation = Add of string option * string | Remove of string
  [@@deriving sexp]

  type t =
    | Directory of directory_operation
    | Ack of Message_switch_core.Protocol.message_id
    | Send of
        Message_switch_core.Protocol.origin
        * string
        * Message_switch_core.Protocol.Mtime_ext.Span.t
        * Message_switch_core.Protocol.Message.t
  (* origin * queue * id * body *)
  [@@deriving sexp]

  let of_cstruct x =
    try Some (Cstruct.to_string x |> Sexplib.Sexp.of_string |> t_of_sexp)
    with _ -> None

  let to_cstruct t =
    let s = sexp_of_t t |> Sexplib.Sexp.to_string in
    let c = Cstruct.create (String.length s) in
    Cstruct.blit_from_string s 0 c 0 (Cstruct.length c) ;
    c
end

let do_op queues = function
  | Op.Directory (Op.Add (owner, name)) ->
      Internal.Directory.add queues ?owner name
  | Op.Directory (Op.Remove name) ->
      Internal.Directory.remove queues name
  | Op.Ack (k, v) ->
      Internal.ack queues (k, v)
  | Op.Send (origin, name, id, body) ->
      Internal.send queues origin name id body

let contents q =
  MtimeSpanMap.fold (fun i e acc -> ((q.name, i), e) :: acc) q.q []

module Directory = struct
  let add _queues ?owner name = Op.Directory (Op.Add (owner, name))

  let remove _queues name = Op.Directory (Op.Remove name)

  let find = Internal.Directory.find

  let list = Internal.Directory.list
end

let queue_of_id = fst

let ack _queues id = Op.Ack id

let transfer queues from names =
  List.concat_map
    (fun name ->
      let q = Internal.Directory.find queues name in
      let not_seen =
        match from with
        | None ->
            q.q
        | Some from ->
            let _, _, not_seen = MtimeSpanMap.split from q.q in
            not_seen
      in
      MtimeSpanMap.fold
        (fun id e acc ->
          ((name, id), e.Message_switch_core.Protocol.Entry.message) :: acc
        )
        not_seen []
    )
    names

let entry queues (name, id) =
  let q = Internal.Directory.find queues name in
  MtimeSpanMap.find_opt id q.q

let send queues origin name body =
  Internal.get_next_id queues name
  |> Option.map @@ fun id -> ((name, id), Op.Send (origin, name, id, body))
