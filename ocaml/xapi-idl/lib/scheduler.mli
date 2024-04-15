(** The type of a scheduler *)
type t

(** The handle for referring to an item that has been scheduled *)
type handle

val rpc_of_handle : handle -> Rpc.t

val handle_of_rpc : Rpc.t -> handle

val make : unit -> t
(** Creates a scheduler *)

(** This module is for dumping the state of a scheduler *)
module Dump : sig
  type u = {time: int64; thing: string}

  type dump = u list

  val rpc_of_dump : dump -> Rpc.t

  val dump_of_rpc : Rpc.t -> dump

  val make : t -> dump
end

val one_shot : t -> Mtime.Span.t -> string -> (unit -> unit) -> handle
(** Insert a one-shot item into the scheduler. *)

val cancel : t -> handle -> unit
(** Cancel an item *)

module Delay : sig
  type t

  val make : unit -> t
  (** Makes a PipeDelay.t *)

  val wait : t -> Mtime.Span.t -> bool
  (** Wait for the specified amount of seconds. Returns true if we waited the full
      length of time, false if we were woken. Uses float to be able to be used
      along Thread.wait *)

  val signal : t -> unit
  (** Signal anyone currently waiting with the PipeDelay.t *)
end
