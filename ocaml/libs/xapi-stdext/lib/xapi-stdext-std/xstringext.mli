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
module Char : sig
  module Map : sig
    include Map.S with type key := Char.t
  end
end

module String : sig
  val replaced :
       replaceable:(char -> bool)
    -> get_replacement:(char -> string option)
    -> string
    -> string
  (** [replaced ~replaceable ~get_replacement str] applies [get_replacement] to
      [str] to replace the characted with a string when it returns [Some st] or
      blablabla *)

  val replace : char -> by:string -> string -> string
  (** [replace ch ~by s] replaces all the occurrences of [ch] in [s] by [~by]
    *)

  val split : limit:int -> char -> string -> string list
  (** split a string on a single char *)

  val rtrim : string -> string
  (** FIXME document me|remove me if similar to strip *)

  val map_unlikely : string -> (char -> string option) -> string
  (** map a string trying to fill the buffer by chunk *)

  val sub_to_end : string -> int -> string
  (** a substring from the specified position to the end of the string *)
end
