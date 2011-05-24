(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

(** Operations on video data. *)

(** A frame. *)
type frame = Image.RGBA32.t

(** A video buffer. *)
type buffer = frame array

(** Size of the buffer in frames. *)
val size : buffer -> int

(** Create a buffer with a given number of frames. The frames themselves should
    not be read or written to, otherwise use [make]. *)
val create : int -> buffer

(** Create a buffer with a given number of frames of given size. *)
val make : int -> int -> int -> buffer

(** Create a fresh copy of a buffer. *)
val copy : buffer -> buffer

(** Concatenate two buffers. *)
val append : buffer -> buffer -> buffer

val blit : buffer -> int -> buffer -> int -> int -> unit

val iter_all : buffer -> (frame -> unit) -> unit

val map_all : buffer -> (frame -> frame) -> unit

val randomize : buffer -> int -> int -> unit

val blank : buffer -> int -> int -> unit

module Ringbuffer_ext : Ringbuffer.R with type elt = frame

module Ringbuffer : Ringbuffer.R with type elt = frame

(** Operations on frame rates. *)
module FPS : sig
  type t = float

  (** Convert a frame rate to a fraction. *)
  val to_frac : t -> int * int
end

module IO : sig
  exception Invalid_file

  module Reader : sig
    class type t =
    object
      method width : int

      method height : int

    (** Number of frames per second. *)
      method frame_rate : FPS.t

    (* method set_target_size : int -> int -> unit *)

    (** Read a given number of frames. *)
      method read : buffer -> int -> int -> int

      method close : unit
    end
  end

  module Writer : sig
    class type t =
    object
      method write : buffer -> int -> int -> unit

      method close : unit
    end

    class to_avi_file : string -> FPS.t -> int -> int -> t
  end
end
