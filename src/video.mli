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

open Mm_image

(** Images of videos. *)
module Image : sig
  type t = Image.YUV420.t

  val create : int -> int -> t
  val of_RGB24_string : string -> int -> t

  (** Convert to format useable by [Graphics.make_image]. *)
  val to_int_image : t -> int array array

  val copy : t -> t
  val width : t -> int
  val height : t -> int
  val dimensions : t -> int * int

  (** Size in bytes. *)
  val size : t -> int

  val blank : t -> unit
  val fill_alpha : t -> int -> unit
  val scale : ?proportional:bool -> t -> t -> unit
  val randomize : t -> unit

  (** [blit_all src dst] blits an entire image. *)
  val blit : t -> t -> unit

  val get_pixel_rgba : t -> int -> int -> int * int * int * int
  val set_pixel_rgba : t -> int -> int -> int * int * int * int -> unit

  (** Add the fist image to the second. *)
  val add : t -> ?x:int -> ?y:int -> t -> unit

  module Effect : sig
    val greyscale : t -> unit
    val sepia : t -> unit
    val invert : t -> unit
    val lomo : t -> unit

    module Alpha : sig
      val scale : t -> float -> unit
      val disk : t -> int -> int -> int -> unit
    end
  end
end

(** A video buffer. *)
type t = Image.t array

type buffer = t

(** Create a buffer with a given number of frames of given size. *)
val make : int -> int -> int -> t

(** Video with a single image. *)
val single : Image.t -> t

val blit : t -> int -> t -> int -> int -> unit

(** Create a fresh copy of a buffer. *)
val copy : t -> t

(** Length in images. *)
val length : t -> int

(** Size in bytes. *)
val size : t -> int

(** Obtaine the i-th image of a video. *)
val get : t -> int -> Image.t

val set : t -> int -> Image.t -> unit
val iter : (Image.t -> unit) -> t -> int -> int -> unit
val blank : t -> int -> int -> unit
val randomize : t -> int -> int -> unit

(* module Ringbuffer_ext : Ringbuffer.R with type elt = frame *)

(* module Ringbuffer : Ringbuffer.R with type elt = frame *)

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
