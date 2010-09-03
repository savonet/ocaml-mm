(** Operation on video data. *)

type frame = Image.RGBA8.t

(** A video buffer. *)
type buffer = frame array

(** Size of the buffer in frames. *)
val size : buffer -> int

val create : int -> buffer

val append : buffer -> buffer -> buffer

val iter_all : buffer -> (frame -> unit) -> unit

val map_all : buffer -> (frame -> frame) -> unit

module Ringbuffer_ext : Ringbuffer.R with type elt = frame

module Ringbuffer : Ringbuffer.R with type elt = frame

module FPS : sig
  type t = float

  val to_frac : t -> int * int
end

module IO : sig
  exception Invalid_file

  class type reader =
  object
    method width : int

    method height : int

    (** Number of frames per second. *)
    method frame_rate : float

    method set_target_size : int -> int -> unit

    (** Read a given number of frames. *)
    method read : buffer -> int -> int -> int

    method close : unit
  end

  class type writer =
  object
    method write : buffer -> int -> int -> unit

    method close : unit
  end

  val writer_to_avi_file : string -> FPS.t -> int -> int -> writer
end
