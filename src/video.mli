(** Operation on video data. *)

type frame = Image.RGBA8.t

(** A video buffer. *)
type buffer = frame array

(** Size of the buffer in frames. *)
val size : buffer -> int

val create : int -> buffer

val append : buffer -> buffer -> buffer

module Ringbuffer_ext : Ringbuffer.R with type elt = frame

module Ringbuffer : Ringbuffer.R with type elt = frame

module IO : sig
  exception Invalid_file

  class type reader =
  object
    method width : int

    method height : int

    (** Number of frames per second. *)
    method frame_rate : float

    (** Read a given number of frames. *)
    method read : buffer -> int -> int -> int

    method close : unit
  end
end
