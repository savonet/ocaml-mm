(** Operations on video data. *)

(** A frame. *)
type frame = Image.RGBA8.t

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
