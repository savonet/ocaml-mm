(** Operations on images. Mostly only the RGBA8 format is supported for now. *)

module RGB8 : sig
  module Color : sig
    type t = int * int * int

    val of_int : int -> t
  end
end

module YUV420 : sig
  type t

  val create : int -> int -> t

  val blank_all : t -> unit
end

(** Operations on images stored in RGBA8 format (ie RGB channels + an alpha
    channel, one byte for each). *)
module RGBA8 : sig
  module Color : sig
    type t = int * int * int * int
  end

  (** An image. *)
  type t

  val create : int -> int -> t

  val copy : t -> t

  val blit : ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  val blit_all : t -> t -> unit

  val fill_all : t -> Color.t -> unit

  val blank_all : t -> unit

  val of_linear_rgb : string -> int -> t

  module Effect : sig
    val greyscale : t -> unit

    val sepia : t -> unit

    val lomo : t -> unit

    val invert : t -> unit

    val rotate : t -> float -> unit

    val mask : t -> t -> unit

    val blur_alpha : t -> unit
  end
end
