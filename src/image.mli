(** Operations on images. Mostly only the RGBA8 format is supported for now. *)

module RGB8 : sig
  module Color : sig
    type t = int * int * int

    val of_int : int -> t
  end
end

module YUV420 : sig
  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t

  val width : t -> int

  val height : t -> int

  val create : int -> int -> t

  val make : data -> int -> data -> data -> int -> t

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

  val width : t -> int

  val height : t -> int

  val create : int -> int -> t

  val get_pixel : t -> int -> int -> Color.t

  val set_pixel : t -> int -> int -> Color.t -> unit

  val copy : t -> t

  val blit : ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  val blit_all : t -> t -> unit

  val fill_all : t -> Color.t -> unit

  val blank_all : t -> unit

  val of_RGB8_string : string -> int -> t

  val to_bmp : t -> string

  val to_RGB8_string : t -> string

  val of_YUV420 : YUV420.t -> t

  val to_int_image : t -> int array array

  val scale_to : t -> t -> unit

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
