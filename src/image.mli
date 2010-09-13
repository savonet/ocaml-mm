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

  val internal : t -> (data * int) * (data * data * int)

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

  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** An image. *)
  type t

  val width : t -> int

  val height : t -> int

  val data : t -> data

  val stride : t -> int

  val create : int -> int -> t

  (* Does not copy the data. Use [copy] for this. *)
  val make : ?stride:int -> int -> int -> data -> t

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

  val randomize_all : t -> unit

  val add : ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  module Scale : sig
    type kind = Linear | Bilinear

    val onto : ?kind:kind -> ?proportional:bool -> t -> t -> unit

    val create : ?kind:kind -> ?proportional:bool -> t -> int -> int -> t
  end

  module Effect : sig
    val translate : t -> int -> int -> unit

    val affine : t -> float -> float -> int -> int -> unit

    val greyscale : t -> unit

    val sepia : t -> unit

    val lomo : t -> unit

    val invert : t -> unit

    val rotate : t -> float -> unit

    val mask : t -> t -> unit

    (** Effects on alpha channel. *)
    module Alpha : sig
      val blur : t -> unit

      (** Scale alpha channel with a given coefficient. *)
      val scale : t -> float -> unit

      val disk : t -> int -> int -> int -> unit
    end
  end
end
