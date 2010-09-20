(** Operations on images. Mostly only the RGBA8 format is supported for now. *)

(** Operations on images stored in RGB8 format, ie RGB channels, one byte each. *)
module RGB8 : sig
  (** Operations on colors. *)
  module Color : sig
    (** An RGB8 color (values of components should be between 0 and 255). *)
    type t = int * int * int

    (** Decode a color stored as RGB. *)
    val of_int : int -> t
  end
end

(** Operations on images stored in YUV420 format, ie one luma (Y) and two chrominance (U and V) channels. *)
module YUV420 : sig
  (** Data of a channel. *)
  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** An image in YUV420 format. *)
  type t

  (** Width of an image. *)
  val width : t -> int

  (** Height of an image. *)
  val height : t -> int

  (** Create an image of given width and height. *)
  val create : int -> int -> t

  (** Clear an image (sets it to black). *)
  val blank_all : t -> unit

  val make : int -> int -> data -> int -> data -> data -> int -> t
  val internal : t -> (data * int) * (data * data * int)
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

  val dimensions : t -> int * int

  val data : t -> data

  val stride : t -> int

  val create : int -> int -> t

  (* Does not copy the data. Use [copy] for this. *)
  val make : ?stride:int -> int -> int -> data -> t

  val get_pixel : t -> int -> int -> Color.t

  val set_pixel : t -> int -> int -> Color.t -> unit

  val copy : t -> t

  val blit : ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  (** [blit_all src dst] copies all the contents of [src] into [dst]. *)
  val blit_all : t -> t -> unit

  (** {2 Conversions from/to other formats} *)

  val of_RGB8_string : string -> int -> t

  val to_RGB8_string : t -> string

  val of_YUV420 : YUV420.t -> t

  val to_int_image : t -> int array array

  val to_BMP : t -> string

  val of_PPM : ?alpha:RGB8.Color.t -> string -> t

  (** {2 Manipulation of images} *)

  val add : ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  val fill_all : t -> Color.t -> unit

  val blank_all : t -> unit

  val randomize_all : t -> unit

  module Scale : sig
    type kind = Linear | Bilinear

    val onto : ?kind:kind -> ?proportional:bool -> t -> t -> unit

    val create : ?kind:kind -> ?copy:bool -> ?proportional:bool -> t -> int -> int -> t
  end

  module Effect : sig
    (** Translate image. *)
    val translate : t -> int -> int -> unit

    (** Apply an affine transformation to an image. *)
    val affine : t -> float -> float -> int -> int -> unit

    (** Convert to greyscale. *)
    val greyscale : t -> unit

    (** Convert to sepia colors. *)
    val sepia : t -> unit

    (** Lomo effect on colors (see http://en.wikipedia.org/wiki/Lomo_effect ). *)
    val lomo : t -> unit

    (** Invert colors. *)
    val invert : t -> unit

    (** Rotate image by a given angle (in radians). *)
    val rotate : t -> float -> unit

    val mask : t -> t -> unit

    (** Effects on alpha channel. *)
    module Alpha : sig
      val blur : t -> unit

      (** Scale alpha channel with a given coefficient. *)
      val scale : t -> float -> unit

      val disk : t -> int -> int -> int -> unit

      val of_color : t -> RGB8.Color.t -> int -> unit
    end
  end
end

module Generic : sig
  module Pixel : sig
    type rgb_format =
      | RGB24       (** 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
      | BGR24       (** 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
      | RGB32       (** 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
      | BGR32       (** 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
      | RGBA32      (** 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)
    type yuv_format =
      | YUV422    (** Planar YCbCr 4:2:2. Each component is an uint8_t *)
      | YUV444    (** Planar YCbCr 4:4:4. Each component is an uint8_t *)
      | YUV411    (** Planar YCbCr 4:1:1. Each component is an uint8_t *)
      | YUV410    (** Planar YCbCr 4:1:0. Each component is an uint8_t *)
      | YUVJ420   (** Planar YCbCr 4:2:0. Each component is an uint8_t, luma
                      and chroma values are full range (0x00 .. 0xff) *)
      | YUVJ422   (** Planar YCbCr 4:2:2. Each component is an uint8_t, luma and
                      chroma values are full range (0x00 .. 0xff) *)
      | YUVJ444   (** Planar YCbCr 4:4:4. Each component is an uint8_t, luma and
                      chroma values are full range (0x00 .. 0xff) *)

    type format =
      | RGB of rgb_format
      | YUV of yuv_format

    val string_of_format : format -> string
  end

  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t

  val width : t -> int

  val height : t -> int

  val pixel_format : t -> Pixel.format

  val rgb_data : t -> data * int

  val yuv_data : t -> (data * int) * (data * data * int)

  val of_RGBA8 : RGBA8.t -> t

  val of_YUV420 : YUV420.t -> t

  val convert : ?copy:bool -> ?proportional:bool -> ?scale_kind:RGBA8.Scale.kind -> t -> t -> unit
end
