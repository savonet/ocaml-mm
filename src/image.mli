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

(** Operations on images. *)

module Data : sig
  type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** external alloc : int -> t = "caml_data_alloc" *)
  val alloc : int -> t

  val of_string : string -> t
  val to_string : t -> string
  val to_bytes : t -> bytes
  val length : t -> int
  val blit : t -> int -> t -> int -> int -> unit
  val blit_all : t -> t -> unit
  val copy : t -> t
  val round : int -> int -> int
end

module Pixel : sig
  type rgba = int * int * int * int
  type rgb = int * int * int
  type yuv = int * int * int
  type yuva = yuv * int

  val yuv_of_rgb : rgb -> yuv
  val rgb_of_yuv : yuv -> rgb
end

module Draw : sig
  val line : (int -> int -> unit) -> int * int -> int * int -> unit
end

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

module BGRA : sig
  type t

  val data : t -> Data.t
end

(** Operations on images stored in RGBA32 format (ie RGB channels + an alpha
    channel, one byte for each). *)
module RGBA32 : sig
  module Color : sig
    type t = int * int * int * int
  end

  type data =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** An image. *)
  type t

  val width : t -> int
  val height : t -> int
  val dimensions : t -> int * int
  val data : t -> data
  val size : t -> int
  val stride : t -> int
  val create : int -> int -> t

  (* Does not copy the data. Use [copy] for this. *)
  val make : ?stride:int -> int -> int -> data -> t
  val get_pixel : t -> int -> int -> Color.t
  val set_pixel : t -> int -> int -> Color.t -> unit
  val get_pixel_rgba : t -> int -> int -> Pixel.rgba
  val set_pixel_rgba : t -> int -> int -> Pixel.rgba -> unit
  val copy : t -> t

  val blit :
    ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  (** [blit_all src dst] copies all the contents of [src] into [dst]. *)
  val blit_all : t -> t -> unit

  (** {2 Conversions from/to other formats} *)

  val of_RGB24_string : string -> int -> t
  val to_RGB24_string : t -> string
  val of_BGRA : BGRA.t -> t
  val to_BGRA : t -> BGRA.t
  val to_int_image : t -> int array array
  val to_BMP : t -> string
  val of_PPM : ?alpha:RGB8.Color.t -> string -> t

  (** Swap red and blue channels. Useful for quickly handling BGRA formats. *)
  val swap_rb : t -> unit

  (** {2 Manipulation of images} *)

  val add : ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit
  val fill_all : t -> Color.t -> unit
  val blank_all : t -> unit
  val fill_alpha : t -> int -> unit
  val blank : t -> unit
  val randomize_all : t -> unit
  val randomize : t -> unit

  (** [scale src dst] scales the image [src] to [dst]. *)
  val scale : ?proportional:bool -> t -> t -> unit

  module Scale : sig
    type kind = Linear | Bilinear

    val onto : ?kind:kind -> ?proportional:bool -> t -> t -> unit

    val create :
      ?kind:kind -> ?copy:bool -> ?proportional:bool -> t -> int -> int -> t
  end

  module Effect : sig
    (** Translate image. *)
    val translate : t -> int -> int -> unit

    (** Apply an affine transformation to an image. *)
    val affine : t -> float -> float -> int -> int -> unit

    (** Flip (mirror across horizontal axis). *)
    val flip : t -> unit

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
    val box_blur : t -> unit

    (** Effects on alpha channel. *)
    module Alpha : sig
      val blur : t -> unit

      (** Scale alpha channel with a given coefficient. *)
      val scale : t -> float -> unit

      val disk : t -> int -> int -> int -> unit
      val of_color : t -> RGB8.Color.t -> int -> unit
    end
  end

  module Draw : sig
    val line : t -> Color.t -> int * int -> int * int -> unit
  end

  module Motion : sig
    val compute : int -> t -> t -> int * int

    module Multi : sig
      type vectors

      val compute : int -> t -> t -> vectors
      val median_denoise : vectors -> unit
      val mean : vectors -> int * int
      val arrows : vectors -> t -> unit
    end
  end
end

(** Operations on images stored in YUV420 format, ie one luma (Y) and two chrominance (U and V) channels. *)
module YUV420 : sig
  (** An image in YUV420 format. *)
  type t

  val make : int -> int -> Data.t -> int -> Data.t -> Data.t -> int -> t
  val make_data : int -> int -> Data.t -> int -> int -> t
  val create : ?y_stride:int -> ?uv_stride:int -> int -> int -> t

  (** Ensure that the image has an alpha channel. *)
  val ensure_alpha : t -> unit

  val remove_alpha : t -> unit

  val of_YUV420_string :
    ?y_stride:int -> ?uv_stride:int -> string -> int -> int -> t

  val of_RGB24_string : string -> int -> t
  val of_RGBA32 : RGBA32.t -> t
  val to_RGBA32 : t -> RGBA32.t
  val of_PPM : string -> t

  (** Width of an image. *)
  val width : t -> int

  (** Height of an image. *)
  val height : t -> int

  val y : t -> Data.t
  val y_stride : t -> int
  val u : t -> Data.t
  val v : t -> Data.t
  val uv_stride : t -> int
  val data : t -> Data.t * Data.t * Data.t
  val alpha : t -> Data.t option
  val set_alpha : t -> Data.t option -> unit
  val dimensions : t -> int * int

  (** Size in bytes. *)
  val size : t -> int

  (** Whether the image has an alpha channel. *)
  val has_alpha : t -> bool

  (* (\** Obtaine data with given stride. No copy is made when possible. *\) *)
  (* val data_stride : t -> int -> int -> Data.t * Data.t * Data.t *)

  val copy : t -> t
  val blit_all : t -> t -> unit
  val blit : t -> t -> unit
  val scale : ?proportional:bool -> t -> t -> unit
  val blank_all : t -> unit

  (** Add the fist image to the second. *)
  val add : t -> ?x:int -> ?y:int -> t -> unit

  val blank : t -> unit
  val fill : t -> Pixel.yuv -> unit
  val fill_alpha : t -> int -> unit
  val disk_alpha : t -> int -> int -> int -> unit

  (* [box_alpha img x y width height alpha] Set alpha value
     on a given image box. *)
  val box_alpha : t -> int -> int -> int -> int -> float -> unit
  val randomize : t -> unit
  val get_pixel_y : t -> int -> int -> int
  val get_pixel_u : t -> int -> int -> int
  val get_pixel_v : t -> int -> int -> int
  val get_pixel_rgba : t -> int -> int -> Pixel.rgba
  val set_pixel_rgba : t -> int -> int -> Pixel.rgba -> unit

  (** Convert to format useable by [Graphics.make_image]. *)
  val to_int_image : t -> int array array

  module Effect : sig
    val greyscale : t -> unit
    val sepia : t -> unit
    val invert : t -> unit
    val lomo : t -> unit

    (** Effects on alpha channel. *)
    module Alpha : sig
      (** Scale alpha channel with a given coefficient. *)
      val scale : t -> float -> unit

      val disk : t -> int -> int -> int -> unit
    end
  end
end

(** Operations on images in generic formats (many formats are supported). *)
module Generic : sig
  (** Since the module is very generic, many of the functions are not
      implemented for particular formats. This exception is raised when it is
      the case. *)
  exception Not_implemented

  (** Generic pixels. *)
  module Pixel : sig
    (** Format of an RGB pixel. *)
    type rgb_format =
      | RGB24
          (** 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
      | BGR24
          (** 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
      | RGB32
          (** 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
      | BGR32
          (** 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
      | RGBA32
          (** 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)

    (** Format of a YUV pixel. *)
    type yuv_format =
      | YUV422  (** Planar YCbCr 4:2:2. Each component is an uint8_t *)
      | YUV444  (** Planar YCbCr 4:4:4. Each component is an uint8_t *)
      | YUV411  (** Planar YCbCr 4:1:1. Each component is an uint8_t *)
      | YUV410  (** Planar YCbCr 4:1:0. Each component is an uint8_t *)
      | YUVJ420
          (** Planar YCbCr 4:2:0. Each component is an uint8_t, luma
                      and chroma values are full range (0x00 .. 0xff) *)
      | YUVJ422
          (** Planar YCbCr 4:2:2. Each component is an uint8_t, luma and
                      chroma values are full range (0x00 .. 0xff) *)
      | YUVJ444
          (** Planar YCbCr 4:4:4. Each component is an uint8_t, luma and
                      chroma values are full range (0x00 .. 0xff) *)

    (** Format of a pixel. *)
    type format = RGB of rgb_format | YUV of yuv_format

    (** String representation of the format of a pixel. *)
    val string_of_format : format -> string
  end

  (** Data contents of an image. *)
  type data =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** An image. *)
  type t

  (** Width of an image. *)
  val width : t -> int

  (** Height of an image. *)
  val height : t -> int

  (** Pixel format of an image. *)
  val pixel_format : t -> Pixel.format

  (** Create a new image of RGB format. *)
  val make_rgb : Pixel.rgb_format -> ?stride:int -> int -> int -> data -> t

  (** Data and stride of an RGB image. *)
  val rgb_data : t -> data * int

  (** Data of a YUV image. *)
  val yuv_data : t -> (data * int) * (data * data * int)

  (** Create a generic image from an RGBA32 image. *)
  val of_RGBA32 : RGBA32.t -> t

  val to_RGBA32 : t -> RGBA32.t

  (** Create a generic image from a YUV420 image. *)
  val of_YUV420 : YUV420.t -> t

  val to_YUV420 : t -> YUV420.t
  val blank : t -> unit

  (** Convert a generic image from a format to another. *)
  val convert :
    ?proportional:bool -> ?scale_kind:RGBA32.Scale.kind -> t -> t -> unit
end
