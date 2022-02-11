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

open ImageBase

module RGBA32 = ImageRGBA32

type t = {
  mutable y : Data.t;
  mutable y_stride : int;
  mutable u : Data.t;
  mutable v : Data.t;
  mutable uv_stride : int;
  width : int;
  height : int;
  mutable alpha : Data.t option; (* alpha stride is y_stride *)
}

let width img = img.width
let height img = img.height
let dimensions img = (width img, height img)
let y img = img.y
let y_stride img = img.y_stride
let u img = img.u
let v img = img.v
let uv_stride img = img.uv_stride
let data img = (img.y, img.u, img.v)
let alpha img = img.alpha
let set_alpha img alpha = img.alpha <- alpha
let size img = Data.size img.y + Data.size img.u + Data.size img.v

let ensure_alpha img =
  if img.alpha = None then (
    let a = Data.alloc (img.height * img.y_stride) in
    Data.fill a 0xff;
    img.alpha <- Some a)

external fill : t -> Pixel.yuv -> unit = "caml_yuv420_fill"

let fill_alpha img a =
  if a = 0xff then img.alpha <- None
  else (
    ensure_alpha img;
    Bigarray.Array1.fill (Option.get img.alpha) a)

let blank img = fill img (Pixel.yuv_of_rgb (0, 0, 0))
let blank_all = blank

let make width height ?alpha y y_stride u v uv_stride =
  { y; y_stride; u; v; uv_stride; width; height; alpha }

let make_data width height data y_stride uv_stride =
  assert (Data.length data = height * (y_stride + uv_stride));
  let y = Data.sub data 0 (height * y_stride) in
  let u = Data.sub data (height * y_stride) (height / 2 * uv_stride) in
  let v =
    Data.sub data
      ((height * y_stride) + (height / 2 * uv_stride))
      (height / 2 * uv_stride)
  in
  make width height y y_stride u v uv_stride

(* Default alignment. *)
let align = Sys.word_size / 8

let default_stride width y_stride uv_stride =
  let y_stride = Option.value ~default:(Data.round align width) y_stride in
  let uv_stride =
    Option.value ~default:(Data.round align ((width + 1) / 2)) uv_stride
  in
  (y_stride, uv_stride)

let create ?(blank=false) ?y_stride ?uv_stride width height =
  let y_stride, uv_stride = default_stride width y_stride uv_stride in
  let y = Data.aligned align (height * y_stride) in
  let u, v =
    let height = Data.round 2 ((height + 1) / 2) in
    ( Data.aligned align (height * uv_stride),
      Data.aligned align (height * uv_stride) )
  in
  let img = make width height y y_stride u v uv_stride in
  if blank then blank_all img;
  img

let has_alpha img = img.alpha <> None
let remove_alpha img = img.alpha <- None

let of_YUV420_string ?y_stride ?uv_stride s width height =
  (* let y_stride, uv_stride = default_stride width y_stride uv_stride in *)
  let y_stride = Option.value ~default:width y_stride in
  let uv_stride = Option.value ~default:(width / 2) uv_stride in
  let data = Data.of_string s in
  make_data width height data y_stride uv_stride

external of_RGB24_string : t -> string -> unit = "caml_yuv420_of_rgb24_string"

let of_RGB24_string s width =
  let height = String.length s / (3 * width) in
  let img = create width height in
  of_RGB24_string img s;
  img

external of_RGBA32 : RGBA32.t -> t -> unit = "caml_yuv420_of_rgba32"

let of_RGBA32 rgb =
  let width = RGBA32.width rgb in
  let height = RGBA32.height rgb in
  let img = create width height in
  ensure_alpha img;
  of_RGBA32 rgb img;
  img

external to_RGBA32 : t -> RGBA32.t -> unit = "caml_yuv420_to_rgba32"

let to_RGBA32 img =
  let width = img.width in
  let height = img.height in
  let rgb = RGBA32.create width height in
  to_RGBA32 img rgb;
  rgb

let of_PPM s =
  let img = of_RGBA32 (RGBA32.of_PPM s) in
  remove_alpha img;
  img

let to_BMP img =
  to_RGBA32 img |> RGBA32.to_BMP

let copy img =
  let dst =
    create ~y_stride:img.y_stride ~uv_stride:img.uv_stride img.width
      img.height
  in
  Bigarray.Array1.blit img.y dst.y;
  Bigarray.Array1.blit img.u dst.u;
  Bigarray.Array1.blit img.v dst.v;
  let alpha =
    match img.alpha with None -> None | Some alpha -> Some (Data.copy alpha)
  in
  dst.alpha <- alpha;
  dst

let blit_all src dst =
  assert (src.width = dst.width);
  assert (src.height = dst.height);
  if src.y_stride = dst.y_stride && src.uv_stride = dst.uv_stride then (
    Data.blit src.y 0 dst.y 0 (dst.height * dst.y_stride);
    Data.blit src.u 0 dst.u 0 (dst.height / 2 * dst.uv_stride);
    Data.blit src.v 0 dst.v 0 (dst.height / 2 * dst.uv_stride);
    match src.alpha with
    | None -> dst.alpha <- None
    | Some alpha -> (
        match dst.alpha with
        | None -> dst.alpha <- Some (Data.copy alpha)
        | Some alpha' -> Bigarray.Array1.blit alpha alpha'))
  else (
    dst.y <- Data.copy src.y;
    dst.u <- Data.copy src.u;
    dst.v <- Data.copy src.v;
    dst.y_stride <- src.y_stride;
    dst.uv_stride <- src.uv_stride;
    match src.alpha with
    | None -> dst.alpha <- None
    | Some alpha -> dst.alpha <- Some (Data.copy alpha))

let blit src dst = blit_all src dst

external randomize : t -> unit = "caml_yuv_randomize"
external add : t -> int -> int -> t -> unit = "caml_yuv420_add"

let add src ?(x = 0) ?(y = 0) dst =
  add src x y dst

external set_pixel_rgba : t -> int -> int -> Pixel.rgba -> unit
  = "caml_yuv420_set_pixel_rgba"

(* [@@noalloc] *)
let set_pixel_rgba img i j ((_, _, _, a) as p) =
  assert (0 <= i && i < img.width && 0 <= j && j < img.height);
  if a <> 0xff then ensure_alpha img;
  set_pixel_rgba img i j p

  (*
  let set_pixel_rgba img i j (r,g,b,a) =
    let data = img.data in
    let width = img.width in
    let height = img.height in
    if img.alpha <> None || a <> 0xff then
      (
        ensure_alpha img;
        Bigarray.Array1.set (Option.get img.alpha) (j * width + i) a
      );
    let y,u,v = Pixel.yuv_of_rgb (r,g,b) in
    Bigarray.Array1.set data (j * width + i) y;
    Bigarray.Array1.set data (height * width + (j / 2) * (width / 2) + i / 2) u;
    Bigarray.Array1.set data (height * width * 5 / 4 + (j / 2) * (width / 2) + i / 2) v
   *)

let get_pixel_y img i j = Data.get img.y ((j * img.y_stride) + i)
let get_pixel_u img i j = Data.get img.u ((j / 2 * img.uv_stride) + (i / 2))
let get_pixel_v img i j = Data.get img.v ((j / 2 * img.uv_stride) + (i / 2))

external get_pixel_rgba : t -> int -> int -> Pixel.rgba
  = "caml_yuv420_get_pixel_rgba"

external to_int_image : t -> int array array = "caml_yuv420_to_int_image"
external scale_full : t -> t -> unit = "caml_yuv420_scale"

let scale_full src dst =
  if has_alpha src then ensure_alpha dst;
  scale_full src dst

(** [scale_coef src dst (xn,xd) (yn,yd)] scales [src] into [dst] multiplying x
    dimension by xn/xd and y dimension by yn/yd. *)
external scale_coef : t -> t -> int * int -> int * int -> unit
  = "caml_yuv420_scale_coef"

let scale_proportional src dst =
  if has_alpha src then ensure_alpha dst;
  let sw, sh = (src.width, src.height) in
  let dw, dh = (dst.width, dst.height) in
  if dw = sw && dh = sh then blit_all src dst
  else (
    let n, d = if dh * sw < sh * dw then (dh, sh) else (dw, sw) in
    scale_coef src dst (n, d) (n, d))

let scale ?(proportional = false) src dst =
  if proportional then scale_proportional src dst else scale_full src dst

external rotate : t -> int -> int -> float -> t -> unit = "caml_yuv_rotate"

let rotate src x y a dst =
  ensure_alpha dst;
  rotate src x y a dst

let alpha_to_y img =
  ensure_alpha img;
  img.y <- Option.get img.alpha;
  img.alpha <- None

external scale_alpha : t -> float -> unit = "caml_yuv_scale_alpha"

let scale_alpha img a =
  if a <> 1. then (
    ensure_alpha img;
    scale_alpha img a)

external disk_alpha : t -> int -> int -> int -> unit = "caml_yuv_disk_alpha"

let disk_alpha img x y r =
  ensure_alpha img;
  disk_alpha img x y r

external box_alpha : t -> int -> int -> int -> int -> float -> unit
  = "caml_yuv_box_alpha_bytecode" "caml_yuv_box_alpha_native"

let box_alpha img x y r =
  ensure_alpha img;
  box_alpha img x y r

external alpha_of_color : t -> int -> int -> int -> int -> unit = "caml_yuv_alpha_of_color"

let alpha_of_color img (y,u,v) d =
  ensure_alpha img;
  alpha_of_color img y u v d

external gradient_uv : t -> int * int -> int * int -> int * int -> unit = "caml_yuv_gradient_uv"

module Effect = struct
  external greyscale : t -> unit = "caml_yuv_greyscale"

  external invert : t -> unit = "caml_yuv_invert"

  external sepia : t -> unit = "caml_yuv_sepia"

  external lomo : t -> unit = "caml_yuv_lomo"

  module Alpha = struct
    let scale = scale_alpha
    let disk = disk_alpha
  end
end
