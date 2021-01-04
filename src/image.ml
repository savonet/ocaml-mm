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

let option_value o ~default = match o with Some v -> v | None -> default
let option_get = function Some v -> v | None -> invalid_arg "option is None"

module Data = struct
  type t =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (* Creates an 16-bytes aligned plane. Returns (stride*plane). *)
  (* external create_rounded_plane : int -> int -> int * t = "caml_data_aligned_plane" *)

  let alloc n =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.C_layout n

  (** [round n k] rounds [n] to the nearest upper multiple of [k]. *)
  let round k n = (n + (k - 1)) / k * k

  external aligned : int -> int -> t = "caml_data_aligned"

  (* Creates an 16-bytes aligned plane. Returns (stride*plane). *)
  let rounded_plane width height =
    let align = 16 in
    let stride = round 16 width in
    let data = aligned align (height * stride) in
    (stride, data)

  external to_string : t -> string = "caml_data_to_string"
  external to_bytes : t -> bytes = "caml_data_to_string"
  external of_string : string -> t = "caml_data_of_string"

  let blit_all src dst = Bigarray.Array1.blit src dst

  external blit : t -> int -> t -> int -> int -> unit = "caml_data_blit_off"

  (* [@@noalloc] *)

  external copy : t -> t = "caml_data_copy"

  let sub buf ofs len = Bigarray.Array1.sub buf ofs len
  let length img = Bigarray.Array1.dim img
  let size img = length img
  let get = Bigarray.Array1.get
  let fill buf x = Bigarray.Array1.fill buf x
end

module Pixel = struct
  type rgba = int * int * int * int
  type rgb = int * int * int
  type yuv = int * int * int
  type yuva = (int * int * int) * int

  external yuv_of_rgb : rgb -> yuv = "caml_yuv_of_rgb"
  external rgb_of_yuv : yuv -> rgb = "caml_rgb_of_yuv"
end

module Draw = struct
  (* Besenham algorithm. *)
  let line p (sx, sy) (dx, dy) =
    let steep = abs (dy - sy) > abs (dx - sx) in
    let sx, sy, dx, dy = if steep then (sy, sx, dy, dx) else (sx, sy, dx, dy) in
    let sx, sy, dx, dy =
      if sx > dx then (dx, dy, sx, sy) else (sx, sy, dx, dy)
    in
    let deltax = dx - sx in
    let deltay = abs (dy - sy) in
    let error = ref (deltax / 2) in
    let ystep = if sy < dy then 1 else -1 in
    let j = ref sy in
    for i = sx to dx - 1 do
      if steep then p !j i else p i !j;
      error := !error - deltay;
      if !error < 0 then (
        j := !j + ystep;
        error := !error + deltax )
    done
end

module Motion_multi = struct
  type vectors_data =
    (int, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t

  type vectors = {
    vectors : vectors_data;
    vectors_width : int;
    block_size : int;
  }

  external median_denoise : int -> vectors_data -> unit
    = "caml_rgb_motion_multi_median_denoise"

  let median_denoise v = median_denoise v.vectors_width v.vectors

  external mean : int -> vectors_data -> int * int
    = "caml_rgb_motion_multi_mean"

  let mean v = mean v.vectors_width v.vectors
end

module RGB8 = struct
  module Color = struct
    type t = int * int * int

    let of_int n =
      if n > 0xffffff then raise (Invalid_argument "Not a color");
      ((n lsr 16) land 0xff, (n lsr 8) land 0xff, n land 0xff)
  end
end

module Gray8 = struct
  (* TODO: stride ? *)
  type t = { data : Data.t; width : int }

  let make w d = { data = d; width = w }

  (* Don't use create_rounded_plane here since there is not stride.. *)
  let create w h =
    make w
      (Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h))

  module Motion = struct
    external compute : int -> int -> Data.t -> Data.t -> int * int
      = "caml_mm_Gray8_motion_compute"

    let compute bs o n = compute bs n.width o.data n.data

    module Multi = struct
      include Motion_multi

      external compute : int -> int -> Data.t -> Data.t -> vectors_data
        = "caml_mm_Gray8_motion_multi_compute"

      let compute bs o n =
        {
          vectors = compute bs n.width o.data n.data;
          vectors_width = n.width / bs;
          block_size = bs;
        }
    end
  end
end

module BGRA = struct
  type data = Data.t
  type t = { data : data; width : int; height : int; stride : int }

  let make ?stride width height data =
    let stride = match stride with Some v -> v | None -> 4 * width in
    { data; width; height; stride }

  let create ?stride width height =
    let stride = match stride with Some v -> v | None -> 4 * width in
    let stride, data = Data.rounded_plane stride height in
    make ~stride width height data

  let data img = img.data
end

module RGBA32 = struct
  module Color = struct
    type t = int * int * int * int
  end

  type data =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = {
    (* Order matters for C callbacks! *)
    data : data;
    width : int;
    height : int;
    stride : int;
  }

  let width buf = buf.width
  let height buf = buf.height
  let dimensions buf = (buf.width, buf.height)
  let data buf = buf.data
  let size buf = Bigarray.Array1.dim buf.data
  let stride buf = buf.stride

  let make ?stride width height data =
    let stride = match stride with Some v -> v | None -> 4 * width in
    { data; width; height; stride }

  let create ?stride width height =
    let stride = match stride with Some v -> v | None -> 4 * width in
    let stride, data = Data.rounded_plane stride height in
    make ~stride width height data

  let copy f =
    let nf = create ~stride:f.stride f.width f.height in
    Bigarray.Array1.blit f.data nf.data;
    nf

  (* Remove the optional stride argument. *)
  let create width height = create width height

  external blit : t -> t -> unit = "caml_rgb_blit"
  external blit_off : t -> t -> int -> int -> bool -> unit = "caml_rgb_blit_off"

  external blit_off_scale : t -> t -> int * int -> int * int -> bool -> unit
    = "caml_rgb_blit_off_scale"

  let blit_all src dst =
    assert (
      src.width = dst.width && src.height = dst.height
      && src.stride = dst.stride );
    blit src dst

  let blit ?(blank = true) ?(x = 0) ?(y = 0) ?w ?h src dst =
    match (w, h) with
      | None, None -> blit_off src dst x y blank
      | Some w, Some h -> blit_off_scale src dst (x, y) (w, h) blank
      | _, _ -> assert false

  external fill_all : t -> Color.t -> unit = "caml_rgb_fill"
  external blank_all : t -> unit = "caml_rgb_blank"

  let blank = blank_all

  external fill_alpha : t -> int -> unit = "caml_rgb_fill_alpha"
  external of_RGB24_string : t -> string -> unit = "caml_rgb_of_rgb8_string"

  let of_RGB24_string data width =
    let height = String.length data / 3 / width in
    let ans = create width height in
    of_RGB24_string ans data;
    ans

  external of_BGRA : t -> BGRA.t -> unit = "caml_rgba_of_bgra"

  let of_BGRA bgra =
    let img = create bgra.BGRA.width bgra.BGRA.height in
    of_BGRA img bgra;
    img

  external to_BGRA : BGRA.t -> t -> unit = "caml_rgba_of_bgra"

  let to_BGRA img =
    let bgra = BGRA.create img.width img.height in
    to_BGRA bgra img;
    bgra

  external to_Gray8 : t -> Data.t -> unit = "caml_mm_RGBA8_to_Gray8"

  let to_Gray8 rgb gray = to_Gray8 rgb gray.Gray8.data

  let to_Gray8_create rgb =
    let gray = Gray8.create (width rgb) (height rgb) in
    to_Gray8 rgb gray;
    gray

  external get_pixel : t -> int -> int -> Color.t = "caml_rgb_get_pixel"
  external set_pixel : t -> int -> int -> Color.t -> unit = "caml_rgb_set_pixel"

  let set_pixel img i j =
    assert (0 <= i && i < img.width);
    assert (0 <= j && j < img.height);
    set_pixel img i j

  let get_pixel_rgba = get_pixel
  let set_pixel_rgba = set_pixel

  external randomize_all : t -> unit = "caml_rgb_randomize"

  let randomize = randomize_all

  module Scale = struct
    type kind = Linear | Bilinear

    external scale_coef : t -> t -> int * int -> int * int -> unit
      = "caml_rgb_scale"

    external bilinear_scale_coef : t -> t -> float -> float -> unit
      = "caml_rgb_bilinear_scale"

    let scale_coef_kind k src dst (dw, sw) (dh, sh) =
      match k with
        | Linear -> scale_coef src dst (dw, sw) (dh, sh)
        | Bilinear ->
            let x = float dw /. float sw in
            let y = float dh /. float sh in
            bilinear_scale_coef src dst x y

    let onto ?(kind = Linear) ?(proportional = false) src dst =
      let sw, sh = (src.width, src.height) in
      let dw, dh = (dst.width, dst.height) in
      if dw = sw && dh = sh then blit_all src dst
      else if not proportional then
        scale_coef_kind kind src dst (dw, sw) (dh, sh)
      else (
        let n, d = if dh * sw < sh * dw then (dh, sh) else (dw, sw) in
        scale_coef_kind kind src dst (n, d) (n, d) )

    let create ?kind ?(copy = true) ?proportional src w h =
      if (not copy) && width src = w && height src = h then src
      else (
        let dst = create w h in
        onto ?kind ?proportional src dst;
        dst )
  end

  let scale ?proportional src dst = Scale.onto ?proportional src dst

  external to_BMP : t -> string = "caml_rgb_to_bmp"
  external to_RGB24_string : t -> string = "caml_image_to_rgb24"

  exception Invalid_format of string

  let of_PPM ?alpha data =
    let w, h, d, o =
      try
        (* TODO: make it useable without bound checks *)
        assert (data.[0] = 'P');
        assert (data.[1] = '6');
        assert (data.[2] = '\n');
        let n = ref 3 in
        let read_int () =
          let ans = ref 0 in
          let ( !! ) = int_of_char in
          while !!'0' <= !!(data.[!n]) && !!(data.[!n]) <= !!'9' do
            ans := (!ans * 10) + !!(data.[!n]) - !!'0';
            incr n
          done;
          assert (data.[!n] = ' ' || data.[!n] = '\n');
          incr n;
          !ans
        in
        if data.[!n] = '#' then (
          incr n;
          while data.[!n] <> '\n' do
            incr n
          done;
          incr n );
        let w = read_int () in
        let h = read_int () in
        let d = read_int () in
        (w, h, d, !n)
      with _ -> raise (Invalid_format "Not a PPM file.")
    in
    let datalen = String.length data - o in
    if d <> 255 then
      raise
        (Invalid_format
           (Printf.sprintf "Files of color depth %d are not handled." d));
    if datalen < 3 * w * h then
      raise
        (Invalid_format
           (Printf.sprintf "Got %d bytes of data instead of expected %d."
              datalen
              (3 * w * h)));
    let ans = create w h in
    for j = 0 to h - 1 do
      for i = 0 to w - 1 do
        let r, g, b =
          ( int_of_char data.[o + (3 * ((j * w) + i)) + 0],
            int_of_char data.[o + (3 * ((j * w) + i)) + 1],
            int_of_char data.[o + (3 * ((j * w) + i)) + 2] )
        in
        let a =
          match alpha with
            | Some (ra, ga, ba) ->
                if r = ra && g = ga && b = ba then 0x00 else 0xff
            | None -> 0xff
        in
        set_pixel ans i j (r, g, b, a)
      done
    done;
    ans

  external to_int_image : t -> int array array = "caml_rgb_to_color_array"

  (*
  let to_int_image buf =
    let w = buf.width in
    let h = buf.height in
    Array.init
      h
      (fun j ->
        Array.init
          w
          (fun i ->
            let r,g,b,a = get_pixel buf i j in
            (r lsl 16) + (g lsl 8) + b
          )
      )
  *)

  external add : t -> t -> unit = "caml_rgb_add"

  let add_fast = add

  external add_off : t -> t -> int -> int -> unit = "caml_rgb_add_off"

  external add_off_scale : t -> t -> int * int -> int * int -> unit
    = "caml_rgb_add_off_scale"

  let add ?(x = 0) ?(y = 0) ?w ?h src dst =
    match (w, h) with
      | None, None ->
          if x = 0 && y = 0 && src.width = dst.width && src.height = dst.height
          then add_fast src dst
          else add_off src dst x y
      | Some w, Some h -> add_off_scale src dst (x, y) (w, h)
      | _, _ -> assert false

  external swap_rb : t -> unit = "caml_rgba_swap_rb"

  module Effect = struct
    external greyscale : t -> bool -> unit = "caml_rgb_greyscale"

    let sepia buf = greyscale buf true
    let greyscale buf = greyscale buf false

    external invert : t -> unit = "caml_rgb_invert"
    external rotate : t -> float -> unit = "caml_rgb_rotate"

    external affine : t -> float -> float -> int -> int -> unit
      = "caml_rgb_affine"

    (* TODO: faster implementation? *)
    let translate f x y = affine f 1. 1. x y

    external flip : t -> unit = "caml_rgb_flip"
    external mask : t -> t -> unit = "caml_rgb_mask"
    external lomo : t -> unit = "caml_rgb_lomo"
    external box_blur : t -> unit = "caml_mm_RGBA8_box_blur"

    module Alpha = struct
      external scale : t -> float -> unit = "caml_rgb_scale_opacity"
      external blur : t -> unit = "caml_rgb_blur_alpha"
      external disk : t -> int -> int -> int -> unit = "caml_rgb_disk_opacity"

      external of_color_simple : t -> int * int * int -> int -> unit
        = "caml_rgb_color_to_alpha_simple"

      (* TODO: this does not work yet. *)
      (* external of_color : t -> int * int * int -> float -> float -> unit = "caml_rgb_color_to_alpha" *)
      let of_color = of_color_simple
    end
  end

  module Draw = struct
    external line : t -> int * int * int * int -> int * int -> int * int -> unit
      = "caml_mm_RGBA8_draw_line"
  end

  module Motion = struct
    (* TODO: compute old only once? *)
    let compute bs o n =
      Gray8.Motion.compute bs (to_Gray8_create o) (to_Gray8_create n)

    module Multi = struct
      include Motion_multi

      let compute bs o n =
        Gray8.Motion.Multi.compute bs (to_Gray8_create o) (to_Gray8_create n)

      external arrows : int -> vectors_data -> t -> unit
        = "caml_rgb_motion_multi_arrows"

      let arrows v img = arrows v.block_size v.vectors img
    end
  end
end

module YUV420 = struct
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

  let make width height y y_stride u v uv_stride =
    { y; y_stride; u; v; uv_stride; width; height; alpha = None }

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

  let default_stride width y_stride uv_stride =
    let align = 4 in
    let y_stride = option_value ~default:(Data.round align width) y_stride in
    let uv_stride =
      option_value ~default:(Data.round align ((width + 1) / 2)) uv_stride
    in
    (y_stride, uv_stride)

  let create ?y_stride ?uv_stride width height =
    let align = 4 in
    let y_stride, uv_stride = default_stride width y_stride uv_stride in
    let y = Data.aligned align (height * y_stride) in
    let u, v =
      let height = Data.round 2 height in
      ( Data.aligned align (height / 2 * uv_stride),
        Data.aligned align (height / 2 * uv_stride) )
    in
    make width height y y_stride u v uv_stride

  let ensure_alpha img =
    if img.alpha = None then (
      let a = Data.alloc (img.height * img.y_stride) in
      Data.fill a 0xff;
      img.alpha <- Some a )

  let has_alpha img = img.alpha <> None
  let remove_alpha img = img.alpha <- None

  let of_YUV420_string ?y_stride ?uv_stride s width height =
    (* let y_stride, uv_stride = default_stride width y_stride uv_stride in *)
    let y_stride = option_value ~default:width y_stride in
    let uv_stride = option_value ~default:(width / 2) uv_stride in
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

  external fill : t -> Pixel.yuv -> unit = "caml_yuv420_fill"

  let fill_alpha img a =
    if a = 0xff then img.alpha <- None
    else (
      ensure_alpha img;
      Bigarray.Array1.fill (option_get img.alpha) a )

  let blank img = fill img (Pixel.yuv_of_rgb (0, 0, 0))
  let blank_all = blank

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
              | Some alpha' -> Bigarray.Array1.blit alpha alpha' ) )
    else (
      dst.y <- Data.copy src.y;
      dst.u <- Data.copy src.u;
      dst.v <- Data.copy src.v;
      dst.y_stride <- src.y_stride;
      dst.uv_stride <- src.uv_stride;
      match src.alpha with
        | None -> dst.alpha <- None
        | Some alpha -> dst.alpha <- Some (Data.copy alpha) )

  let blit src dst = blit_all src dst

  external randomize : t -> unit = "caml_yuv_randomize"
  external add : t -> int -> int -> t -> unit = "caml_yuv420_add"

  let add src ?(x = 0) ?(y = 0) dst = add src x y dst

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
        Bigarray.Array1.set (option_get img.alpha) (j * width + i) a
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

  external scale_coef : t -> t -> int * int -> int * int -> unit
    = "caml_yuv420_scale_coef"

  let scale_proportional src dst =
    if has_alpha src then ensure_alpha dst;
    let sw, sh = (src.width, src.height) in
    let dw, dh = (dst.width, dst.height) in
    if dw = sw && dh = sh then blit_all src dst
    else (
      let n, d = if dh * sw < sh * dw then (dh, sh) else (dw, sw) in
      scale_coef src dst (n, d) (n, d) )

  let scale ?(proportional = false) src dst =
    if proportional then scale_proportional src dst else scale_full src dst

  external scale_alpha : t -> float -> unit = "caml_yuv_scale_alpha"

  let scale_alpha img a =
    if a <> 1. then (
      ensure_alpha img;
      scale_alpha img a )

  external disk_alpha : t -> int -> int -> int -> unit = "caml_yuv_disk_alpha"

  let disk_alpha img x y r =
    ensure_alpha img;
    disk_alpha img x y r

  external box_alpha : t -> int -> int -> int -> int -> float -> unit
    = "caml_yuv_box_alpha_bytecode" "caml_yuv_box_alpha_native"

  let box_alpha img x y r =
    ensure_alpha img;
    box_alpha img x y r

  module Effect = struct
    external greyscale : t -> unit = "caml_yuv_greyscale"

    let sepia _ = failwith "Not implemented: sepia"
    let invert _ = failwith "Not implemented: invert"
    let lomo _ = failwith "Not implemented: lomo"

    module Alpha = struct
      let scale = scale_alpha
      let disk = disk_alpha
    end
  end
end

module Generic = struct
  exception Not_implemented

  module Pixel = struct
    type rgb_format =
      | RGB24 (* 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
      | BGR24 (* 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
      | RGB32 (* 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
      | BGR32 (* 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
      | RGBA32

    (* 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)

    type yuv_format =
      | YUV422 (* Planar YCbCr 4:2:2. Each component is an uint8_t *)
      | YUV444 (* Planar YCbCr 4:4:4. Each component is an uint8_t *)
      | YUV411 (* Planar YCbCr 4:1:1. Each component is an uint8_t *)
      | YUV410 (* Planar YCbCr 4:1:0. Each component is an uint8_t *)
      | YUVJ420 (* Planar YCbCr 4:2:0. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
      | YUVJ422 (* Planar YCbCr 4:2:2. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
      | YUVJ444

    (* Planar YCbCr 4:4:4. Each component is an uint8_t, luma and
     * chroma values are full range (0x00 .. 0xff) *)

    type format = RGB of rgb_format | YUV of yuv_format

    let size = function
      | RGB x -> (
          match x with RGB24 | BGR24 -> 3 | RGB32 | BGR32 | RGBA32 -> 4 )
      | YUV _ -> raise Not_implemented

    let string_of_format = function
      | RGB x -> (
          match x with
            | RGB24 -> "RGB24"
            | BGR24 -> "BGR24"
            | RGB32 -> "RGB32"
            | BGR32 -> "BGR32"
            | RGBA32 -> "RGBA32" )
      | YUV x -> (
          match x with
            | YUV422 -> "YUV422"
            | YUV444 -> "YUV444"
            | YUV411 -> "YUV411"
            | YUV410 -> "YUV410"
            | YUVJ420 -> "YUVJ420"
            | YUVJ422 -> "YUVJ422"
            | YUVJ444 -> "YUVJ444" )
  end

  type data =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type rgb = { rgb_pixel : Pixel.rgb_format; rgb_data : data; rgb_stride : int }

  type yuv = {
    yuv_pixel : Pixel.yuv_format;
    y : data;
    y_stride : int;
    u : data;
    v : data;
    uv_stride : int;
  }

  type t_data = RGB of rgb | YUV of yuv
  type t = { data : t_data; width : int; height : int }

  let rgb_data img =
    match img.data with
      | RGB rgb -> (rgb.rgb_data, rgb.rgb_stride)
      | _ -> assert false

  let yuv_data img =
    match img.data with
      | YUV yuv -> ((yuv.y, yuv.y_stride), (yuv.u, yuv.v, yuv.uv_stride))
      | _ -> assert false

  let width img = img.width
  let height img = img.height

  let pixel_format img =
    match img.data with
      | RGB rgb -> Pixel.RGB rgb.rgb_pixel
      | YUV yuv -> Pixel.YUV yuv.yuv_pixel

  let make_rgb pix ?stride width height data =
    let stride =
      match stride with
        | Some s -> s
        | None -> width * Pixel.size (Pixel.RGB pix)
    in
    let rgb_data = { rgb_pixel = pix; rgb_data = data; rgb_stride = stride } in
    { data = RGB rgb_data; width; height }

  let of_RGBA32 img =
    let rgb_data =
      {
        rgb_pixel = Pixel.RGBA32;
        rgb_data = img.RGBA32.data;
        rgb_stride = img.RGBA32.stride;
      }
    in
    {
      data = RGB rgb_data;
      width = img.RGBA32.width;
      height = img.RGBA32.height;
    }

  let to_RGBA32 img =
    let rgb_data = match img.data with RGB d -> d | _ -> assert false in
    assert (rgb_data.rgb_pixel = Pixel.RGBA32);
    {
      RGBA32.data = rgb_data.rgb_data;
      width = img.width;
      height = img.height;
      stride = rgb_data.rgb_stride;
    }

  let of_YUV420 img =
    let yuv_data =
      {
        yuv_pixel = Pixel.YUVJ420;
        y = img.YUV420.y;
        y_stride = img.YUV420.y_stride;
        u = img.YUV420.u;
        v = img.YUV420.v;
        uv_stride = img.YUV420.uv_stride;
      }
    in
    {
      data = YUV yuv_data;
      width = img.YUV420.width;
      height = img.YUV420.height;
    }

  let to_YUV420 img =
    let yuv = match img.data with YUV yuv -> yuv | _ -> assert false in
    assert (yuv.yuv_pixel = Pixel.YUVJ420);
    YUV420.make img.width img.height yuv.y yuv.y_stride yuv.u yuv.v
      yuv.uv_stride

  external rgba32_to_bgr32 : data -> int -> data -> int -> int * int -> unit
    = "caml_RGBA32_to_BGR32"

  external rgb24_to_rgba32 : data -> int -> data -> int -> int * int -> unit
    = "caml_RGB24_to_RGBA32"

  external rgb32_to_rgba32 : data -> int -> data -> int -> int * int -> unit
    = "caml_RGB32_to_RGBA32"

  let blank img =
    match img.data with
      | RGB rgb -> (
          match rgb.rgb_pixel with
            | Pixel.RGBA32 -> RGBA32.blank (to_RGBA32 img)
            | _ -> failwith "Not implemented" )
      | YUV yuv -> (
          match yuv.yuv_pixel with
            | Pixel.YUVJ420 -> YUV420.blank (to_YUV420 img)
            | _ -> failwith "Not implemented" )

  let convert ?(proportional = true) ?scale_kind src dst =
    match (src.data, dst.data) with
      | RGB s, RGB d
        when s.rgb_pixel = Pixel.RGBA32 && d.rgb_pixel = Pixel.RGBA32 ->
          let src = to_RGBA32 src in
          let dst = to_RGBA32 dst in
          RGBA32.Scale.onto ?kind:scale_kind ~proportional src dst
      | YUV s, RGB d
        when s.yuv_pixel = Pixel.YUVJ420 && d.rgb_pixel = Pixel.RGBA32 ->
          let src = to_YUV420 src in
          let src = YUV420.to_RGBA32 src in
          let dst = to_RGBA32 dst in
          RGBA32.Scale.onto ?kind:scale_kind ~proportional src dst
      | RGB s, YUV d
        when s.rgb_pixel = Pixel.RGBA32 && d.yuv_pixel = Pixel.YUVJ420 ->
          let src = to_RGBA32 src in
          let src = YUV420.of_RGBA32 src in
          let dst = to_YUV420 dst in
          YUV420.scale ~proportional src dst
      | RGB s, RGB d
        when s.rgb_pixel = Pixel.RGBA32 && d.rgb_pixel = Pixel.BGR32 ->
          if src.width = dst.width && src.height = dst.height then
            rgba32_to_bgr32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride
              (src.width, src.height)
          else raise Not_implemented
      | RGB s, RGB d
        when s.rgb_pixel = Pixel.RGB24 && d.rgb_pixel = Pixel.RGBA32 ->
          if src.width = dst.width && src.height = dst.height then
            rgb24_to_rgba32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride
              (src.width, src.height)
          else raise Not_implemented
      | RGB s, RGB d
        when s.rgb_pixel = Pixel.RGB32 && d.rgb_pixel = Pixel.RGBA32 ->
          if src.width = dst.width && src.height = dst.height then
            rgb32_to_rgba32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride
              (src.width, src.height)
          else raise Not_implemented
      | _ -> raise Not_implemented
end
