module Motion_multi = struct
  type vectors_data = (int, Bigarray.nativeint_elt, Bigarray.c_layout) Bigarray.Array1.t

  type vectors =
      {
        vectors : vectors_data;
        vectors_width : int;
        block_size: int;
      }

  external median_denoise : int -> vectors_data -> unit = "caml_rgb_motion_multi_median_denoise"

  let median_denoise v = median_denoise v.vectors_width v.vectors

  external mean : int -> vectors_data -> int * int = "caml_rgb_motion_multi_mean"

  let mean v = mean v.vectors_width v.vectors
end

module RGB8 = struct
  module Color = struct
    type t = int * int * int

    let of_int n =
      if n > 0xffffff then raise (Invalid_argument "Not a color");
      (n lsr 16) land 0xff, (n lsr 8) land 0xff, n land 0xff
  end
end

module Gray8 = struct
  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t =
      {
        data : data;
        width : int;
      }

  let make w d =
    {
      data = d;
      width = w;
    }

  let create w h =
    make w (Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w*h))

  module Motion = struct
    external compute : int -> int -> data -> data -> int * int = "caml_mm_Gray8_motion_compute"

    let compute bs o n = compute bs n.width o.data n.data

    module Multi = struct
      include Motion_multi

      external compute : int -> int -> data -> data -> vectors_data = "caml_mm_Gray8_motion_multi_compute"

      let compute bs o n =
        {
          vectors = compute bs n.width o.data n.data;
          vectors_width = n.width / bs;
          block_size = bs;
        }
    end
  end
end


module YUV420 = struct
  (* TODO: also store width and height? *)
  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  let kind = Bigarray.int8_unsigned

  type yuv_data = (data * int) * (data * data * int)

  (** (Y, Y stride), (U, V, UV stride) *)
  type t =
      {
        data : yuv_data;
        width : int;
        height : int;
      }

  let width img = img.width

  let height img = img.height

  let make w h y ys u v uvs =
    {
      data = (y, ys), (u, v, uvs);
      width = w;
      height = h;
    }

  let internal img = img.data

  let create w h =
    let len = w * h in
    let create len =
      Bigarray.Array1.create kind Bigarray.c_layout len
    in
    let y = create len in
    let u = create (len/4) in
    let v = create (len/4) in
    make w h y w u v (w/2)

  external blank : data -> unit = "caml_yuv_blank"

  let blank_all x =
    let (y,_),(u,v,_) = x.data in
    blank y ; blank u ; blank v
end

module RGBA32 = struct
  module Color = struct
    type t = int * int * int * int
  end

  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t =
      {
	(* Order matters for C callbacks! *)
	data   : data;
	width  : int;
	height : int;
	stride : int
      }

  let width buf = buf.width

  let height buf = buf.height

  let dimensions buf = buf.width, buf.height

  let data buf = buf.data

  let stride buf = buf.stride

  let make ?stride width height data =
    let stride =
      match stride with
	| Some v -> v
	| None -> 4*width
    in
    {
      data   = data;
      width  = width;
      height = height;
      stride = stride
    }

  let create ?stride width height =
    let stride =
      match stride with
	| Some v -> v
	| None -> 4*width
    in
    let data =
      Bigarray.Array1.create
	Bigarray.int8_unsigned Bigarray.c_layout
	(stride*height)
    in
    make ~stride width height data

  let copy f =
    let nf = create ~stride:f.stride f.width f.height in
    Bigarray.Array1.blit f.data nf.data;
    nf

  (* Remove the optional stride argument. *)
  let create width height = create width height

  external blit : t -> t -> unit = "caml_rgb_blit"

  external blit_off : t -> t -> int -> int -> bool -> unit = "caml_rgb_blit_off"

  external blit_off_scale : t -> t -> int * int -> int * int -> bool -> unit = "caml_rgb_blit_off_scale"

  let blit_all src dst =
    assert (src.width = dst.width && src.height = dst.height && src.stride = dst.stride);
    blit src dst

  let blit ?(blank=true) ?(x=0) ?(y=0) ?w ?h src dst =
    match (w,h) with
      | None, None -> blit_off src dst x y blank
      | Some w, Some h -> blit_off_scale src dst (x,y) (w,h) blank
      | _, _ -> assert false

  external fill_all : t -> Color.t -> unit = "caml_rgb_fill"

  external blank_all : t -> unit = "caml_rgb_blank" "noalloc"

  external of_RGB24_string : t -> string -> unit = "caml_rgb_of_rgb8_string"

  let of_RGB24_string data width =
    let height = (String.length data / 3) / width in
    let ans = create width height in
    of_RGB24_string ans data;
    ans

  external of_YUV420 : YUV420.yuv_data -> t -> unit = "caml_rgb_of_YUV420"

  let of_YUV420 img = of_YUV420 img.YUV420.data

  let of_YUV420 frame =
    let ans = create (YUV420.width frame) (YUV420.height frame) in
    of_YUV420 frame ans;
    ans

  external to_YUV420 : t -> YUV420.yuv_data -> unit = "caml_rgb_to_YUV420"

  let to_YUV420 x y = to_YUV420 x y.YUV420.data

  external to_Gray8 : t -> Gray8.data -> unit = "caml_mm_RGBA8_to_Gray8"

  let to_Gray8 rgb gray = to_Gray8 rgb gray.Gray8.data

  let to_Gray8_create rgb =
    let gray = Gray8.create (width rgb) (height rgb) in
    to_Gray8 rgb gray;
    gray

  external get_pixel : t -> int -> int -> Color.t = "caml_rgb_get_pixel"

  external set_pixel : t -> int -> int -> Color.t -> unit = "caml_rgb_set_pixel"

  external randomize_all : t -> unit = "caml_rgb_randomize"

  module Scale = struct
    type kind = Linear | Bilinear

    external scale_coef : t -> t -> int * int -> int * int -> unit = "caml_rgb_scale"

    external bilinear_scale_coef : t -> t -> float -> float -> unit = "caml_rgb_bilinear_scale"

    let scale_coef_kind k src dst (dw,sw) (dh,sh) =
      match k with
        | Linear ->
          scale_coef src dst (dw,sw) (dh,sh)
        | Bilinear ->
          let x = float dw /. float sw in
          let y = float dh /. float sh in
          bilinear_scale_coef src dst x y

    let onto ?(kind=Linear) ?(proportional=false) src dst =
      let sw, sh = src.width,src.height in
      let dw, dh = dst.width,dst.height in
      if dw = sw && dh = sh then
        blit_all src dst
      else
        (
          if not proportional then
            scale_coef_kind kind src dst (dw, sw) (dh, sh)
          else
            let n, d =
              if dh * sw < sh * dw then
	        dh, sh
              else
	        dw, sw
            in
            scale_coef_kind kind src dst (n,d) (n,d)
        )

    let create ?kind ?(copy=true) ?proportional src w h =
      if not copy && width src = w && height src = h then
        src
      else
        let dst = create w h in
        onto ?kind ?proportional src dst;
        dst
  end

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
          let (!!) = int_of_char in
          while !!'0' <= !!(data.[!n]) && !!(data.[!n]) <= !!'9' do
            ans := !ans * 10 + !!(data.[!n]) - !!'0';
            incr n
          done;
          assert (data.[!n] = ' ' || data.[!n] = '\n');
          incr n;
          !ans
        in
        if data.[!n] = '#' then
          (
            incr n;
            while data.[!n] <> '\n' do incr n done;
            incr n
          );
        let w = read_int () in
        let h = read_int () in
        let d = read_int () in
        w,h,d,!n
      with
	| _ -> raise (Invalid_format "Not a PPM file.")
    in
    let datalen = String.length data - o in
    if d <> 255 then
      raise (Invalid_format (Printf.sprintf "Files of color depth %d \
                                             are not handled." d));
    if datalen < 3*w*h then
      raise (Invalid_format (Printf.sprintf "Got %d bytes of data instead of \
                                             expected %d." datalen (3*w*h)));
    let ans = create w h in
    for j = 0 to h - 1 do
      for i = 0 to w - 1 do
        let r, g, b =
          int_of_char data.[o + 3 * (j * w + i) + 0],
          int_of_char data.[o + 3 * (j * w + i) + 1],
          int_of_char data.[o + 3 * (j * w + i) + 2]
        in
        let a =
          match alpha with
            | Some (ra, ga, ba) ->
              if r = ra && g = ga && b = ba then 0x00 else 0xff
            | None -> 0xff
        in
        set_pixel ans i j (r, g, b, a);
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

  external add_off_scale : t -> t -> int * int -> int * int -> unit = "caml_rgb_add_off_scale"

  let add ?(x=0) ?(y=0) ?w ?h src dst =
    match (w,h) with
      | None, None ->
        if x = 0 && y = 0 && src.width = dst.width && src.height = dst.height then
          add_fast src dst
        else
          add_off src dst x y
      | Some w, Some h -> add_off_scale src dst (x,y) (w,h)
      | _, _ -> assert false

  module Effect = struct
    external greyscale : t -> bool -> unit = "caml_rgb_greyscale"

    let sepia buf = greyscale buf true

    let greyscale buf = greyscale buf false

    external invert : t -> unit = "caml_rgb_invert"

    external rotate : t -> float -> unit = "caml_rgb_rotate"

    external affine : t -> float -> float -> int -> int -> unit = "caml_rgb_affine"

    (* TODO: faster implementation? *)
    let translate f x y = affine f 1. 1. x y

    external mask : t -> t -> unit = "caml_rgb_mask"

    external lomo : t -> unit = "caml_rgb_lomo"

    external box_blur : t -> unit = "caml_mm_RGBA8_box_blur"

    module Alpha = struct
      external scale : t -> float -> unit = "caml_rgb_scale_opacity"

      external blur : t -> unit = "caml_rgb_blur_alpha"

      external disk : t -> int -> int -> int -> unit = "caml_rgb_disk_opacity"

      external of_color_simple : t -> int * int * int -> int -> unit = "caml_rgb_color_to_alpha_simple"
      (* TODO: this does not work yet. *)
      external of_color : t -> int * int * int -> float -> float -> unit = "caml_rgb_color_to_alpha"
      let of_color = of_color_simple
    end
  end

  module Motion = struct
    (* TODO: compute old only once? *)
    let compute bs o n = Gray8.Motion.compute bs (to_Gray8_create o) (to_Gray8_create n)

    module Multi = struct
      include Motion_multi

      let compute bs o n =
        Gray8.Motion.Multi.compute bs (to_Gray8_create o) (to_Gray8_create n)

      external arrows : int -> vectors_data -> t -> unit = "caml_rgb_motion_multi_arrows"

      let arrows v img =
        arrows v.block_size v.vectors img
    end
  end
end

module Generic = struct
  exception Not_implemented

  module Pixel = struct
    type rgb_format =
      | RGB24       (* 24 bit RGB. Each color is an uint8_t. Color order is RGBRGB *)
      | BGR24       (* 24 bit BGR. Each color is an uint8_t. Color order is BGRBGR *)
      | RGB32       (* 32 bit RGB. Each color is an uint8_t. Color order is RGBXRGBX, where X is unused *)
      | BGR32       (* 32 bit BGR. Each color is an uint8_t. Color order is BGRXBGRX, where X is unused *)
      | RGBA32      (* 32 bit RGBA. Each color is an uint8_t. Color order is RGBARGBA *)
    type yuv_format =
      | YUV422    (* Planar YCbCr 4:2:2. Each component is an uint8_t *)
      | YUV444    (* Planar YCbCr 4:4:4. Each component is an uint8_t *)
      | YUV411    (* Planar YCbCr 4:1:1. Each component is an uint8_t *)
      | YUV410    (* Planar YCbCr 4:1:0. Each component is an uint8_t *)
      | YUVJ420   (* Planar YCbCr 4:2:0. Each component is an uint8_t,
                   * luma and chroma values are full range (0x00 .. 0xff) *)
      | YUVJ422   (* Planar YCbCr 4:2:2. Each component is an uint8_t,
                   * luma and chroma values are full range (0x00 .. 0xff) *)
      | YUVJ444   (* Planar YCbCr 4:4:4. Each component is an uint8_t, luma and
                   * chroma values are full range (0x00 .. 0xff) *)
    type format =
      | RGB of rgb_format
      | YUV of yuv_format

    let size = function
      | RGB x ->
        begin
          match x with
            | RGB24
            | BGR24 -> 3
            | RGB32
            | BGR32
            | RGBA32 -> 4
        end
      | YUV _ -> raise Not_implemented

    let string_of_format = function
      | RGB x ->
        begin
          match x with
            | RGB24  -> "RGB24"
            | BGR24  -> "BGR32"
            | RGB32  -> "RGB32"
            | BGR32  -> "BGR32"
            | RGBA32 -> "RGBA32"
        end
      | YUV x ->
        begin
          match x with
            | YUV422  -> "YUV422"
            | YUV444  -> "YUV444"
            | YUV411  -> "YUV411"
            | YUV410  -> "YUV410"
            | YUVJ420 -> "YUVJ420"
            | YUVJ422 -> "YUVJ422"
            | YUVJ444 -> "YUVJ444"
        end
  end

  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type rgb =
      {
        rgb_pixel : Pixel.rgb_format;
        rgb_data : data;
        rgb_stride : int;
      }

  type yuv =
      {
        yuv_pixel  : Pixel.yuv_format;
        y          : data;
        y_stride   : int;
        u          : data;
        v          : data;
        uv_stride  : int;
      }

  type t_data =
    | RGB of rgb
    | YUV of yuv

  type t =
      {
        data : t_data;
        width : int;
        height : int;
      }

  let rgb_data img =
    match img.data with
      | RGB rgb -> rgb.rgb_data, rgb.rgb_stride
      | _ -> assert false

  let yuv_data img =
    match img.data with
      | YUV yuv -> (yuv.y,yuv.y_stride),(yuv.u,yuv.v,yuv.uv_stride)
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
        | None -> width * (Pixel.size (Pixel.RGB pix))
    in
    let rgb_data =
      {
        rgb_pixel = pix;
        rgb_data = data;
        rgb_stride = stride;
      }
    in
    {
      data = RGB rgb_data;
      width = width;
      height = height;
    }

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
    let rgb_data =
      match img.data with
        | RGB d -> d
        | _ -> assert false
    in
    assert (rgb_data.rgb_pixel = Pixel.RGBA32);
    {
      RGBA32.
      data = rgb_data.rgb_data;
      width = img.width;
      height = img.height;
      stride = rgb_data.rgb_stride;
    }

  let of_YUV420 img =
    let (y,y_stride),(u,v,uv_stride) = img.YUV420.data in
    let yuv_data =
      {
        yuv_pixel = Pixel.YUVJ420;
        y = y;
        y_stride = y_stride;
        u = u;
        v = v;
        uv_stride = uv_stride
      }
    in
    {
      data = YUV yuv_data;
      width = img.YUV420.width;
      height = img.YUV420.height;
    }

  let to_YUV420 img =
    let yuv =
      match img.data with
        | YUV yuv -> yuv
        | _ -> assert false
    in
    assert (yuv.yuv_pixel = Pixel.YUVJ420);
    {
      YUV420.
      data = yuv_data img;
      width = img.width;
      height = img.height;
    }

  external rgba32_to_bgr32 : data -> int -> data -> int -> int * int -> unit = "caml_RGBA32_to_BGR32"

  external rgb24_to_rgba32 : data -> int -> data -> int -> int * int -> unit = "caml_RGB24_to_RGBA32"

  external rgb32_to_rgba32 : data -> int -> data -> int -> int * int -> unit = "caml_RGB32_to_RGBA32"

  let convert ?(copy=false) ?(proportional=true) ?scale_kind src dst =
    match src.data, dst.data with
      | RGB s, RGB d when s.rgb_pixel = Pixel.RGBA32 && d.rgb_pixel = Pixel.RGBA32 ->
        let src = to_RGBA32 src in
        let dst = to_RGBA32 dst in
        RGBA32.Scale.onto ?kind:scale_kind ~proportional src dst
      | YUV s, RGB d when s.yuv_pixel = Pixel.YUVJ420 && d.rgb_pixel = Pixel.RGBA32 ->
        let src = to_YUV420 src in
        let src = RGBA32.of_YUV420 src in
        let dst = to_RGBA32 dst in
        RGBA32.Scale.onto ?kind:scale_kind ~proportional src dst
      | RGB s, YUV d when s.rgb_pixel = Pixel.RGBA32 && d.yuv_pixel = Pixel.YUVJ420 ->
        let src = to_RGBA32 src in
        let src = RGBA32.Scale.create ?kind:scale_kind ~proportional ~copy:false src dst.width dst.height in
        let dst = to_YUV420 dst in
        RGBA32.to_YUV420 src dst
      | RGB s, RGB d when s.rgb_pixel = Pixel.RGBA32 && d.rgb_pixel = Pixel.BGR32 ->
        if src.width = dst.width && src.height = dst.height then
          rgba32_to_bgr32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride (src.width,src.height)
        else
          raise Not_implemented
      | RGB s, RGB d when s.rgb_pixel = Pixel.RGB24 && d.rgb_pixel = Pixel.RGBA32 ->
        if src.width = dst.width && src.height = dst.height then
          rgb24_to_rgba32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride (src.width,src.height)
        else
          raise Not_implemented
      | RGB s, RGB d when s.rgb_pixel = Pixel.RGB32 && d.rgb_pixel = Pixel.RGBA32 ->
        if src.width = dst.width && src.height = dst.height then
          rgb32_to_rgba32 s.rgb_data s.rgb_stride d.rgb_data d.rgb_stride (src.width,src.height)
        else
          raise Not_implemented
      | _ -> raise Not_implemented
end
