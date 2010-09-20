module RGB8 = struct
  module Color = struct
    type t = int * int * int

    let of_int n =
      if n > 0xffffff then raise (Invalid_argument "Not a color");
      (n lsr 16) land 0xff, (n lsr 8) land 0xff, n land 0xff
  end
end

module YUV420 = struct
  (* TODO: also store width and height? *)
  type data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

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

  external create : int -> int -> t = "caml_yuv_create"

  external blank_all : t -> unit = "caml_yuv_blank"
end

module RGBA8 = struct
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
    assert (src.width = dst.width && src.height = dst.height);
    blit src dst

  let blit ?(blank=true) ?(x=0) ?(y=0) ?w ?h src dst =
    match (w,h) with
      | None, None -> blit_off src dst x y blank
      | Some w, Some h -> blit_off_scale src dst (x,y) (w,h) blank
      | _, _ -> assert false

  external fill_all : t -> Color.t -> unit = "caml_rgb_fill"

  external blank_all : t -> unit = "caml_rgb_blank" "noalloc"

  external of_RGB8_string : t -> string -> unit = "caml_rgb_of_rgb8_string"

  let of_RGB8_string data width =
    let height = (String.length data / 3) / width in
    let ans = create width height in
    of_RGB8_string ans data;
    ans

  external of_YUV420 : YUV420.yuv_data -> t -> unit = "caml_rgb_of_YUV420"

  let of_YUV420 img = of_YUV420 img.YUV420.data

  let of_YUV420 frame =
    let ans = create (YUV420.width frame) (YUV420.height frame) in
    of_YUV420 frame ans;
    ans

  external to_YUV420 : t -> YUV420.t -> unit = "caml_rgb_to_YUV420"

  external get_pixel : t -> int -> int -> Color.t = "caml_rgb_get_pixel"

  external set_pixel : t -> int -> int -> Color.t -> unit = "caml_rgb_set_pixel"

  external randomize_all : t -> unit = "caml_rgb_randomize"

  module Scale = struct
    type kind = Linear | Bilinear

    external scale_coef : t -> t -> int * int -> int * int -> unit = "caml_rgb_scale"

    external bilinear_scale_coef : t -> t -> float -> float -> unit = "caml_rgb_bilinear_scale"

    let scale_coef_kind k src dst (dw,sw) (dh,sh) =
      if dw <> sw || dh <> sh then
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

  external to_RGB8_string : t -> string = "caml_image_to_rgb8"

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

  (* TODO: naming RGB8 vs RGBA32 *)
  let of_RGBA8 img =
    let rgb_data =
      {
        rgb_pixel = Pixel.RGBA32;
        rgb_data = img.RGBA8.data;
        rgb_stride = img.RGBA8.stride;
      }
    in
      {
        data = RGB rgb_data;
        width = img.RGBA8.width;
        height = img.RGBA8.height;
      }

  let to_RGBA8 img =
    let rgb_data =
      match img.data with
        | RGB d -> d
        | _ -> assert false
    in
    assert (rgb_data.rgb_pixel = Pixel.RGBA32);
    {
      RGBA8.
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

  let convert ?(copy=false) ?(proportional=true) ?scale_kind src dst =
    let is_rgb img =
      match img.data with
        | RGB x when x.rgb_pixel = Pixel.RGBA32 -> true
        | YUV x when x.yuv_pixel = Pixel.YUVJ420 -> false
        | _ -> raise Not_implemented
    in
    match is_rgb src,is_rgb dst with
      | false,false -> raise Not_implemented (* TODO *)
      | true,true ->
        let src = to_RGBA8 src in
        let dst = to_RGBA8 dst in
        RGBA8.Scale.onto ?kind:scale_kind ~proportional src dst
      | false,true ->
        let src = to_YUV420 src in
        let src = RGBA8.of_YUV420 src in
        let dst = to_RGBA8 dst in
      (* TODO: optim: we can reuse the data of rgb instead of blitting to dst
         when the dims are the same. *)
        RGBA8.Scale.onto ?kind:scale_kind ~proportional src dst
      | true,false ->
        let src = to_RGBA8 src in
        let src = RGBA8.Scale.create ?kind:scale_kind ~proportional ~copy:false src dst.width dst.height in
        let dst = to_YUV420 dst in
        RGBA8.to_YUV420 src dst
end
