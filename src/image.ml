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

  (** (Y, Y stride), (U, V, UV stride) *)
  type t = (data * int) * (data * data * int)

  let width ((_,w),_) = w

  let height ((y,w),_) = Bigarray.Array1.dim y / w

  let make y ys u v uvs = (y, ys), (u, v, uvs)

  let internal buf = buf

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

  external of_YUV420 : YUV420.t -> t -> unit = "caml_rgb_of_YUV420"

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

    let create ?kind ?proportional src w h =
      let dst = create w h in
      onto ?kind ?proportional src dst;
      dst

  end

  external to_BMP : t -> string = "caml_rgb_to_bmp"

  external to_RGB8_string : t -> string = "caml_image_to_rgb8"

  (* cf to_int_image *)
  (*
  let to_graphics_image buf =
    let w = buf.width in
    let h = buf.height in
    let buf = to_RGB8_string buf in
    Array.init
      h
      (fun j ->
        Array.init
          w
          (fun i ->
            let r = int_of_char buf.[3*(j*w+i)+0] in
            let g = int_of_char buf.[3*(j*w+i)+1] in
            let b = int_of_char buf.[3*(j*w+i)+2] in
            (r lsl 16) + (g lsl 8) + b
          )
      )
  *)
(*
  let save_bmp f fname =
  let oc = open_out_bin fname in
  output_string oc (to_bmp f);
  close_out oc
*)

  exception Invalid_format of string

  (* TODO: avoid using Str *)
  let ppm_header =
    Str.regexp "P6\n\\(#.*\n\\)?\\([0-9]+\\) \\([0-9]+\\)\n\\([0-9]+\\)\n"

  let of_PPM ?alpha data =
    (
      try
	if not (Str.string_partial_match ppm_header data 0) then
          raise (Invalid_format "Not a PPM file.");
      with
	| _ -> raise (Invalid_format "Not a PPM file.")
    );
    let w = int_of_string (Str.matched_group 2 data) in
    let h = int_of_string (Str.matched_group 3 data) in
    let d = int_of_string (Str.matched_group 4 data) in
    let o = Str.match_end () in
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

      external of_color_simple : t -> int * int * int -> float -> float -> unit = "caml_rgb_color_to_alpha_simple"

      external of_color : t -> int * int * int -> float -> float -> unit = "caml_rgb_color_to_alpha"
    end
  end
end
