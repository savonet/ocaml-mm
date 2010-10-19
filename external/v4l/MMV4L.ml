module G = Image.Generic

module V4L = struct
  type device

  external opendev : string -> int -> int -> int -> device = "caml_v4l_open"

  external grab : device -> G.data -> unit = "caml_v4l_grab"

  external close : device -> unit = "caml_v4l_close"
end

class reader device width height =
object (self)
  val dev = V4L.opendev device width height (3 * width)

  val img =
    let data = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height * 3) in
    G.make_rgb G.Pixel.RGB24 width height data

  method frame_rate = 12.

  method width = width

  method height = height

  method read buf ofs len =
    V4L.grab dev (fst (G.rgb_data img));
    for i = ofs to ofs + len - 1 do
      G.convert ~copy:true ~proportional:true img (G.of_RGBA8 buf.(i))
    done;
    len

  method close =
    V4L.close dev
end
