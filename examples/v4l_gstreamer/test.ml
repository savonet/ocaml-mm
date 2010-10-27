open Gstreamer

let width = 320
let height = 240
let fps = 10

let src = "videotestsrc"
let src = "v4l2src device=/dev/video0"

let pipeline = Printf.sprintf "%s ! ffmpegcolorspace ! videoscale ! appsink max-buffers=2 drop=true name=sink caps=\"video/x-raw-rgb,width=%d,height=%d,pixel-aspect-ratio=1/1,bpp=(int)24,depth=(int)24,endianness=(int)4321,red_mask=(int)0xff0000,green_mask=(int)0x00ff00,blue_mask=(int)0x0000ff,framerate=(fraction)10/1\"" src width height

let () =
  Gstreamer.init ();
  Printf.printf "%s\n%!" (version_string ());
  Printf.printf "%s\n%!" pipeline;
  let bin =  Pipeline.parse_launch pipeline in

  let sink = Bin.get_by_name (Bin.of_element bin) "sink" in

  let sdl = new MMSDL.writer_to_screen width height in
  let vid = Video.create 1 in

  ignore (Element.set_state bin State_playing);
  while true do
    let b = App_sink.pull_buffer (App_sink.of_element sink) in
    let blen = Bigarray.Array1.dim b in
    let img = Image.Generic.make_rgb Image.Generic.Pixel.RGB24 width height b in
    let out = Image.RGBA32.create width height in
    Image.Generic.convert ~copy:true ~proportional:true img (Image.Generic.of_RGBA32 out);
    vid.(0) <- out;
    sdl#write vid 0 1
  done;
  ignore (Element.set_state bin State_null)
