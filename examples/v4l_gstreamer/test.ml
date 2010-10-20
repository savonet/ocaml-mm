open Gstreamer

let width = 320
let height = 240

let () =
  Gstreamer.init ();
  Printf.printf "%s\n%!" (version_string ());
  let bin = Pipeline.parse_launch "videotestsrc ! videoscale ! appsink max_buffers=2 drop=true caps=video/x-raw-rgb,width=320,height=240,framerate='(fraction)'10/1,bpp=32,depth=24 name=sink" in
  let sink = Bin.get_by_name (Bin.of_element bin) "sink" in

  let sdl = new MMSDL.writer_to_screen width height in
  let vid = Video.create 1 in

  ignore (Element.set_state bin State_playing);
  while true do
    let b = App_sink.pull_buffer (App_sink.of_element sink) in
    let blen = Bigarray.Array1.dim b in
    Printf.printf "%d\n%!" (blen / (width * height));
    let img = Image.Generic.make_rgb Image.Generic.Pixel.RGB32 width height b in
    let out = Image.RGBA8.create width height in
    Image.Generic.convert ~copy:true ~proportional:true img (Image.Generic.of_RGBA8 out);
    vid.(0) <- out;
    sdl#write vid 0 1
  done;
  ignore (Element.set_state bin State_null)
