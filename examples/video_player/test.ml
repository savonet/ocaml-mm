open Gstreamer

let width = 320
let height = 240
let fps = 24
let audio_channels = 2
let audio_rate = 44100

let src = "filesrc location=../test.wmv"

let pipeline =
  Printf.sprintf "%s ! decodebin name=decode \
decode. ! ffmpegcolorspace ! videoscale ! videorate ! appsink max-buffers=2 drop=true name=videosink caps=\"video/x-raw-rgb,width=%d,height=%d,pixel-aspect-ratio=1/1,bpp=(int)24,depth=(int)24,endianness=(int)4321,red_mask=(int)0xff0000,green_mask=(int)0x00ff00,blue_mask=(int)0x0000ff,framerate=(fraction)%d/1\" \
decode. ! audioconvert ! audioresample ! appsink max-buffers=2 drop=true name=audiosink caps=\"audio/x-raw-int,width=16,channels=%d,rate=%d,signed=true\"" src width height fps audio_channels audio_rate

let () =
  Gstreamer.init ();
  Printf.printf "%s\n%!" (version_string ());
  Printf.printf "%s\n%!" pipeline;
  let bin =  Pipeline.parse_launch pipeline in

  let videosink = Bin.get_by_name (Bin.of_element bin) "videosink" in
  let audiosink = Bin.get_by_name (Bin.of_element bin) "audiosink" in

  let sdl = new MMSDL.writer_to_screen width height in
  let oss = new MMOSS.writer audio_channels audio_rate in
  let vid = Video.create 1 in

  ignore (Element.set_state bin State_playing);
  while true do
    (* Video *)
    let b = App_sink.pull_buffer (App_sink.of_element videosink) in
    let img = Image.Generic.make_rgb Image.Generic.Pixel.RGB24 width height b in
    let out = Image.RGBA32.create width height in
    Image.Generic.convert ~copy:true ~proportional:true img (Image.Generic.of_RGBA32 out);
    vid.(0) <- out;
    sdl#write vid 0 1;

    (* Audio *)
    let b = App_sink.pull_buffer_string (App_sink.of_element audiosink) in
    let samples = Audio.S16LE.duration audio_channels (String.length b) in
    let buf = Audio.create audio_channels samples in
    Audio.S16LE.to_audio b 0 buf 0 samples;
    oss#write buf 0 samples
  done;
  ignore (Element.set_state bin State_null)
