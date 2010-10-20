open Gstreamer

let channels = 2
let freq = 44100

let src = "audiotestsrc"
let src = "filesrc location=../test.wav"

let () =
  Gstreamer.init ();
  Printf.printf "%s\n%!" (version_string ());
  let bin = Pipeline.parse_launch (src ^ " ! decodebin ! audioresample ! audio/x-raw-int,channels=2,rate=44100 ! appsink name=sink") in
  let sink = Bin.get_by_name (Bin.of_element bin) "sink" in

  let oss = new MMOSS.writer channels freq in

  ignore (Element.set_state bin State_playing);
  while true do
    let s = App_sink.pull_buffer_string (App_sink.of_element sink) in
    let buflen = String.length s / 4 in
    let buf = Audio.create channels buflen in
    Audio.S16LE.to_audio s 0 buf 0 buflen;
    oss#write buf 0 buflen
  done;
  ignore (Element.set_state bin State_null)
