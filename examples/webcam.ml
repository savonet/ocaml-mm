open Mm

let show img =
  let width = Image.YUV420.width img in
  let height = Image.YUV420.height img in
  let img = Image.YUV420.to_int_image img in
  Graphics.open_graph "";
  Graphics.resize_window width height;
  let img = Graphics.make_image img in
  Graphics.draw_image img 0 0;
  Graphics.synchronize ();
  Graphics.loop_at_exit [] (fun _ -> ())

let () =
  let width = 640 in
  let height = 480 in
  let dev = Mm_v4l2.open_device "/dev/video0" width height in
  Graphics.open_graph "";
  Graphics.resize_window width height;
  let img = Mm_v4l2.grab dev in
  let img = Image.RGBA32.to_int_image img in
  let img = Graphics.make_image img in
  Graphics.draw_image img 0 0;
  Graphics.synchronize ();
  Graphics.loop_at_exit [] (fun _ -> ())
