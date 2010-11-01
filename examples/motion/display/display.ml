module Img = Image.RGBA32

let read_PPM ?alpha fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let data = String.create len in
  really_input ic data 0 len;
  close_in ic;
  Img.of_PPM ?alpha data

let () =
  let fname = Sys.argv.(1) in
  let img = read_PPM fname in
  let w,h = Img.dimensions img in
  Printf.printf "Dimensions: %d %d\n%!" w h;
  let img2 = Img.copy img in
  Img.Effect.translate img2 3 5;
  Img.Effect.box_blur img2;
  (*
  let vect = Img.Motion.Multi.compute 8 img img2 in
  (* Img.Motion.Multi.median_denoise vect; *)
  let mx, my = Img.Motion.Multi.mean vect in
  *)
  let mx, my = Img.Motion.compute 8 img img2 in
  Printf.printf "Motion: %d %d\n%!" mx my;
  (* Img.Motion.Multi.arrows vect img2; *)
  (* let img = Img.Scale.create ~kind:Img.Scale.Bilinear img 500 500 in *)
  Graphics.open_graph "";
  Graphics.resize_window w h;
  Graphics.draw_image (Graphics.make_image (Img.to_int_image img2)) 0 0;
  ignore (Graphics.wait_next_event [Graphics.Key_pressed])
