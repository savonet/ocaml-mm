let vidbuflen = 10
(*
let width = 1024/2
let height = 600/2
*)
let width = 352
let height = 288

let () =
  let fname = Sys.argv.(1) in
  let f = MMFFmpeg.reader_of_file fname in
  let out = MMFFmpeg.writer_to_file "out.avi" (f#frame_rate/.2.) width height in
  let vid = Video.create vidbuflen in
  let loop = ref true in
  let tot = ref 0 in
  Printf.printf "FPS: %.02f\n%!" f#frame_rate;
  (* f#set_target_size width height; *)
  Graphics.open_graph "";
  Graphics.resize_window width height;
  while !loop do
    let r = f#read vid 0 vidbuflen in
    tot := !tot + r;
    (* Printf.printf "got: %d\n%!" !tot; *)
    if r = 0 then loop := false;
    Printf.printf "out: %d\n%!" !tot;
    Video.map_all
      vid
      (fun f ->
        let img = Image.RGBA8.create width height in
        Image.RGBA8.scale_to f img;
        img
      );
    out#write vid 0 r;
    if !tot >= 200 then
      (
        Printf.printf "CLOSE\n%!";
        out#close;
        exit 0
      );
    let img = vid.(0) in
    let img = Image.RGBA8.to_int_image img in
    let img = Graphics.make_image img in
    Graphics.draw_image img 0 0;
  done;
  (* out#close; *)
  f#close
