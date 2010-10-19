let vidbuflen = 1
let width = 1024/2
let height = 600/2

let () = MMSDL.init ()

let () =
  let fname = (* Sys.argv.(1) *) "/dev/video0" in
  let f = new MMV4L.reader fname width height in
  let sdl = new MMSDL.writer_to_screen width height in
  let vid = Video.create vidbuflen in
  let loop = ref true in
  let tot = ref 0 in
  (* f#set_target_size width height; *)
  (*
  Graphics.open_graph "";
  Graphics.resize_window width height;
  *)
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
        Image.RGBA8.Scale.onto f img;
        img
      );
    (* Video.iter_all vid Image.RGBA8.Effect.invert; *)
    sdl#write vid 0 r;
    (* if !tot >= 200 then
      (
        Printf.printf "CLOSE\n%!";
        out#close;
        exit 0
      ); *)
    (*
    let img = vid.(0) in
    let img = Image.RGBA8.to_int_image img in
    let img = Graphics.make_image img in
    Graphics.draw_image img 0 0;
    *)
  done;
  sdl#close;
  f#close
