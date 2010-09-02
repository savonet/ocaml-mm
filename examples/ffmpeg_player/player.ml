let fps = 5
let width = 1024
let height = 600

let () =
  let fname = Sys.argv.(1) in
  let f = MMFFmpeg.reader_of_file fname in
  let vid = Video.create fps in
  let loop = ref true in
  let tot = ref 0 in
  (* f#set_target_size width height; *)
  Graphics.open_graph "";
  Graphics.resize_window width height;
  while !loop do
    let r = f#read vid 0 fps in
    tot := !tot + r;
    Printf.printf "got: %d\n%!" !tot;
    if r = 0 then loop := false;
    let img = Image.RGBA8.create width height in
    Image.RGBA8.scale_to vid.(0) img;
    let img = Image.RGBA8.to_int_image img in
    let img = Graphics.make_image img in
    Graphics.draw_image img 0 0
  done;
  f#close
