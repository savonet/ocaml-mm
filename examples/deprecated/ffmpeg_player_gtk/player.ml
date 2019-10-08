let fps = 50
let width = 512
let height = 300

let play (d:GDraw.drawable) () =
  let fname = Sys.argv.(1) in
  let f = new MMFFmpeg.reader_of_file fname in
  let vid = Video.create fps in
  let loop = ref true in
  let tot = ref 0 in
  Printf.printf "FPS: %.02f\n%!" f#frame_rate;
  (* f#set_target_size width height; *)
  while !loop do
    let r = f#read vid 0 fps in
    tot := !tot + r;
    Printf.printf "got: %d\n%!" !tot;
    if r = 0 then loop := false;
    let img = Image.RGBA32.create width height in
    Image.RGBA32.Scale.onto vid.(0) img;
    Image.RGBA32.Effect.rotate img 3.1416;
    let img = Image.RGBA32.to_RGB24_string img in
    d#put_rgb_data ~width ~height (Gpointer.region_of_string img)
  done;
  f#close

let () =
  let w = new Gui.window () in
  w#da#misc#realize ();
  let drawable = new GDraw.drawable w#da#misc#window in
  ignore (w#window#connect#destroy ~callback:GMain.Main.quit);
  ignore (w#menu_quit#connect#activate ~callback:GMain.Main.quit);
  ignore (w#menu_play#connect#activate ~callback:(play drawable));
  w#window#show ();
  GtkThread.main ()
