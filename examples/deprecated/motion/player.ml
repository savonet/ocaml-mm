let vidbuflen = 1
let width = 1024/2
let height = 600/2

module I = Image.RGBA32

let () = MMSDL.init ()

let prev = ref None

let motion img =
  let p =
    match !prev with
      | None -> img
      | Some prev -> prev
  in
  prev := Some img;
  (*
  let vect = I.Motion.Multi.compute 5 p img in
  I.Motion.Multi.median_denoise vect;
  let mx, my = I.Motion.Multi.mean vect in
  I.Motion.Multi.arrows vect img;
  *)
  let mx, my = I.Motion.compute 8 p img in
  Printf.printf "Motion: %d %d\n%!" mx my;
  let w, h = I.dimensions img in
  let a = 10 in
  I.Draw.line img (255,255,255,255) (w/2,h/2) (w/2+a*mx,h/2+a*my);
  img

let () =
  let fname = Sys.argv.(1) in
  let f = new MMFFmpeg.reader_of_file fname in
  let sdl = new MMSDL.writer_to_screen width height in
  let out = new MMFFmpeg.writer_to_file "out.avi" f#frame_rate width height 4000000 in
  let vid = Video.create vidbuflen in
  let loop = ref true in
  let tot = ref 0 in
  while !loop do
    let r = f#read vid 0 vidbuflen in
    tot := !tot + r;
    if r = 0 then loop := false;
    (* Printf.printf "out: %d\n%!" !tot; *)
    Video.map_all
      vid
      (fun f ->
        let img = I.create width height in
        I.Scale.onto f img;
        motion img
      );
    out#write vid 0 r;
    sdl#write vid 0 r;
  done;
  out#close;
  sdl#close;
  f#close
