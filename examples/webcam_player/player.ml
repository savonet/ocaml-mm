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
  while !loop do
    let r = f#read vid 0 vidbuflen in
    tot := !tot + r;
    (* Printf.printf "got: %d\n%!" !tot; *)
    if r = 0 then loop := false;
    Printf.printf "out: %d\n%!" !tot;
    (*
    Video.map_all
      vid
      (fun f ->
        let img = Image.RGBA32.create width height in
        Image.RGBA32.Scale.onto f img;
        img
      );
    *)
    (* Video.iter_all vid Image.RGBA32.Effect.invert; *)
    sdl#write vid 0 r
  done;
  sdl#close;
  f#close
