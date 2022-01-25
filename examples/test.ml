open Mm_audio
open Mm_image

let skip_long = false

let test name f =
  Printf.printf "- %s... %!" name;
  f ();
  Printf.printf "ok\n%!"

let time ?(skip=false) name f =
  Printf.printf "- %s... %!" name;
  if skip then Printf.printf "skipped\n%!" else
    let t0 = Sys.time () in
    f ();
    let t1 = Sys.time () in
    Printf.printf "%.02f s\n%!" (t1 -. t0)

let () =
  Printf.printf "\n# Testing MM library\n\n%!"

let () =
  Printf.printf "## Testing audio\n\n%!";
  time ~skip:skip_long "adding many buffers" (fun () ->
      let a = Audio.create 2 44100 in
      for _ = 1 to 10000 do
        let b = Audio.create 2 44100 in
        Audio.add b a
      done
    );
  Printf.printf "\n"

let () =
  Printf.printf "## Testing video\n\n%!";
  test "fill buffer" (fun () ->
      let a = Image.YUV420.create 10 10 in
      Image.YUV420.fill a (0,0,0)
    );
  test "test various sizes" (fun () ->
      for i = 0 to 7 do
        for j = 0 to 7 do
          let a = Image.YUV420.create (16+i) (16+j) in
          Image.YUV420.fill a (0,0,0)
        done
      done
    );
  test "adding images" (fun () ->
      let a = Image.YUV420.create 640 480 in
      Image.YUV420.blank a;
      Image.YUV420.fill a (10, 10, 10);
      let b = Image.YUV420.create 64 64 in
      Image.YUV420.fill b (10, 10, 10);
      Image.YUV420.add b a;
      Image.YUV420.add b ~x:10 ~y:10 a;
      Image.YUV420.add b ~x:(-10) ~y:(-10) a;
      Image.YUV420.add b ~x:1000 ~y:1000 a;
    );
  Printf.printf "\n"

let () =
  Gc.full_major ()
