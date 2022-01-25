open Mm_audio
open Mm_image

let time name f =
  Printf.printf "- %s... %!" name;
  let t0 = Sys.time () in
  f ();
  let t1 = Sys.time () in
  Printf.printf "%.02f s\n%!" (t1 -. t0)

let () =
  Printf.printf "\n# Testing MM library\n\n%!"

let () =
  Printf.printf "## Testing audio\n\n%!";
  time "adding many buffers" (fun () ->
      let a = Audio.create 2 44100 in
      for _ = 1 to 10000 do
        let b = Audio.create 2 44100 in
        Audio.add b a
      done
    );
  Printf.printf "\n"

let () =
  Printf.printf "## Testing video\n\n%!";
  let a = Image.YUV420.create 640 480 in
  Image.YUV420.blank a;
  let b = Image.YUV420.create 64 64 in
  Printf.printf "- adding images\n%!";
  Image.YUV420.add b a;
  Image.YUV420.add b ~x:10 ~y:10 a;
  Image.YUV420.add b ~x:1000 ~y:1000 a;
  Printf.printf "\n"
