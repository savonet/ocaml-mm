open Mm_image

let () =
  Printf.printf "Testing video...\n\n";
  let a = Image.YUV420.create 640 480 in
  Image.YUV420.blank a
