open Image

let () =
  print_endline "Testing YUV420.";
  let width = 5 in
  let height = 5 in
  let img = Image.YUV420.create width height in
  for j = 0 to height - 1 do
    for i = 0 to width - 1 do
      Image.YUV420.set_pixel_rgba img i j (0xff,0xff,0xff,0xff)
    done
  done;
  Image.YUV420.blank img

let ppm f =
  let buf =
    let fi = open_in f in
    let flen = in_channel_length fi in
    let buf = Bytes.create flen in
    really_input fi buf 0 flen;
    close_in fi;
    Bytes.to_string buf
  in
  let img = RGBA32.of_PPM buf in
  YUV420.of_RGBA32 img

let display img =
  let width = YUV420.width img in
  let height = YUV420.height img in
  Graphics.open_graph "";
  Graphics.resize_window width height;
  let img = Graphics.make_image (YUV420.to_int_image img) in
  Graphics.draw_image img 0 0;
  ignore (Graphics.read_key ());
  Gc.full_major ()

let () =
  Printexc.record_backtrace true;
  let img = ppm "test.ppm" in
  let img2 = YUV420.create 100 300 in
  (* YUV420.fill img2 (YUV420.Pixel.yuv_of_rgb (0xff,0,0)); *)
  (* YUV420.blank img2; *)
  YUV420.scale img img2;
  YUV420.fill_alpha img 100;
  (* YUV420.fill_alpha img2 200; *)
  YUV420.add img2 ~x:20 ~y:10 img;
  (* YUV420.fill img (Pixel.yuv_of_rgb (0xff,0,0)); *)
  (* YUV420.Effect.greyscale img; *)
  YUV420.disk_alpha img 10 10 300;
  Draw.line (fun i j -> YUV420.set_pixel_rgba img i j (0xff,0,0,0xff)) (10,30) (300,200);
  display img

  (*
let () =
  Printexc.record_backtrace true;
  let img = ppm "test.ppm" in
  let width = 100 in
  let height = 100 in
  let img =
    Printf.printf "w,h = %d,%d\n\n\n%!" width height;
    let img' = Video.Image.create width height in
    Printf.printf "alpha: %b\n%!" (YUV420.has_alpha img');
    Video.Image.scale img img';
    Printf.printf "scaled img\n\n\n%!";
    img'
  in
  let img =
    let fwidth = 300 in
    let fheight = 300 in
    Printf.printf "want video img of %d x %d\n\n\n%!" fwidth fheight;
    let img' = Video.Image.create fwidth fheight in
    Printf.printf "created video img\n\n\n%!";
    Video.Image.blank img';
    Image.YUV420.fill_alpha img' 0;
    Video.Image.add img img' ~x:50 ~y:50;
    img'
  in
  let img2 = Video.Image.create 300 300 in
  Video.Image.blit img img2;
  (* let img = Video.Image.copy img in *)
  display img2
   *)

(*
let () =
  Printexc.record_backtrace true;
  let width = 200 in
  let height = 200 in
  let img = YUV420.create width height in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      YUV420.set_pixel_rgba img i j (i, j, i+j, i*j)
    done
  done;
  let img2 = YUV420.create 500 600 in
  YUV420.blank_all img2;
  (* YUV420.fill img2 (0,0,0xff); *)
  YUV420.scale ~proportional:true img img2;
  display img2
 *)
