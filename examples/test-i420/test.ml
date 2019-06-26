open Image

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
  I420.of_RGBA32 img

let display img =
  let width = I420.width img in
  let height = I420.height img in
  Graphics.open_graph "";
  Graphics.resize_window width height;
  let img = Graphics.make_image (I420.to_int_image img) in
  Graphics.draw_image img 0 0;
  ignore (Graphics.read_key ());
  Gc.full_major ()

  (*
let () =
  Printexc.record_backtrace true;
  let img = ppm "test.ppm" in
  let img2 = I420.create 100 300 in
  (* I420.fill img2 (I420.Pixel.yuv_of_rgb (0xff,0,0)); *)
  (* I420.blank img2; *)
  I420.scale img img2;
  I420.fill_alpha img 100;
  (* I420.fill_alpha img2 200; *)
  I420.add img2 ~x:20 ~y:10 img;
display img
   *)

let () =
  Printexc.record_backtrace true;
  let img = ppm "test.ppm" in
  let width = 100 in
  let height = 100 in
  let img =
    Printf.printf "w,h = %d,%d\n\n\n%!" width height;
    let img' = Video.Image.create width height in
    Printf.printf "alpha: %b\n%!" (I420.has_alpha img');
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
    Image.I420.fill_alpha img' 0;
    Video.Image.add img img' ~x:50 ~y:50;
    img'
  in
  let img2 = Video.Image.create 300 300 in
  Video.Image.blit img img2;
  (* let img = Video.Image.copy img in *)
  display img2
