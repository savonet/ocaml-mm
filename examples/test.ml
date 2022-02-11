open Mm

let skip_long = ref false

let () =
  Printexc.record_backtrace true;
  Arg.parse
    [
      "--skip-long", Arg.Set skip_long, "Skip long tests."
    ]
    (fun _ -> ())
    "test [options]"

let write fname s =
  if not (Sys.file_exists "out") then Sys.mkdir "out" 0o755;
  let fname = "out/"^fname in
  let oc = open_out fname in
  output_string oc s;
  close_out oc

let test ?(skip=false) name f =
  Printf.printf "- %s... %!" name;
  if skip then Printf.printf "skipped\n%!" else
    (
      f ();
      Printf.printf "ok\n%!"
    )

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
  Printf.printf "## Architecture\n\n%!";
  Printf.printf "- word size: %d\n%!" Sys.word_size;
  Printf.printf "\n%!"

let () =
  Printf.printf "## Testing audio\n\n%!";
  test "basic functions" (fun () ->
      let a = Audio.create 2 44100 in
      Audio.noise a;
      Audio.pan 0.4 a;
      ignore (Audio.squares a);
      Audio.amplify 0.5 a
    );
  time ~skip:!skip_long "adding many buffers" (fun () ->
      let a = Audio.create 2 44100 in
      for _ = 1 to 10000 do
        let b = Audio.create 2 44100 in
        Audio.add b a
      done
    );
  Printf.printf "\n"

let () =
  Printf.printf "## Testing images\n\n%!";
  test "rounding" (fun () ->
      for k = 1 to 5 do
        for n = 0 to 33 do
          assert (Image.Data.round k n = Float.to_int (float k *. Float.ceil (float n /. float k)))
        done
      done
    );
  test "fill buffer" (fun () ->
      let a = Image.YUV420.create 10 10 in
      Image.YUV420.fill a (0,0,0)
    );
  test "various sizes" (fun () ->
      for i = 0 to 7 do
        for j = 0 to 7 do
          let w = 16+i in
          let h = 16+j in
          let a = Image.YUV420.create w h in
          Image.YUV420.set_pixel_rgba a (w-1) (h-1) (0,0,0,0);
          Image.YUV420.fill a (0,0,0);
          Image.YUV420.randomize a
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
  test "converting" (fun () ->
      let a = Image.YUV420.create 640 480 in
      let b = Image.YUV420.of_RGBA32 (Image.YUV420.to_RGBA32 a) in
      ignore b
    );
  test "blank" (fun () ->
      let img = Image.YUV420.create 640 480 in
      Image.YUV420.blank img;
      write "blank.bmp" (Image.YUV420.to_BMP img)
    );
  test "add" (fun () ->
      let img = Image.YUV420.create 640 480 in
      Image.YUV420.blank img;
      Image.YUV420.fill_alpha img 0;
      let r = Image.YUV420.create 200 100 in
      Image.YUV420.fill r (Image.Pixel.yuv_of_rgb (0xff,0,0));
      Image.YUV420.add r ~x:10 ~y:70 img;
      Image.YUV420.rotate (Image.YUV420.copy img) 200 200 0.7 img;
      write "add.bmp" (Image.YUV420.to_BMP img)
    );
  let module C = Image.Canvas(struct include Image.YUV420 let create w h = create w h let scale = scale ~proportional:false end) in
  test "canvas line" (fun () ->
      for _ = 1 to 100 do
        let l = C.Draw.line (0xff,0xff,0xff,0xff) (15,24) (59,78) in
        ignore l
      done
    );
  test "gradient" (fun () ->
      let img = Image.YUV420.create 640 480 in
      Image.YUV420.gradient_uv img (0,0) (100,200) (200,150);
      write "gradient.bmp" (Image.YUV420.to_BMP img)
    );
  test "manual gradient" (fun () ->
      let d = 400 in
      let img = Image.YUV420.create d d in
      for j = 0 to d - 1 do
        for i = 0 to d - 1 do
          Image.YUV420.set_pixel_rgba img i j (0xff,0,0,(i+j)*0xff/(2*d))
        done
      done;
      let bg = Image.YUV420.create d d in
      Image.YUV420.fill bg (0,0,0);
      Image.YUV420.add img ~x:50 ~y:50 bg;
      write "mgradient.bmp" (Image.YUV420.to_BMP bg)
    );
  test "color to alpha" (fun () ->
      let d = 500 in
      let img = Image.YUV420.create d d in
      Image.YUV420.fill img (0,0,0);
      let c = (0xff, 0xff, 20) in
      let r = Image.YUV420.create 200 200 in
      Image.YUV420.fill r c;
      Image.YUV420.add r ~x:100 ~y:150 img;
      Image.YUV420.alpha_of_color img c 5;
      write "c2a.bmp" (Image.YUV420.to_BMP img)
    )

let () =
  Gc.full_major ()
