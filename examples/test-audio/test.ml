(* Testing audio float/int conversions *)
let () =
  let approx eps buf buf' =
    let ans = ref true in
    for c = 0 to Audio.channels buf - 1 do
      for i = 0 to Audio.length buf - 1 do
        ans := !ans && abs_float (buf.(c).{i} -. buf'.(c).{i}) < eps;
        if not !ans then Printf.printf "%f vs %f\n%!" buf.(c).{i} buf'.(c).{i};
      done
    done;
    !ans
  in
  let buflen = 1024 in
  let channels = 2 in
  let buf = Audio.create channels buflen in
  let buf' = Audio.copy buf in
  Audio.noise buf;
  (* S16LE *)
  print_endline "Testing S16LE.";
  let s = Bytes.create (Audio.S16LE.size channels buflen) in
  Audio.S16LE.of_audio buf s 0;
  Audio.S16LE.to_audio (Bytes.unsafe_to_string s) 0 buf';
  assert (approx 0.0001 buf buf');
  (* S16BE *)
  (* TODO: negative numbers get messed up, we have to think about sign bit and endianness. *)
  (* print_endline "Testing S16BE." *)
  (* let s = Bytes.create (Audio.S16BE.size channels buflen) in *)
  (* Audio.S16BE.of_audio buf s 0; *)
  (* Audio.S16BE.to_audio (Bytes.unsafe_to_string s) 0 buf'; *)
  (* assert (approx 0.0001 buf buf'); *)
  (* U8 *)
  print_endline "Testing U8.";
  let s = Bytes.create (Audio.U8.size channels buflen) in
  Audio.U8.of_audio buf s 0;
  Audio.U8.to_audio (Bytes.unsafe_to_string s) 0 buf';
  assert (approx 0.01 buf buf');
  (* S24LE *)
  print_endline "Testing S24LE.";
  let s = Bytes.create (Audio.S24LE.size channels buflen) in
  Audio.S24LE.of_audio buf s 0;
  Audio.S24LE.to_audio (Bytes.unsafe_to_string s) 0 buf';
  assert (approx 0.000001 buf buf');
  (* S32LE *)
  print_endline "Testing S32LE.";
  let s = Bytes.create (Audio.S32LE.size channels buflen) in
  Audio.S32LE.of_audio buf s 0;
  Audio.S32LE.to_audio (Bytes.unsafe_to_string s) 0 buf';
  assert (approx 0.000000001 buf buf')

let () =
  print_endline "Testing buffers.";
  let channels = 2 in
  let buflen = 1024 in
  let buf = Audio.create channels buflen in
  let buf' = Audio.create channels buflen in
  Audio.noise buf;
  Audio.blit (Audio.sub buf 0 (buflen/2)) (Audio.sub buf' 0 (buflen/2));
  Audio.blit (Audio.sub buf (buflen/2) (buflen/2)) (Audio.sub buf' (buflen/2) (buflen/2));
  assert (buf = buf');
  Audio.add buf buf';
  Audio.amplify 2. buf;
  Audio.clip buf
