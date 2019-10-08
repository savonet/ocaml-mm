let approx eps buf buf' =
  let ans = ref true in
  for c = 0 to Audio.channels buf - 1 do
    for i = 0 to Audio.length buf - 1 do
      ans := !ans && abs_float (buf.(c).{i} -. buf'.(c).{i}) < eps;
      if not !ans then Printf.printf "%f vs %f\n%!" buf.(c).{i} buf'.(c).{i};
    done
  done;
  !ans

let () =
  let buflen = 1024 in
  let channels = 2 in
  let buf = Audio.create channels buflen in
  let buf' = Audio.copy buf in
  Audio.noise buf;
  (* S16LE *)
  let s = Bytes.create (Audio.S16LE.size channels buflen) in
  Audio.S16LE.of_audio buf s 0;
  Audio.S16LE.to_audio (Bytes.unsafe_to_string s) 0 buf';
  assert (approx 0.0001 buf buf');
  (* S16BE *)
  (* TODO: negative numbers get messed up, we have to think about sign bit and endianness. *)
  (* let s = Bytes.create (Audio.S16BE.size channels buflen) in *)
  (* Audio.S16BE.of_audio buf s 0; *)
  (* Audio.S16BE.to_audio (Bytes.unsafe_to_string s) 0 buf'; *)
  (* assert (approx 0.0001 buf buf'); *)
  (* U8 *)
  let s = Bytes.create (Audio.U8.size channels buflen) in
  Audio.U8.of_audio buf s 0;
  Audio.U8.to_audio (Bytes.unsafe_to_string s) 0 buf';
  assert (approx 0.01 buf buf')
