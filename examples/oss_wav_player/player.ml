let () =
  let fname = Sys.argv.(1) in
  let f = Audio.IO.reader_of_wav_file fname in
  let flen = f#duration in
  let oss = Audio.IO.OSS.writer f#channels f#sample_rate in
  let blen = 1024 in
  let buf = Audio.create f#channels blen in
  let read = ref 0 in
  Printf.printf "Opened WAV file with %d channels at %dHz.\n%!" f#channels f#sample_rate;
  let delay = Audio.Effect.delay f#channels f#sample_rate 0.2 ~ping_pong:true 0.5 in
  let bqf = Audio.Effect.biquad_filter f#channels f#sample_rate `High_pass 400. 1. in
  while !read <> flen do
    let n = min blen (flen - !read) in
    ignore (f#read buf 0 n);
    read := !read + n;
    (* delay#process buf 0 n; *)
    (* bqf#process buf 0 n; *)
    oss#write buf 0 n
  done;
  oss#close;
  f#close
