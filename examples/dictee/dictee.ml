let () =
  let fname = Sys.argv.(1) in
  let f = MMExt.Audio.IO.Mad.reader_of_file fname in
  let oss = Audio.IO.OSS.writer f#channels f#sample_rate in
  let blen = 1024 in
  let buf = Audio.create f#channels blen in
  let loop = ref true in
  while !loop do
    let r = f#read buf 0 blen in
    loop := r <> 0;
    oss#write buf 0 r
  done;
  oss#close;
  f#close
