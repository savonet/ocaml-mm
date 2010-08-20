let total_duration = 5

let () =
  let channels = 2 in
  let sample_rate = 44100 in
  let wav = Audio.IO.writer_to_wav_file channels sample_rate "out.wav" in
  let blen = 1024 in
  let buf = Audio.Mono.create blen in
  let sine = Audio.Mono.Generator.sine sample_rate 440. in
  for i = 0 to sample_rate / blen * total_duration - 1 do
    sine#fill buf 0 blen;
    wav#write [|buf; buf|] 0 blen
  done;
  wav#close

(*
let () =
  let channels = 2 in
  let sample_rate = 44100 in
  let oss = Audio.IO.OSS.writer channels sample_rate in
  let blen = 1024 in
  let buf = Audio.Mono.create blen in
  let sine = Audio.Mono.Generator.sine sample_rate 440. in
  while true do
    sine#fill buf 0 blen;
    oss#write [|buf; buf|] 0 blen
  done;
  oss#close
*)
