let () =
  let fname = Sys.argv.(1) in
  let f = MIDI.IO.reader_of_file fname in
  let channels = 2 in
  let sample_rate = 44100 in
  let wav = Audio.IO.writer_to_wav_file channels sample_rate "out.wav" in
  let oss = Audio.IO.OSS.writer channels sample_rate in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let mchannels = 16 in
  let mbuf = Array.create mchannels [] in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = MIDI.Synth.Multichan.init mchannels (fun _ -> Audio.Generator.Synth.saw ~adsr sample_rate) in
  try
    while true do
      f#read_samples sample_rate mbuf blen;
      mbuf.(9) <- [];
      MIDI.Synth.Multichan.fill synth mbuf buf 0 blen;
      Audio.amplify 0.5 buf 0 blen;
      wav#write buf 0 blen;
      oss#write buf 0 blen
    done
  with
    | MIDI.IO.End_of_stream ->
      wav#close;
      oss#close;
      f#close
