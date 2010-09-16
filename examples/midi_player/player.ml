let () =
  let fname = Sys.argv.(1) in
  let f = new MIDI.IO.Reader.of_file fname in
  let channels = 2 in
  let sample_rate = 44100 in
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav" in
  let oss = new MMOSS.writer channels sample_rate in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let mchannels = 16 in
  let mbuf = MIDI.Multitrack.create mchannels blen in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = new Synth.Multitrack.create mchannels (fun _ -> new Synth.saw ~adsr sample_rate) in
  let agc = Audio.Effect.auto_gain_control channels sample_rate ~volume_init:0.5 () in
  let r = ref (-1) in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 1));
  while !r <> 0 do
    r := f#read sample_rate mbuf 0 blen;
    MIDI.Multitrack.clear ~channel:9 mbuf 0 blen;
    synth#play mbuf 0 buf 0 blen;
    (* Audio.amplify 0.5 buf 0 blen; *)
    agc#process buf 0 blen;
    wav#write buf 0 blen;
    oss#write buf 0 blen
  done;
  wav#close;
  oss#close;
  f#close
