let sample_rate = 44100
let channels = 2

let sd freq v =
  let lpf = new Audio.Mono.Effect.biquad_filter sample_rate `Low_pass (freq*.2.) 1. in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0., 0.4, 0., 1.) in
  let g = new Audio.Mono.Generator.white_noise sample_rate in
  let g = new Audio.Mono.Generator.chain g lpf in
  let g = new Audio.Mono.Generator.adsr adsr g in
  let g = new Audio.Generator.of_mono g in
  g

let () =
  let oss = new MMOSS.writer channels sample_rate in
  (* let wav = new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav" in *)
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let mbuf = MIDI.create blen in
  let synth_sd = new Synth.create sd in
  let keybd = new MMSDL.midi_keyboard in
  while true do
    keybd#read sample_rate [|mbuf|] 0 blen;
    synth_sd#play mbuf 0 buf 0 blen;
    (* wav#write buf 0 blen; *)
    oss#write buf 0 blen
  done;
  (* wav#close; *)
  oss#close
