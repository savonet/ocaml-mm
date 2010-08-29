module FFT = Audio.Mono.Analyze.FFT

let channels = 2
let sample_rate = 44100

let () =
  let oss_out = Audio.IO.OSS.writer channels sample_rate in
  let oss_in = Audio.IO.OSS.reader channels sample_rate in
  let fft = FFT.init 11 in
  let blen = FFT.duration fft in
  let buf = Audio.create channels blen in
  let agc = Audio.Effect.auto_gain_control channels sample_rate ~rms_target:2. () in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.02,0.01,0.9,0.05) in
  let synth = Audio.Generator.Synth.saw ~adsr sample_rate in
  let loop = ref true in
  let prevnote = ref (-1) in
  synth#set_volume 0.1;
  while !loop do
    let r = oss_in#read buf 0 blen in
    assert (r = blen);
    agc#process buf 0 blen;
    Printf.printf "RMS: %.05f\n%!" (Audio.Mono.Analyze.rms (Audio.to_mono buf) 0 blen);
    let note, vnote = FFT.note sample_rate fft ~note_min:(Audio.Note.create 0 4) (Audio.to_mono buf) 0 blen in
    let note = if vnote > 0.01 then note else -1 in
    if note <> !prevnote then
      (
	if !prevnote <> (-1) then
	  synth#note_off !prevnote 1.;
	if note <> (-1) then
	  synth#note_on note 1.
      );
    prevnote := note;
    (* synth#fill_add buf 0 blen; *)
    oss_out#write buf 0 blen
  done;
  oss_in#close;
  oss_out#close
