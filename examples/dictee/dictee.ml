module FFT = Audio.Mono.Analyze.FFT

let () =
  let fname = Sys.argv.(1) in
  let f = MMExt.Audio.IO.Mad.reader_of_file fname in
  let oss = Audio.IO.OSS.writer f#channels f#sample_rate in
  let wav = Audio.IO.writer_to_wav_file f#channels f#sample_rate "out.wav" in
  let fft_n = 11 in
  let fft = FFT.init fft_n in
  let blen = FFT.duration fft in
  let buf = Audio.create f#channels blen in
  let agc = Audio.Effect.auto_gain_control f#channels f#sample_rate ~rms_target:2. () in
  let adsr = Audio.Mono.Effect.ADSR.make f#sample_rate (0.02,0.01,0.9,0.05) in
  let synth = Audio.Generator.Synth.saw ~adsr f#sample_rate in
  let loop = ref true in
  let prevnote = ref (-1) in
  synth#set_volume 0.1;
  while !loop do
    let r = f#read buf 0 blen in
    agc#process buf 0 blen;
    loop := r <> 0;
    let note, vnote = FFT.note f#sample_rate fft ~note_min:(Audio.Note.create 0 4) (Audio.to_mono buf) 0 blen in
    (*
    let note =
      if (* vnote > 0.01 && *) Audio.Note.octave note > 3 then
	note
      else
	-1
    in
    *)
    let note = if vnote > 0.01 then note else -1 in
    if note <> !prevnote then
      (
	if !prevnote <> (-1) then
	  synth#note_off !prevnote 1.;
	if note <> (-1) then
	  synth#note_on note 1.
      );
    prevnote := note;
    synth#fill_add buf 0 blen;
    wav#write buf 0 blen;
    oss#write buf 0 blen
  done;
  wav#close;
  oss#close;
  f#close
