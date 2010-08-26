module FFT = Audio.Mono.Analyze.FFT

let () =
  let fname = Sys.argv.(1) in
  let f = MMExt.Audio.IO.Mad.reader_of_file fname in
  let oss = Audio.IO.OSS.writer f#channels f#sample_rate in
  let fft_n = 11 in
  let fft = FFT.init fft_n in
  let blen = FFT.duration fft in
  let bdur = float blen /. float f#sample_rate in
  let buf = Audio.create f#channels blen in
  let adsr = Audio.Mono.Effect.ADSR.make f#sample_rate (0.02,0.01,0.9,0.05) in
  let synth = Audio.Generator.Synth.saw f#sample_rate in
  let loop = ref true in
  synth#set_volume 0.;
  while !loop do
    let r = f#read buf 0 blen in
    loop := r <> 0;
    let c = FFT.complex_create (Audio.to_mono buf) 0 blen in
    FFT.Window.cosine c;
    FFT.fft fft c;
    let kmax = ref 0. in
    let vmax = ref 0. in
    for k = 1 to blen / 2 - 2 do
      (* Quadratic interpolation. *)
      let v' = Complex.norm c.(k-1) in
      let v = Complex.norm c.(k) in
      let v'' = Complex.norm c.(k-1) in
      let p = (v'' -. v') /. (2. *. v' -. 2. *. v +. v'') in
      let v = v -. (v' -. v'') *. p /. 4. in
      let p = p +. float k in
      if v > !vmax then
	(
	  kmax := p;
	  vmax := v
	);
    done;
    let freq = !kmax /. bdur in
    let note = Audio.Note.of_freq freq in
    let vmax = !vmax /. float (FFT.duration fft) in
    Printf.printf "Note: %s (%d, %.02fHz) at %.02f\n%!" (Audio.Note.to_string (Audio.Note.of_freq freq)) note freq vmax;
    (* Audio.clear buf 0 blen; *)
    if vmax > 0.01 && snd (Audio.Note.modulo note) > 2 then
      synth#note_on note 1.;
    synth#fill_add buf 0 blen;
    synth#note_off note 1.;
    oss#write buf 0 blen
  done;
  oss#close;
  f#close
