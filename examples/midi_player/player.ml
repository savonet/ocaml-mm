let () =
  let fname = Sys.argv.(1) in
  let f = MIDI.IO.reader_of_file fname in
  let channels = 2 in
  let sample_rate = 44100 in
  let oss = Audio.IO.OSS.writer channels sample_rate in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let mbuf = Array.create 16 [] in
  let synth = MIDI.Synth.create (Audio.Generator.Synth.sine sample_rate) in
  while true do
    f#read_samples sample_rate mbuf blen;
    synth#feed mbuf.(2);
    synth#fill buf 0 blen;
    oss#write buf 0 blen
  done;
  oss#close;
  f#close
