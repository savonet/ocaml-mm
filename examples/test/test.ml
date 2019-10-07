let () =
  let buflen = 1024 in
  let channels = 2 in
  let buf = Audio.create channels buflen in
  let s = Bytes.create (Audio.S16LE.size channels buflen) in
  ignore (Audio.S16LE.of_audio buf s 0)
