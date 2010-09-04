let () =
  let fname = Sys.argv.(1) in
  let t = new MMTheora.reader_of_file fname in
  t#close
