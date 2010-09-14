(* unit argument because we might put optional arguments for parameters *)
class writer () =
object (self)
  val dev = Ao.open_live ()

  method write buf ofs len =
    let s = Audio.S16LE.make buf ofs len in
    Ao.play dev s

  method close =
    Ao.close dev
end
