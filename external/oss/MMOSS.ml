module OSS = struct
  external set_format : Unix.file_descr -> int -> int = "caml_oss_dsp_setfmt"

  external set_channels : Unix.file_descr -> int -> int = "caml_oss_dsp_channels"

  external set_rate : Unix.file_descr -> int -> int = "caml_oss_dsp_speed"
end

(* TODO: other formats than 16 bits? *)
class writer ?(device="/dev/dsp") channels sample_rate =
object (self)
  inherit IO.Unix.rw ~write:true device

  initializer
    assert (OSS.set_format fd 16 = 16);
    assert (OSS.set_channels fd channels = channels);
    assert (OSS.set_rate fd sample_rate = sample_rate)

  method private stream_really_write buf ofs len =
    let w = ref 0 in
    while !w <> len do
      w := !w + self#stream_write buf (ofs + !w) (len - !w)
    done

  method write buf ofs len =
    let s = Audio.S16LE.make buf ofs len in
    self#stream_really_write s 0 (String.length s)

  method close =
    self#stream_close
end

class reader ?(device="/dev/dsp") channels sample_rate =
object (self)
  inherit IO.Unix.rw ~read:true device

  initializer
    assert (OSS.set_format fd 16 = 16);
    assert (OSS.set_channels fd channels = channels);
    assert (OSS.set_rate fd sample_rate = sample_rate)

  method channels = channels
  method sample_rate = sample_rate

  method duration : int = assert false
  method duration_time : float = assert false

  method read buf ofs len =
    let slen = Audio.S16LE.length channels len in
    let s = String.create slen in
    let r = self#stream_read s 0 slen in
    let len = Audio.S16LE.duration channels r in
    Audio.S16LE.to_audio s 0 buf ofs len;
    len

  method seek (n:int) : unit = assert false

  method close =
    self#stream_close
end
