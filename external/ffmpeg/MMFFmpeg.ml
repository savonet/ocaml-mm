(* Register codecs. *)
module FFmpeg = struct
  external init : unit -> unit = "caml_ffmpeg_init"
  type t
  external openfile : string -> t = "caml_ffmpeg_openfile"
  external width : t -> int = "caml_ffmpeg_width"
  external height : t -> int = "caml_ffmpeg_height"
  exception End_of_stream
  let () = Callback.register_exception "ffmpeg_exn_end_of_stream" End_of_stream
  external read_frame : t -> string = "caml_ffmpeg_read_frame"
  (* TODO *)
  let close _ = ()
end

class reader_of_file fname =
  (* TODO: we should do this only once *)
  let () = FFmpeg.init () in
  let ff = FFmpeg.openfile fname in
  let width = FFmpeg.width ff in
  let height = FFmpeg.height ff in
object (self)
  method frame_rate = 0.

  method width = width

  method height = height

  method read_frame =
    let buf = FFmpeg.read_frame ff in
    Image.RGBA8.of_RGB8_string buf width

  method read buf ofs len =
    let n = ref 0 in
    try
      while !n < len do
        buf.(ofs + !n) <- self#read_frame;
        incr n
      done;
      !n
    with
      | FFmpeg.End_of_stream -> !n

  method close = FFmpeg.close ff

  method seek (n:int) : unit = assert false
end

let reader_of_file fname =
  (new reader_of_file fname :> Video.IO.reader)
