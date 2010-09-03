(* Register codecs. *)
(* TODO: for now the size of the output frames have to match the size of the
   vid. *)
module FFmpeg = struct
  external init : unit -> unit = "caml_ffmpeg_init"

  exception End_of_stream
  let () = Callback.register_exception "ffmpeg_exn_end_of_stream" End_of_stream

  module Decoder = struct
    type t

    external openfile : string -> t = "caml_ffmpeg_dec_openfile"

    external dump_format : t -> string -> unit = "caml_ffmpeg_dec_dump_format"

    external width : t -> int = "caml_ffmpeg_dec_width"

    external height : t -> int = "caml_ffmpeg_dec_height"

    external read_frame : t -> Video.frame -> unit = "caml_ffmpeg_dec_read_frame"

    external close : t -> unit = "caml_ffmpeg_dec_close"

    external set_target_size : t -> int -> int -> unit = "caml_ffmpeg_dec_set_target_size"

    external frame_rate : t -> float = "caml_ffmpeg_dec_fps"
  end

  module Encoder = struct
    type t

    external openfile : string -> int * int -> int -> int -> int -> t = "caml_ffmpeg_enc_openfile"

    external dump_format : t -> string -> unit = "caml_ffmpeg_enc_dump_format"

    external write_frame : t -> Image.RGBA8.t -> unit = "caml_ffmpeg_enc_write_frame"

    external close : t -> unit = "caml_ffmpeg_enc_close"
  end

  (*
  module Scale = struct
    type t

    external create : int * int -> int * int -> t = "caml_sws_create"

    external scale_to : t -> Video.frame -> Video.frame -> unit = "caml_sws_scale_to"
  end
  *)
end

module D = FFmpeg.Decoder
module E = FFmpeg.Encoder

class reader_of_file fname =
  (* TODO: we should do this only once *)
  let () = FFmpeg.init () in
  let ff = D.openfile fname in
  let () = D.dump_format ff fname in
  let width = D.width ff in
  let height = D.height ff in
object (self)
  method frame_rate = D.frame_rate ff

  method width = width

  method height = height

  method set_target_size (w:int) (h:int) : unit =
    (* Not working yet *)
    assert false
    (* FFmpeg.set_target_size ff w h *)

  method read_frame =
    let img = Image.RGBA8.create width height in
    D.read_frame ff img;
    img

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

  method close = D.close ff

  method seek (n:int) : unit = assert false
end

let reader_of_file fname =
  (new reader_of_file fname :> Video.IO.reader)

class writer_to_file fname fr w h =
  let fr = Video.FPS.to_frac fr in
  (* TODO: parameter for bitrate *)
  let br = 800000 in
  let enc = E.openfile fname fr w h br in
  let () = E.dump_format enc fname in
object(self)
  method write buf ofs len =
    for i = ofs to ofs + len - 1 do
      E.write_frame enc buf.(i)
    done

  method close =
    E.close enc
end

let writer_to_file fname fr w h =
  (new writer_to_file fname fr w h :> Video.IO.writer)
