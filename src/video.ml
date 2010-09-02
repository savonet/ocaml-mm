type frame = Image.RGBA8.t

type buffer = frame array

let size = Array.length

let append = Array.append

(* TODO: we don't want to fill it with useless frames, right? *)
let create len =
  let i = Image.RGBA8.create 0 0 in
  Array.make len i

module RE = struct
  type t = frame

  let create () = Image.RGBA8.create 0 0
end

module Ringbuffer_ext = Ringbuffer.Make_ext (RE)

module Ringbuffer = Ringbuffer.Make (RE)

module IO = struct
  exception Invalid_file

  class type reader =
  object
    method width : int

    method height : int

    method frame_rate : float

    method set_target_size : int -> int -> unit

    method read : buffer -> int -> int -> int

    (* method read_audio : Audio.buffer -> int -> int -> int *)

    method close : unit
  end

  class type writer =
  object
    method write : buffer -> int -> int -> unit

    method write_audio : Audio.buffer -> int -> int -> unit

    method close : unit
  end

  class virtual avi_writer frame_rate w h =
    let frames_per_chunk = frame_rate in
    let frame_size = w * h * 3 in
  object (self)
    inherit IO.helper

    method virtual stream_write : string -> int -> int -> int
    method virtual stream_seek : int -> unit
    method virtual stream_close : unit

    initializer
      self#output "RIFF";
      self#output_int 0; (* TOFILL: file size *)
      self#output "AVI "; (* file type *)
      (* Headers *)
      self#output "LIST";
      self#output_int 192; (* size of the list *)
      self#output "hdrl";
      (* AVI header *)
      self#output "avih";
      self#output_int 56; (* AVI header size *)
      self#output_int (1000000/frame_rate); (* microseconds per frame *)
      self#output_int 0; (* max bytes per sec *)
      self#output_int 0; (* unused (pad to multiples of this size) *)
      self#output_byte 0; (* flags *)
      self#output_byte 1; (* flags (interleaved) *)
      self#output_byte 0; (* flags *)
      self#output_byte 0; (* flags *)
      self#output_int 0; (* TOFILL: total number of frames *)
      self#output_int 0; (* initial frame *)
      self#output_int 1; (* number of streams (TODO: change if audio) *)
      self#output_int 0; (* suggested buffer size *)
      self#output_int w; (* width *)
      self#output_int h; (* height *)
      self#output_int 0; (* scale *)
      self#output_int 0; (* rate *)
      self#output_int 0; (* start *)
      self#output_int 0; (* length *)
      (* Stream headers *)
      self#output "LIST";
      self#output_int 116;
      self#output "strl";
      (* Stream header *)
      self#output "strh";
      self#output_int 56;
      self#output "vids";
      self#output "RGB "; (* codec *)
      self#output_int 0; (* flags *)
      self#output_int 0; (* stream priority and language *)
      self#output_int 0; (* initial frames *)
      self#output_int 1; (* scale : rate / scale frames / second or samples / second *)
      self#output_int frame_rate;
      self#output_int 0; (* stream start time (in frames). *)
      self#output_int 0; (* TOFILL: stream length (= number of frames) *)
      self#output_int (frames_per_chunk * frame_size); (* suggested buffer size *)
      self#output_int 0; (* stream quality *)
      self#output_int 0; (* size of samples *)
      self#output_int 0; (* destination rectangle: left *)
      self#output_int 0; (* top *)
      self#output_int w; (* right *)
      self#output_int h; (* bottom *)
      (* Stream format *)
      self#output "strf";
      self#output_int 40;
      self#output_int 40; (* video size (????) *)
      self#output_int w; (* width *)
      self#output_int h; (* height *)
      self#output_short 1; (* panes *)
      self#output_short 24; (* color depth *)
      self#output_int 0; (* tag1 (????) *)
      self#output_int frame_size; (* image size *)
      self#output_int 0; (* X pixels per meter *)
      self#output_int 0; (* Y pixels per meter *)
      self#output_int 0; (* colors used *)
      self#output_int 0; (* important colors *)

      (* movie data *)
      self#output "LIST";
      self#output_int 0; (* TOFILL: movie size *)
      self#output "movi"
      (* video chunks follow *)
  end
end
