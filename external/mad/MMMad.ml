(* TODO: use optimized version for files. *)
class virtual reader =
object (self)
  inherit IO.helper

  method virtual private stream_close : unit

  val mutable channels = 0
  method channels = channels
  (* TODO *)
  method duration : int = raise Not_found
  method duration_time : float = raise Not_found
  method sample_rate = 44100

  val mutable rb = Audio.Ringbuffer_ext.create 0 0

  val mutable mf = None

  method private mf =
    match mf with Some mf -> mf | _ -> assert false

  initializer
  let f =
    Mad.openstream
      (fun n ->
        let s = String.create n in
        let n = self#stream_read s 0 n in
        s, n)
  in
  let _, c, _ = Mad.get_output_format f in
  (* TODO: we should decode a frame in order to get the real number of
     channels... *)
  let c = 2 in
  mf <- Some f;
  channels <- c;
  rb <- Audio.Ringbuffer_ext.create channels 0

  method private decode = Mad.decode_frame_float self#mf

  method close = self#stream_close

  method read buf ofs len =
    let r = ref (-1) in
    while !r <> 0 && Audio.Ringbuffer_ext.read_space rb < len do
      let data =
        try
          self#decode
        with
          | Mad.End_of_stream -> Audio.create (self#channels) 0
      in
      r := Audio.duration data;
      Audio.Ringbuffer_ext.write rb data 0 !r
    done;
    let maxlen = Audio.Ringbuffer_ext.read_space rb in
    let len = min maxlen len in
    Audio.Ringbuffer_ext.read rb buf ofs len;
    len

  (* TODO *)
  method seek (n:int) : unit = assert false
end

class reader_of_file fname =
object (self)
  inherit IO.Unix.rw ~read:true fname
  inherit reader
end
