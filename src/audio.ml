(* TODO:
   - lots of functions require offset and length whereas in most cases we
   want to apply the operations on the whole buffers -> labeled optional
   arguments?
   - do we want to pass samplerate as an argument or to store it in buffers? *)

let pi = 3.14159265358979323846

let lin_of_dB x = 10. ** (x /. 20.)

let dB_of_lin x = 20. *. log x /. log 10.

let samples_of_seconds sr t =
  int_of_float (float sr *. t)

let freq_of_note n = 440. *. (2. ** ((float n -. 69.) /. 12.))

module Sample = struct
  type t = float

  let clip x =
    let x = max (-.1.) x in
    let x = min 1. x in
    x
end

module Mono = struct
  type buffer = float array

  let create n = Array.make n 0.

  let duration buf = Array.length buf

  let clear b ofs len = Array.fill b ofs len 0.

  (* let blit b1 o1 b2 o2 len = Array.blit b1 o1 b2 o2 len *)
  external blit : float array -> int -> float array -> int -> int -> unit = "caml_float_array_blit"

  let add b1 o1 b2 o2 len=
    for i = 0 to len - 1 do
      b1.(o1 + i) <- b1.(o1 + i) +. b2.(o2 + i)
    done

  let add_coeff b1 o1 k b2 o2 len =
    for i = 0 to len - 1 do
      b1.(o1 + i) <- b1.(o1 + i) +. k *. b2.(o2 + i)
    done

  let amplify k b ofs len =
    for i = ofs to ofs + len - 1 do
      b.(i) <- k *. b.(i)
    done

  let randomize b ofs len =
    for i = ofs to ofs + len - 1 do
      b.(i) <- Random.float 2. -. 1.
    done

  let clip buf ofs len =
    for i = ofs to ofs + len - 1 do
      buf.(i) <- Sample.clip buf.(i)
    done

  let resample ratio inbuf offs len =
    if ratio = 1. then
      let outbuf = create len in
      blit inbuf offs outbuf 0 len;
      outbuf
    else
      let outlen = int_of_float (float len *. ratio) in
      let outbuf = create outlen in
      for i = 0 to outlen - 1 do
	let inidx = min (int_of_float (float i /. ratio)) (len - 1) in
	outbuf.(i) <- inbuf.(inidx + offs)
      done;
      outbuf

  (* TODO: refined allocation/deallocation policies *)
  module Extensible_buffer = struct
    type t =
	{
	  mutable buffer : buffer
	}

    let prepare buf len =
      if duration buf.buffer >= len then
	buf.buffer
      else
	(* TODO: optionally blit the old buffer onto the new one. *)
	(* let oldbuf = buf.buffer in *)
	let newbuf = create len in
	buf.buffer <- newbuf;
	newbuf

    let create len =
      {
	buffer = create len
      }

    let duration buf = duration buf.buffer
  end

  module Analyze = struct
    let rms b ofs len =
      let r = ref 0. in
      for i = ofs to ofs + len - 1 do
	let x = b.(i) in
	r := !r +. x *. x
      done;
      sqrt (!r /. float len)

    module FFT = struct
      type t =
	  {
	    n : int; (* number of samples *)
	    circle : Complex.t array;
	    temp : Complex.t array;
	  }

      let init n =
	let h = (1 lsl n) / 2 in
	let fh = float h in
	let circle = Array.make h Complex.zero in
	for i = 0 to h - 1 do
	  let theta = pi *. float_of_int i /. fh in
	  circle.(i) <- {Complex.re = cos theta; Complex.im = sin theta}
    	done;
	{
	  n = n;
	  circle = circle;
	  temp = Array.make (1 lsl n) Complex.zero;
	}

      let complex_create buf ofs len =
	Array.init len (fun i -> {Complex.re = buf.(ofs + i); Complex.im = 0.})

      let ccoef k c =
	{Complex.re = k *. c.Complex.re; Complex.im = k *. c.Complex.im}

      let fft f d =
	(* TODO: greater should be ok too? *)
	assert (Array.length d = 1 lsl f.n);
	let ( +~ ) = Complex.add in
	let ( -~ ) = Complex.sub in
	let ( *~ ) = Complex.mul in
	let rec fft d s n =
	  if (n > 1) then
            let h = n / 2 in
            for i = 0 to h - 1 do
              f.temp.(s + i) <- d.(s + 2 * i);          (* even *)
              f.temp.(s + h + i) <- d.(s + 2 * i + 1)   (* odd  *)
	    done;
            fft d s h;
            fft d (s + h) h;
            let a = (1 lsl f.n) / n in
            for i = 0 to h - 1 do
              let wkt = f.circle.(i * a) *~ f.temp.(s + h + i) in
              d.(s + i) <- f.temp.(s + i) +~ wkt ;
              d.(s + h + i) <- f.temp.(s + i) -~ wkt
            done
	in
	fft d 0 (1 lsl f.n)

      let ifft f d =
	let n = float f.n in
	let normc c = {Complex.re = c.Complex.re /. n; Complex.im = (-.c.Complex.im) /. n} in
	for i = 0 to Array.length d - 1 do
	  d.(i) <- Complex.conj d.(i)
	done;
	fft f d;
	for i = 0 to Array.length d - 1 do
	  d.(i) <- normc d.(i)
	done

      (* See http://en.wikipedia.org/wiki/Window_function *)
      module Window = struct
	let iter f d =
	  let len = Array.length d in
	  let n = float len in
	  for i = 0 to len - 1 do
	    let k = f (float i) n in
	    d.(i) <- ccoef k d.(i)
	  done

	let iteri f d =
	  let len = Array.length d in
	  let n = float len in
	  for i = 0 to len - 1 do
	    let k = f i n in
	    d.(i) <- ccoef k d.(i)
	  done

	let hann d = iter (fun i n -> 0.5 *. (1. -. cos (2. *. pi *. i /. n))) d

	let hamming d = iter (fun i n -> 0.54 *. (0.46 *. cos (2. *. pi *. i /. n))) d

	let cosine d = iter (fun i n -> sin (pi *. i /. n)) d

	let lanczos d =
	  let sinc x =
	    let px = pi *. x in
	    (sin px) /. px
	  in
	  iter (fun i n -> sinc (2. *. i /. n)) d

	let triangular d =
	  iter
	    (fun i n ->
	      if i <= n /. 2. then
		2. *. i /. n
	      else
		(n /. 2. -. i) *. 2. /. n
	    ) d

	let bartlett_hann d =
	  let a0 = 0.62 in
	  let a1 = 0.48 in
	  let a2 = 0.38 in
	  iter (fun i n -> a0 -. a1 *. abs_float (i /. n -. 0.5) -. a2 *. cos (2. *. pi *. i /. n)) d

	let blackman ?(alpha=0.16) f d =
	  let a = alpha in
	  let a0 = (1. -. a) /. 2. in
	  let a1 = 1. /. 2. in
	  let a2 = a /. 2. in
	  iter (fun i n -> a0 -. a1 *. cos (2. *. pi *. i /. n) +. a2 *. cos (4. *. pi *. i /. n)) d
	  (* iteri (fun i n -> a0 -. a1 *. circle.(i) *)

	(* TODO: use circle to compute cosines *)
	let low_res a0 a1 a2 a3 f d =
	  iter (fun i n -> a0 -. a1 *. cos (2. *. pi *. i /. n) +. a2 *. cos (4. *. pi *. i /. n) -. a3 *. cos (6. *. pi *. i /. n)) d

	let nuttall f d = low_res 0.355768 0.487396 0.144232 0.012604 f d

	let blackman_harris f d = low_res 0.35875 0.48829 0.14128 0.01168 f d

	let blackman_nuttall f d = low_res 0.3635819 0.4891775 0.1365995 0.0106411 f d
      end
    end
  end

  module Effect = struct
    let compand_mu_law mu buf ofs len =
      for i = ofs to ofs + len - 1 do
	let bufi = buf.(i) in
        let sign = if bufi < 0. then -1. else 1. in
        buf.(i) <- sign *. log (1. +. mu  *. abs_float bufi) /. log (1. +. mu)
      done

    module ADSR = struct
      type t = int * int * float * int

      (** Convert adsr in seconds to samples. *)
      let make sr (a,d,s,r) =
	samples_of_seconds sr a,
	samples_of_seconds sr d,
	s,
	samples_of_seconds sr r

      (** State in the ADSR enveloppe (A/D/S/R/dead + position in the state). *)
      type state = int * int

      let init () = 0, 0

      let release (_,p) = (3,p)

      let dead (s,_) = s = 4

      let rec process adsr st buf ofs len =
	let a,(d:int),s,(r:int) = adsr in
	let state, state_pos = st in
	match state with
          | 0 ->
            let fa = float a in
            for i = 0 to min len (a - state_pos) - 1 do
              buf.(ofs + i) <- float (state_pos + i) /. fa *. buf.(ofs + i)
            done;
            if len < a - state_pos then
              0, state_pos + len
            else
              process adsr (1,0) buf (ofs + a - state_pos) (len - (a - state_pos))
          | 1 ->
            let fd = float d in
            for i = 0 to min len (d - state_pos) - 1 do
              buf.(ofs + i) <- (1. -. float (state_pos + i) /. fd *. (1. -. s)) *. buf.(ofs + i)
            done;
            if len < d - state_pos then
              1, state_pos + len
            else
              process adsr (2,0) buf (ofs + d - state_pos) (len - (d - state_pos))
          | 2 ->
            amplify s buf ofs len;
            st
          | 3 ->
            let fr = float r in
            for i = 0 to min len (r - state_pos) - 1 do
              buf.(ofs + i) <- s *. (1. -. float (state_pos + i) /. fr) *. buf.(ofs + i)
            done;
            if len < r - state_pos then
              3, state_pos + len
            else
              process adsr (4,0) buf (ofs + r - state_pos) (len - (r - state_pos))
          | 4 ->
            clear buf ofs len;
            st
          | _ -> assert false
    end
  end

  module Generator = struct
    class type t =
    object
      method set_volume : float -> unit

      method fill : buffer -> int -> int -> unit

      method fill_add : buffer -> int -> int -> unit

      method release : unit

      method dead : bool
    end

    class virtual base sample_rate volume =
    object (self)
      val mutable vol = volume

      val mutable dead = false

      method dead = dead

      method release =
	vol <- 0.;
	dead <- true

      method sample_rate : int = sample_rate

      method volume : float = vol

      method set_volume v = vol <- v

      method virtual fill : buffer -> int -> int -> unit

      (* TODO: might be optimized by various synths *)
      method fill_add buf ofs len =
	let tmp = create len in
	self#fill tmp 0 len;
	add buf ofs tmp 0 len
    end

    class virtual simple f phase freq =
    object (self)
      val mutable phase = phase

      method virtual sample_rate : int

      method fill (buf:buffer) ofs len =
	let sr = float self#sample_rate in
	let omega = freq /. sr in
	for i = 0 to len - 1 do
	  buf.(ofs + i) <- f (float i *. omega +. phase)
	done;
	phase <- mod_float (phase +. float len *. omega) 1.
    end

    let simple f sr ?(volume=1.) ?(phase=0.) freq =
      (object
	inherit base sr volume
	inherit simple f phase freq
       end:>t)

    (* TODO: ensure that these functions get inlined!!! *)
    let sine = simple (fun t -> sin (2. *. pi *. t))

    let square = simple (fun t -> let t = fst (modf t) in if t < 0.5 then 1. else -1.)

    let saw =
      simple
	(fun t ->
	  let t = fst (modf t) in
          if t < 0.5 then
            4. *. t -. 1.
          else
            4. *. (1. -. t) -. 1.
	)

    let adsr (adsr:Effect.ADSR.t) (g:t) =
    object (self)
      val mutable adsr_st = Effect.ADSR.init ()

      val tmpbuf = Extensible_buffer.create 0

      method set_volume = g#set_volume

      method fill buf ofs len =
	g#fill buf ofs len;
	adsr_st <- Effect.ADSR.process adsr adsr_st buf ofs len

      method fill_add buf ofs len =
	let tmpbuf = Extensible_buffer.prepare tmpbuf len in
	self#fill tmpbuf 0 len;
	blit tmpbuf 0 buf ofs len

      method release =
	adsr_st <- Effect.ADSR.release adsr_st;
	g#release

      method dead =
	Effect.ADSR.dead adsr_st || g#dead
    end
  end
end

(** An audio buffer. *)
type buffer = Mono.buffer array

(** Length of the buffer in samples. *)
let length b = Array.length b

(** Iterate a function on each channel of the buffer. *)
let iter f b = Array.iter f b

(** Iterate a function on a portion of the buffer. *)
let iterp f b ofs len = Array.iter (fun b -> f b ofs len) b

let iter2 f b1 b2 =
  for c = 0 to Array.length b1 - 1 do
    f b1.(c) b2.(c)
  done

let map f b = Array.map f b

let create chans n =
  Array.init chans (fun _ -> Mono.create n)

let channels buf =
  Array.length buf

let duration buf =
  Mono.duration buf.(0)

let create_same buf =
  create (channels buf) (duration buf)

let clear = iterp Mono.clear

let clip = iterp Mono.clip

let blit b1 o1 b2 o2 len =
  iter2 (fun b1 b2 -> Mono.blit b1 o1 b2 o2 len) b1 b2

let to_mono b =
  let channels = channels b in
  let len = duration b in
  let chans = float channels in
  let ans = Mono.create len in
  for i = 0 to len - 1 do
    for c = 0 to channels - 1 do
      ans.(i) <- ans.(i) +. b.(c).(i)
    done;
    ans.(i) <- ans.(i) /. chans
  done;
  ans

let of_mono b = [|b|]

let resample ratio buf ofs len =
  map (fun buf -> Mono.resample ratio buf ofs len) buf

(*
let to_16le buf ofs sbuf sofs len =
  let chans = channels buf in
  assert (String.length sbuf >= sofs + chans * len * 2);
  for i = 0 to len - 1 do
    for c = 0 to chans - 1 do
      let s = Sample.clip buf.(c).(i + ofs) *. 32767. in
      let s = int_of_float s in
      let s = if s < 0 then (-s) lxor 65535 else s in
      let s1 = s mod 256 in
      let s2 = (s / 256) mod 256 in
      (* Printf.printf "%d -> %d-%d\n%!" s s1 s2; *)
      let s1 = char_of_int s1 in
      let s2 = char_of_int s2 in
      sbuf.[sofs + 2 * (i * chans + c)] <- s1;
      sbuf.[sofs + 2 * (i * chans + c) + 1] <- s2
    done
  done
*)
external to_16le : float array array -> int -> int -> string -> int -> int = "caml_float_pcm_to_16le"

let to_16le_create buf ofs len =
  let slen = channels buf * len * 2 in
  let sbuf = String.create slen in
  ignore (to_16le buf ofs len sbuf 0);
  sbuf

(*
let of_16le sbuf sofs buf ofs len =
  let chans = channels buf in
  for i = 0 to len - 1 do
    for c = 0 to chans - 1 do
      let s1 = int_of_char sbuf.[sofs + 4 * i + 2 * c] in
      let s2 = int_of_char sbuf.[sofs + 4 * i + 2 * c + 1] in
      let s = s1 + s2 * 256 in
      let s = if s >= 32768 then - (s lxor 65535) else s in
      let s = float s /. 32768. in
      buf.(c).(i + ofs) <- s
    done
  done
*)
external of_16le : string -> int -> float array array -> int -> int -> unit = "caml_float_pcm_from_16le"

let add b1 o1 b2 o2 len = iter2 (fun b1 b2 -> Mono.add b1 o1 b2 o2 len) b1 b2

let add_coeff b1 o1 k b2 o2 len = iter2 (fun b1 b2 -> Mono.add_coeff b1 o1 k b2 o2 len) b1 b2

let amplify k buf ofs len =
  iter (fun buf -> Mono.amplify k buf ofs len) buf

(* x between -1 and 1 *)
let pan x buf ofs len =
  assert (channels buf = 2);
  (* Field width in degrees. *)
  let field = 90. in
  (* Degrees to radians + half field. *)
  let phi0 = field *. pi /. 360. in
  (* Map -1 / 1 to radians. *)
  let phi = x *. phi0 in
  let gain_left = ((tan phi0) +. tan phi) /. 2. in
  let gain_right = ((tan phi0) -. tan phi) /. 2. in
  for i = ofs to ofs + len - 1 do
    buf.(0).(i) <- buf.(0).(i) *. gain_left;
    buf.(1).(i) <- buf.(1).(i) *. gain_right
  done

module Ringbuffer = struct
  type t = {
    size : int;
    buffer : buffer;
    mutable rpos : int; (** current read position *)
    mutable wpos : int  (** current write position *)
  }

  let create chans size =
    {
      (* size + 1 so we can store full buffers, while keeping
	 rpos and wpos different for implementation matters *)
      size = size + 1 ;
      buffer = create chans (size + 1);
      rpos = 0;
      wpos = 0
    }

  let read_space t =
    if t.wpos >= t.rpos then (t.wpos - t.rpos)
    else t.size - (t.rpos - t.wpos)

  let write_space t =
    if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
    else (t.rpos - t.wpos) - 1

  let read_advance t n =
    assert (n <= read_space t);
    if t.rpos + n < t.size then t.rpos <- t.rpos + n
    else t.rpos <- t.rpos + n - t.size

  let write_advance t n =
    assert (n <= write_space t);
    if t.wpos + n < t.size then t.wpos <- t.wpos + n
    else t.wpos <- t.wpos + n - t.size

  let peek t buff off len =
    assert (len <= read_space t);
    let pre = t.size - t.rpos in
    let extra = len - pre in
    if extra > 0 then
      (
	blit t.buffer t.rpos buff off pre;
	blit t.buffer 0 buff (off + pre) extra
      )
    else
      blit t.buffer t.rpos buff off len

  let read t buff off len =
    peek t buff off len;
    read_advance t len

  let write t buff off len =
    assert (len <= write_space t);
    let pre = t.size - t.wpos in
    let extra = len - pre in
    if extra > 0 then
      (
        blit buff off t.buffer t.wpos pre;
        blit buff (off + pre) t.buffer 0 extra
      )
    else
      blit buff off t.buffer t.wpos len;
    write_advance t len

  let transmit t f =
    if t.wpos = t.rpos then 0 else
      let len0 =
	if t.wpos >= t.rpos then t.wpos - t.rpos
	else t.size - t.rpos
      in
      let len = f t.buffer t.rpos len0 in
      assert (len <= len0);
      read_advance t len;
      len
end

module Effect = struct
  class type t =
  object
    method process : buffer -> int -> int -> unit
  end

  class virtual base sample_rate =
  object
    method sample_rate : int = sample_rate
  end

  class virtual bufferized chans len =
  object
    val rb = Ringbuffer.create chans len

    val tmpbuf = create chans len

    method read_space = Ringbuffer.read_space rb

    method read_advance = Ringbuffer.read_advance rb

    method peek len =
      Ringbuffer.peek rb tmpbuf 0 len;
      tmpbuf

    method read len =
      Ringbuffer.read rb tmpbuf 0 len;
      tmpbuf

    method write buf ofs len =
      Ringbuffer.write rb buf ofs len
  end

  (* delay d in samples *)
  (* TODO: adaptative buflen *)
  class delay chans buflen d once feedback =
  object (self)
    inherit bufferized chans (d + buflen)

    method process buf ofs len =
      if once then
	self#write buf ofs len;
      (* Make sure that we have a past of exactly d samples. *)
      if self#read_space < d then
	self#write (create chans d) 0 d;
      if self#read_space > d then
	self#read_advance (self#read_space - d);
      if len > d then
	add_coeff buf (ofs + d) feedback buf ofs (len - d);
      let tmp = self#read (min d len) in
      add_coeff buf ofs feedback tmp 0 (min d len);
      if not once then
	self#write buf ofs len
  end

  (* delay in seconds *)
  let delay ~buffer_length channels sample_rate d ?(once=false) feedback =
    let d = int_of_float (float sample_rate *. d) in
    ((new delay channels buffer_length d once feedback):>t)

  (*
  module ADSR = struct
    type t = Mono.Effect.ADSR.t

    type state = Mono.Effect.ADSR.state
  end
  *)
end

(* TODO: we cannot share this with mono, right? *)
module Extensible_buffer = struct
  type t =
      {
        mutable buffer : buffer
      }

  let prepare buf len =
    if duration buf.buffer >= len then
      buf.buffer
    else
      (* TODO: optionally blit the old buffer onto the new one. *)
      let oldbuf = buf.buffer in
      let newbuf = create (channels oldbuf) len in
      buf.buffer <- newbuf;
      newbuf

  let create chans len =
    {
      buffer = create chans len
    }

  let duration buf = duration buf.buffer
end

module Generator = struct
  class type t =
  object
    method set_volume : float -> unit

    method release : unit

    method dead : bool

    method fill : buffer -> int -> int -> unit

    method fill_add : buffer -> int -> int -> unit
  end

  let of_mono g =
  object
    val tmpbuf = Mono.Extensible_buffer.create 0

    method set_volume = g#set_volume

    method fill buf ofs len =
      g#fill buf.(0) ofs len;
      for c = 1 to channels buf - 1 do
	Mono.blit buf.(c) ofs buf.(0) ofs len
      done

    method fill_add buf ofs len =
      let tmpbuf = Mono.Extensible_buffer.prepare tmpbuf len in
      g#fill tmpbuf 0 len;
      for c = 0 to Array.length buf - 1 do
	Mono.add buf.(c) ofs tmpbuf 0 len
      done

    method release = g#release

    method dead = g#dead
  end

  class type generator = t

  module Synth = struct
    class type t =
    object
      method set_volume : float -> unit

      method note_on : int -> float -> unit

      method note_off : int -> float -> unit

      method fill_add : buffer -> int -> int -> unit

      method reset : unit
    end

    type note =
	{
	  note : int;
	  volume : float;
	  generator : generator
	}

    class virtual base volume =
    object (self)
      method virtual generator : generator

      val mutable vol : float = volume

      method set_volume v = vol <- v

      val mutable notes : note list = []

      method note_on n v =
	let note =
	  {
	    note = n;
	    volume = v;
	    generator = self#generator;
	  }
	in
	notes <- note :: notes

      method note_off n (v:float) =
	(* TODO: remove only one note *)
	(* TODO: merge the two iterations on the list *)
	List.iter (fun note -> if note.note = n then note.generator#release) notes;
	notes <- List.filter (fun note -> not note.generator#dead) notes

      method fill_add buf ofs len =
	List.iter (fun note -> note.generator#fill buf ofs len) notes

      method fill buf ofs len =
	clear buf ofs len;
	self#fill_add buf ofs len
    end
  end
end

module IO = struct
  exception Invalid_file

  exception Invalid_operation

  exception End_of_stream

  class type reader =
  object
    method channels : int

    method sample_rate : int

    method duration : int

    method duration_time : float

    method seek : int -> unit

    method close : unit

    method read : buffer -> int -> int -> int
  end

  class virtual reader_base =
  object (self)
    method virtual channels : int

    method virtual sample_rate : int

    method virtual duration : int

    method duration_time =
      float self#duration /. float self#sample_rate

(*
    method virtual seek : int -> unit

    method virtual close : unit

    method virtual read : buffer -> int -> int -> int
*)
  end

  (** To be inherited to read from files. *)
  class virtual rw_unix ?(read=false) ?(write=false) fname =
  object (self)
    val fd =
      let flag, perms =
	match read, write with
	  | false, false -> assert false
	  | true, false -> [Unix.O_RDONLY], 0o644
	  | false, true -> [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC], 0o644
	  | true, true -> [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC], 0o644
      in
      Unix.openfile fname flag perms

    method stream_read buf ofs len = Unix.read fd buf ofs len

    method stream_write buf ofs len = Unix.write fd buf ofs len

    method stream_close = Unix.close fd

    method stream_seek n =
      ignore (Unix.lseek fd n Unix.SEEK_SET)

    method stream_cur_pos =
      Unix.lseek fd 0 Unix.SEEK_CUR
  end

  (* TODO: handle more formats... *)
  class virtual wav_reader =
  object (self)
    method virtual stream_read : string -> int -> int -> int
    method virtual stream_close : unit
    method virtual stream_seek : int -> unit
    method virtual stream_cur_pos : int

    val mutable sample_rate = 0
    val mutable channels = 0
    (** Size of a sample in bits. *)
    val mutable sample_size = 0
    val mutable bytes_per_sample = 0
    (** Duration in samples. *)
    val mutable duration = 0
    val mutable data_offset = 0

    method sample_rate = sample_rate
    method channels = channels
    method duration = duration

    method input n =
      let buf = String.create n in
      let n = self#stream_read buf 0 n in
      buf, n

    method loop_input len =
      let s, i = self#input len in
      if i = len || i = 0 then
	s, i
      else
	(* if i = 0 then s raise End_of_stream else *)
	let s', i' = self#loop_input (len - i) in
        String.sub s 0 i ^ s', (i + i')

    method really_input len = fst (self#loop_input len)

    method input_byte =
      let s, i = self#input 1 in
      if i = 0 then raise End_of_stream;
      int_of_char s.[0]

    method read_int_num_bytes n =
      let rec aux = function
	| 0 -> 0
	| n ->
          let b = self#input_byte in
          b + 256 * (aux (n-1))
      in
      aux n

    method read_int = self#read_int_num_bytes 4

    method read_short = self#read_int_num_bytes 2

    method read_byte = self#read_int_num_bytes 1

    initializer
      if self#really_input 4 <> "RIFF" then
	(* failwith "Bad header: \"RIFF\" not found"; *)
	raise Invalid_file;
      (* Ignore the file size *)
      ignore (self#really_input 4) ;
      if self#really_input 8 <> "WAVEfmt " then
	(* failwith "Bad header: \"WAVEfmt \" not found"; *)
	raise Invalid_file;
      (* Now we always have the following uninteresting bytes:
       * 0x10 0x00 0x00 0x00 0x01 0x00 *)
      ignore (self#really_input 6);
      channels <- self#read_short;
      sample_rate <- self#read_int;
      (* byt_per_sec *) ignore (self#read_int);
      (* byt_per_samp *) ignore (self#read_short);
      sample_size <- self#read_short;

      (* TODO: handle this *)
      (* signed *) assert (sample_size <> 8);

      let section = self#really_input 4 in
      if section <> "data" then
	(
          if section = "INFO" then
            (* failwith "Valid wav file but unread"; *)
	    raise Invalid_file;
          (* failwith "Bad header : string \"data\" not found" *)
	  raise Invalid_file
	);

      let len_dat = self#read_int in
      data_offset <- self#stream_cur_pos;
      bytes_per_sample <- sample_size / 8 * channels;
      duration <- len_dat / bytes_per_sample

    method read_sample =
      if sample_size = 8 then
	(* TODO! *)
	float (self#read_byte) /. 128.
      else
	let s = self#read_short in
	let s = if s >= 32768 then - (s lxor 65535) else s in
	float s /. 32768.

    method read buf ofs len =
      let sbuflen = len * channels * 2 in
      let sbuf, sbuflen = self#loop_input sbuflen in
      let len = sbuflen / (channels * 2) in
      of_16le sbuf 0 buf ofs len;
      len

    method seek n =
      let n = data_offset + n * bytes_per_sample in
      self#stream_seek n

    method close = self#stream_close
  end

  let reader_of_wav_file fname =
  (object
    inherit rw_unix ~read:true fname
    inherit reader_base
    inherit wav_reader
  end :> reader)

  class type writer =
  object
    method write : buffer -> int -> int -> unit

    method close : unit
  end

  class virtual writer_base chans sr =
  object
    method channels : int = chans

    method sample_rate : int = sr
  end

  class virtual wav_writer =
  object (self)
    method virtual stream_write : string -> int -> int -> int
    method virtual stream_seek : int -> unit
    method virtual stream_close : unit
    method virtual channels : int
    method virtual sample_rate : int

    method output s =
      let len = String.length s in
      assert (self#stream_write s 0 len = len)

    method output_num b n =
      let s = String.create b in
      for i = 0 to b - 1 do
	s.[i] <- char_of_int ((n lsr (8 * i)) land 0xff)
      done;
      self#output s
    method output_short n = self#output_num 2 n
    method output_int n = self#output_num 4 n

    initializer
    let bits_per_sample = 16 in
    (* RIFF *)
    self#output "RIFF";
    self#output_int 0;
    self#output "WAVE";
    (* Format *)
    self#output "fmt ";
    self#output_int 16;
    self#output_short 1;
    self#output_short self#channels;
    self#output_int self#sample_rate;
    self#output_int (self#sample_rate * self#channels * bits_per_sample / 8);
    self#output_short (self#channels * bits_per_sample / 8);
    self#output_short bits_per_sample;
    (* Data *)
    self#output "data";
    self#output_int 0 (* size of the data, to be updated afterwards *)

    val mutable datalen = 0

    method write buf ofs len =
      let s = to_16le_create buf ofs len in
      self#output s;
      datalen <- datalen + String.length s

    method close =
      self#stream_seek 4;
      self#output_int (36 + datalen);
      self#stream_seek 40;
      self#output_int datalen;
      self#stream_close
  end

  let writer_to_wav_file chans sr fname =
    (object
      inherit writer_base chans sr
      inherit rw_unix ~write:true fname
      inherit wav_writer
     end :> writer)

  module OSS = struct
    external set_format : Unix.file_descr -> int -> int = "caml_oss_dsp_setfmt"

    external set_channels : Unix.file_descr -> int -> int = "caml_oss_dsp_channels"

    external set_rate : Unix.file_descr -> int -> int = "caml_oss_dsp_speed"

    (* TODO: other formats than 16 bits? *)
    let writer ?(device="/dev/dsp") channels sample_rate =
      (object (self)
	inherit rw_unix ~write:true device

	initializer
	  assert (set_format fd 16 = 16);
	  assert (set_channels fd channels = channels);
	  assert (set_rate fd sample_rate = sample_rate)

	method stream_really_write buf ofs len =
	  let w = ref 0 in
	  while !w <> len do
	    w := !w + self#stream_write buf (ofs + !w) (len - !w)
	  done

	method write buf ofs len =
	  let s = to_16le_create buf ofs len in
	  self#stream_really_write s 0 (String.length s)

	method close =
	  self#stream_close
       end :> writer)
  end
end
