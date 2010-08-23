type division =
  | Ticks_per_quarter of int
  | SMPTE of int * int

type event =
  | Note_off of int * float
  | Note_on of int * float (** Note on: note number (A4 = 69), velocity (between 0 and 1). *)
  | Aftertouch of int * float
  | Control_change of int * int
  | Patch of int
  | Channel_aftertouch of int
  | Pitch of int
  (* Meta-events *)
  | Sequence_number of int
  | Text of string
  | Copyright of string
  | Track_name of string
  | Instrument_name of string
  | Lyric of string
  | Marker of string
  | Cue of string
  | End_of_track
  | Tempo of int
  | Time_signature of int * int * int * int
  | Key_signature of int * bool
  | Custom of string

type delta = int

(* Tempo is in microseconds per quarter. *)
let samples_of_delta samplerate division tempo delta =
  match division with
    | Ticks_per_quarter tpq ->
      (* These computations sometimes overflow on 32 bits. *)
      let tpq = Int64.of_int tpq in
      let tempo = Int64.of_int tempo in
      let tps = Int64.of_int samplerate in
      let ten = Int64.of_int 1000000 in
      let delta = Int64.of_int delta in
      let ( * ) = Int64.mul in
      let ( / ) = Int64.div in
      Int64.to_int ((((delta * tempo) / tpq) * tps) / ten)
    | SMPTE (fps,res) ->
      assert false (* TODO *)
      (* (delta * Lazy.force Frame.size) / (fps * res) *)

module Track = struct
  type t = (delta * event) list

  let create () = []

  let append t1 t2 : t = List.append t1 t2
end

module Synth = struct
  class type t =
  object
    method feed : Track.t -> unit

    method fill : Audio.buffer -> int -> int -> unit

    method fill_add : Audio.buffer -> int -> int -> unit
  end

  class base (synth:Audio.Generator.Synth.t) =
  object (self)
    val mutable midi = Track.create ()

    method feed tr =
      midi <- Track.append midi tr

    method fill_add buf ofs len =
      (* TODO!!! *)
      synth#fill_add buf ofs len

    method fill buf ofs len =
      Audio.clear buf ofs len;
      self#fill_add buf ofs len
  end

  let create s = (new base s :> t)
end

module IO = struct
  exception Invalid_header

  exception Invalid_data

  class virtual base =
  object (self)
    inherit IO.helper

    method input_id = self#really_input 4

    (** Read midi header. *)
    method read_header =
      (* Actual header reading. *)
      let id = self#input_id in
      let len = self#input_int in
      let fmt = self#input_short in
      let tracks = self#input_short in
      let division = self#input_short in
      let division =
	if division land 0x8000 = 0 then
            (* Delta-time ticks per quarter *)
          Ticks_per_quarter division
	else
	  let frames = (division lsr 8) land 0x7f in
	  let ticks = division land 0xff in
          SMPTE (frames, ticks)
      in
      if id <> "MThd" || len <> 6 || (fmt <> 0 && fmt <> 1 && fmt <> 2) then
        raise Invalid_header;
      tracks, division

    (** Read a midi track. *)
    method decode_track_data data =
      let len = String.length data in
      let data = Array.init len (fun i -> int_of_char data.[i]) in
      let pos = ref 0 in
      let read_delta () =
	let ans = ref 0 in
	while data.(!pos) land 0x80 <> 0 do
          ans := !ans lsl 7 + (data.(!pos) land 0x7f);
          incr pos;
          if !pos >= Array.length data then raise Invalid_data
	done;
	ans := !ans lsl 7 + data.(!pos);
	incr pos;
	!ans
      in
      let status = ref 0 in (* for running status *)
      let read_event () =
	let get_byte () =
	  if !pos >= Array.length data then raise Invalid_data;
	  incr pos;
	  data.(!pos - 1)
	in
	let get_text len =
	  let ans = String.create len in
          if !pos + len >= Array.length data then raise Invalid_data;
          for i = 0 to len - 1 do
            ans.[i] <- char_of_int data.(!pos + i)
          done;
          pos := !pos + len;
          ans
	in
	let advance len =
	  pos := !pos + len
	in
	let command =
	  if !pos >= Array.length data then raise Invalid_data;
	  data.(!pos)
	in
	incr pos;
	let command =
	  if command land 0x80 <> 0 then
            (
              status := command;
              command
            )
	  else
            (
              decr pos;
              !status
            )
	in
	let cmd = (command lsr 4) land 0xf in
	let chan = command land 0xf in
	match cmd with
          | 8 ->
            let n = get_byte () in
            let v = get_byte () in
            Some chan, Note_off (n, float v /. 127.)
          | 9 ->
            let n = get_byte () in
            let v = get_byte () in
            Some chan,
            if v = 0 then
                (* I have seen notes at 0. used as note off...... *)
              Note_off (n, 0.)
            else
              Note_on (n, float v /. 127.)
          | 0xa ->
            let n = get_byte () in
            let v = get_byte () in
            Some chan, Aftertouch (n, float v /. 127.)
          | 0xb ->
            let c = get_byte () in
            let v = get_byte () in
            Some chan, Control_change (c, v)
          | 0xc ->
            let p = get_byte () in
            Some chan, Patch p
          | 0xd ->
            let c = get_byte () in
            Some chan, Channel_aftertouch c
          | 0xe ->
            let l = get_byte () land 0x7f in
            let h = get_byte () land 0x7f in
            Some chan, Pitch ((h lsl 7) + l)
          | _ ->
            match command with
              | 0xf0
              | 0xf7 ->
                  (* SysEx *)
                let len = read_delta () in
                advance len;
                raise Not_found
              | 0xff ->
                (
                  let cmd = get_byte () in
                  let len = read_delta () in
                  match cmd with
                    | 0 ->
                      if len <> 2 then raise Invalid_data;
                      let h = get_byte () in
                      let l = get_byte () in
                      None, Sequence_number ((h lsl 8) + l)
                    | 1 ->
                      None, Text (get_text len)
                    | 2 ->
                      None, Copyright (get_text len)
                    | 3 ->
                      None, Track_name (get_text len)
                    | 4 ->
                      None, Instrument_name (get_text len)
                    | 5 ->
                      None, Lyric (get_text len)
                    | 6 ->
                      None, Marker (get_text len)
                    | 7 ->
                      None, Cue (get_text len)
                    | 0x2f (* End of track *) ->
                      if len <> 0 then raise Invalid_data;
                      raise Not_found
                    | 0x51 (* Tempo in microseconds per quarter note *) ->
                      if len <> 3 then raise Invalid_data;
                      let t1 = get_byte () in
                      let t2 = get_byte () in
                      let t3 = get_byte () in
                      let t = t1 lsl 16 + t2 lsl 8 + t3 in
                      None, Tempo t
                    | 0x58 (* Time signature *) ->
                      if len <> 4 then raise Invalid_data;
                          (* numerator,
                           * denominator,
                           * ticks in a metronome click,
                           * 32nd notes to the quarter note *)
                      let n = get_byte () in
                      let d = get_byte () in
                      let c = get_byte () in
                      let b = get_byte () in
                      None, Time_signature (n, d, c, b)
                    | 0x59 (* Key signature *) ->
                      if len <> 2 then raise Invalid_data;
                      let sf = get_byte () in (* sharps / flats *)
                      let m = get_byte () in (* minor? *)
                      None, Key_signature (sf, m <> 0)
                    | 0x54 (* SMPTE Offset *)
                    | 0x7f (* Sequencer-specific data *) ->
                      advance len;
                      raise Not_found
                    | _ ->
                      advance len;
		      Printf.printf "MIDI: unknown meta-event %x.\n%!" cmd;
                      raise Not_found
                )
              | _ ->
                advance 1;
		Printf.printf "MIDI: unknown command %x (pos: %d)\n%!" command !pos;
                raise Not_found
      in
      let ans = ref [] in
      while !pos < len do
	try
          let d = read_delta () in
          let e = read_event () in
          ans := (d, e)::!ans
	with
          | Not_found -> ()
      done;
      List.rev !ans

    method read_track =
      let id = self#input_id in
      let len = self#input_int in
      if id <> "MTrk" then raise Invalid_header;
      let data = self#really_input len in
      self#decode_track_data data
  end
end
