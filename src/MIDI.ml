type division =
  | Ticks_per_quarter of int
  | SMPTE of int * int

type event =
  | Note_on of Audio.Note.t * float (* Note on: note number (A4 = 69), velocity (between 0 and 1). *)
  | Note_off of Audio.Note.t * float
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
      (samplerate * delta) / (fps * res)

(*
let delta_of_samples samplerate division tempo samples =
  match division with
    | Ticks_per_quarter tpq ->
      let tpq = Int64.of_int tpq in
      let tempo = Int64.of_int tempo in
      let samplerate = Int64.of_int samplerate in
      let ten = Int64.of_int 1000000 in
      let samples = Int64.of_int samples in
      let ( * ) = Int64.mul in
      let ( / ) = Int64.div in
      Int64.to_int (samples * samplerate * ten * tpq / tempo)
    | SMPTE (fps,res) ->
      (* TODO: could this overflow? *)
      (fps * res * samples) / samplerate
*)

module Track = struct
  type t = (delta * event) list

  let create () = []

  let append t1 t2 : t = List.append t1 t2
end

type buffer = Track.t array

module Synth = struct
  let rec fill_add synth evs buf ofs len =
    match evs with
      | (t,e)::tl ->
	assert (t < len);
	synth#fill_add buf ofs t;
	(
	  match e with
	    | Note_on (n,v) ->
	      synth#note_on n v
	    | Note_off (n,v) ->
	      synth#note_off n v
	    | Control_change (0x7,v) ->
              synth#set_volume (float v /. 127.)
	    | _ -> ()
	);
	fill_add synth tl buf (ofs+t) (len-t)
      | [] ->
	synth#fill_add buf ofs len

  let fill synth evs buf ofs len =
    Audio.clear buf ofs len;
    fill_add synth evs buf ofs len

  module Multichan = struct
    type t = Audio.Generator.Synth.t array

    let init n f = Array.init n f

    let fill_add synth evs buf ofs len =
      for c = 0 to Array.length synth - 1 do
	fill_add synth.(c) evs.(c) buf ofs len
      done

    let fill synth evs buf ofs len =
      Audio.clear buf ofs len;
      fill_add synth evs buf ofs len
  end
end

module IO = struct
  exception Invalid_header

  exception Invalid_data

  class type reader =
  object
    method read_samples : int -> Track.t array -> int -> int

    method close : unit
  end

  class virtual base =
  object (self)
    inherit IO.helper

    val mutable tracks = 0
    val mutable division = Ticks_per_quarter 0

    method input_id = self#really_input 4
    method input_int = self#input_int_be
    method input_short = self#input_short_be

    (** Read midi header. *)
    method read_header =
      (* Actual header reading. *)
      let id = self#input_id in
      let len = self#input_int in
      let fmt = self#input_short in
      let track_nb = self#input_short in
      let div = self#input_short in
      let div =
	if div land 0x8000 = 0 then
            (* Delta-time ticks per quarter *)
          Ticks_per_quarter div
	else
	  let frames = (div lsr 8) land 0x7f in
	  let ticks = div land 0xff in
          SMPTE (frames, ticks)
      in
      if id <> "MThd" || len <> 6 || (fmt <> 0 && fmt <> 1 && fmt <> 2) then
        raise Invalid_header;
      tracks <- track_nb;
      division <- div

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

  class file_reader fname =
  object (self)
    inherit IO.Unix.rw ~read:true fname
    inherit IO.helper
    inherit base

    val mutable track = []
    val mutable tempo = 500000

    initializer
      (* Read header. *)
      self#read_header;
      (* Read all tracks. *)
      let tracks = Array.init tracks (fun _ -> self#read_track) in
      (* Merge all tracks. *)
      let trk =
	let find_min () =
          let ans = ref None in
          for c = 0 to Array.length tracks - 1 do
            match tracks.(c) with
              | [] -> ()
              | (d,_)::_ ->
                match !ans with
                  | None ->
                    ans := Some (d, c)
                  | Some (d',_) ->
                    if d < d' then ans := Some (d, c)
          done;
          match !ans with
            | Some (d, c) -> d,c
            | None -> raise Not_found
	in
	let ans = ref [] in
        try
          while true do
            let d,c = find_min () in
            ans := (List.hd tracks.(c)) :: !ans;
            tracks.(c) <- List.tl tracks.(c);
            Array.iteri
              (fun n t ->
                if n <> c && t <> [] then
                  let d',e = List.hd t in
                  tracks.(n) <- (d'-d,e)::(List.tl t)
              ) tracks
          done;
          assert false
        with
          | Not_found -> List.rev !ans
      in
      track <- trk

    (* We store here the track with delta-times in samples. TODO: this way of
       doing things is messy but simpler to implement *)
    val mutable track_samples = []
    val mutable track_samples_computed = false

    method read_samples sr buf len =
      (* Compute track_samples if this has not been done yet. *)
      if not track_samples_computed then
	(
	  let t = tempo in
	  track_samples <-
	    List.map
	    (fun (d,(c,e)) ->
	      let d = samples_of_delta sr division tempo d in
	      (
		match e with
		  | Tempo t -> tempo <- t
		  | _ -> ()
	      );
	      (d,(c,e))
	    )
	    track;
	  tempo <- t;
	  track_samples_computed <- true
	);
      let offset_in_buf = ref 0 in
      (* Clear the input buffer. *)
      for c = 0 to Array.length buf - 1 do
	buf.(c) <- []
      done;
      while track_samples <> [] && !offset_in_buf < len do
        let d,(c,e) = List.hd track_samples in
        offset_in_buf := !offset_in_buf + d;
	(
          match e with
            | Tempo t -> tempo <- t
            | _ -> ()
        );
        if !offset_in_buf < len then
	  (
            track_samples <- List.tl track_samples;
            match c with
	      | Some c ->
                (* Filter out relevant events. *)
		(
                  match e with
                    | Note_on _
                    | Note_off _
                    | Control_change _ ->
		      if c < Array.length buf then
			buf.(c) <- (buf.(c))@[d, e]
                    | _ -> () (* TODO *)
                )
	      | None -> () (* TODO *)
          )
	else
          track_samples <- (!offset_in_buf - len,(c,e))::(List.tl track_samples)
      done;
      if track_samples <> [] then len else !offset_in_buf

    method close = self#stream_close
  end

  let reader_of_file fname = (new file_reader fname :> reader)
end
