class type t =
object
  method set_volume : float -> unit

  method note_on : int -> float -> unit

  method note_off : int -> float -> unit

  method fill_add : Audio.buffer -> int -> int -> unit

  method play_add : MIDI.buffer -> Audio.buffer -> int -> int -> unit

  method play : MIDI.buffer -> Audio.buffer -> int -> int -> unit

  method reset : unit
end

type synth = t

type note =
    {
      note : int;
      volume : float;
      generator : Audio.Generator.t
    }

class virtual base =
object (self)
  method virtual private generator : float -> float -> Audio.Generator.t

  val mutable vol : float = 1.

  method set_volume v = vol <- v

  val mutable notes : note list = []

  method note_on n v =
    let note =
      {
	note = n;
	volume = v; (* TODO: we could want to change the volume after a not has begun to be played *)
	generator = self#generator (Audio.Note.freq n) (v *. vol);
      }
    in
    notes <- note :: notes

  method note_off n (v:float) =
    (* TODO: remove only one note *)
    (* TODO: merge the two iterations on the list *)
    List.iter (fun note -> if note.note = n then note.generator#release) notes;
    notes <- List.filter (fun note -> not note.generator#dead) notes

  method fill_add buf ofs len =
    List.iter (fun note -> note.generator#fill_add buf ofs len) notes;

  method private fill buf ofs len =
    Audio.clear buf ofs len;
    self#fill_add buf ofs len

  method private event = function
    | MIDI.Note_off (n,v) ->
      self#note_off n v
    | MIDI.Note_on (n,v) ->
      self#note_on n v
    | MIDI.Control_change (0x7,v) ->
      self#set_volume (float v /. 127.)
    | _ -> ()

  (* TODO: add offset for evs *)
  method play_add evs buf ofs len =
    let rec play o evs ofs =
      match evs with
        | (t,e)::_ when t >= len ->
          ()
        | (t,e)::tl ->
          let delta = t-o in
          self#fill_add buf ofs delta;
          self#event e;
          play t tl (ofs+delta)
        | [] ->
          self#fill_add buf ofs (len-o)
    in
    play 0 (MIDI.data evs) ofs

  method play evs buf ofs len =
    Audio.clear buf ofs len;
    self#play_add evs buf ofs len

  method reset = notes <- []
end

class create g =
object
  inherit base

  method private generator f v = g f v
end

class create_mono g =
object
  inherit create (fun f v -> Audio.Generator.of_mono (g f v))
end

let might_adsr adsr g =
  match adsr with
    | None -> g
    | Some a -> Audio.Mono.Generator.adsr a g

let simple_gen g ?adsr sr ?(volume=1.) ?(phase=0.) f =
    let g = g sr ?volume:(Some volume) ?phase:(Some phase) f in
    let g = might_adsr adsr g in
    Audio.Generator.of_mono g

let sine ?adsr sr = new create (fun f v -> simple_gen Audio.Mono.Generator.sine ?adsr sr ~volume:v f)

let square ?adsr sr = new create (fun f v -> simple_gen Audio.Mono.Generator.square ?adsr sr ~volume:v f)

let saw ?adsr sr = new create (fun f v -> simple_gen Audio.Mono.Generator.saw ?adsr sr ~volume:v f)

let monophonic (g:Audio.Generator.t) =
object (self)
  method set_volume v = g#set_volume v

  method note_on n v =
    g#set_frequency (Audio.Note.freq n);
    g#set_volume v

  method note_off (_:int) (_:float) =
	(* TODO: check for the last note? *)
    g#release

  method fill_add buf ofs len = g#fill_add buf ofs len

  (* TODO *)
  method play_add evs buf ofs len =
    assert false

  method play evs buf ofs len =
    assert false

  method reset = g#set_volume 0.
end

module Multitrack = struct
  class type t =
  object
    method play_add : MIDI.Multitrack.buffer -> Audio.buffer -> int -> int -> unit

    method play : MIDI.Multitrack.buffer -> Audio.buffer -> int -> int -> unit
  end

  class create n (f : int -> synth) =
  object (self)
    val synth = Array.init n f

    method play_add (evs:MIDI.Multitrack.buffer) buf ofs len =
      for c = 0 to Array.length synth - 1 do
        synth.(c)#play_add evs.(c) buf ofs len
      done

    method play evs buf ofs len =
      Audio.clear buf ofs len;
      self#play_add evs buf ofs len
  end
end
