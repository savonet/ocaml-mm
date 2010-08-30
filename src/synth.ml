class type t =
object
  method set_volume : float -> unit

  method note_on : int -> float -> unit

  method note_off : int -> float -> unit

  method fill_add : Audio.buffer -> int -> int -> unit

  method play_add : MIDI.Track.t -> Audio.buffer -> int -> int -> unit

  method play : MIDI.Track.t -> Audio.buffer -> int -> int -> unit

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
  method virtual generator : float -> float -> Audio.Generator.t

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

  method fill buf ofs len =
    Audio.clear buf ofs len;
    self#fill_add buf ofs len

  method event = function
    | MIDI.Note_off (n,v) ->
      self#note_off n v
    | MIDI.Note_on (n,v) ->
      self#note_on n v
    | MIDI.Control_change (0x7,v) ->
      self#set_volume (float v /. 127.)
    | _ -> ()

  method play_add evs buf ofs len =
    match evs with
      | (t,e)::tl ->
        assert (t < len);
        self#fill_add buf ofs t;
        self#event e;
        self#play_add tl buf (ofs+t) (len-t)
      | [] ->
        self#fill_add buf ofs len

  method play evs buf ofs len =
    Audio.clear buf ofs len;
    self#play_add evs buf ofs len

  method reset = notes <- []
end

let create g =
  (object
    inherit base

    method generator f v = g f v
   end :> t)

let create_mono g = create (fun f v -> Audio.Generator.of_mono (g f v))

let might_adsr adsr g =
  match adsr with
    | None -> g
    | Some a -> Audio.Mono.Generator.adsr a g

let simple_gen g ?adsr sr ?(volume=1.) ?(phase=0.) f =
    let g = g sr ?volume:(Some volume) ?phase:(Some phase) f in
    let g = might_adsr adsr g in
    Audio.Generator.of_mono g

let sine ?adsr sr = create (fun f v -> simple_gen Audio.Mono.Generator.sine ?adsr sr ~volume:v f)

let square ?adsr sr = create (fun f v -> simple_gen Audio.Mono.Generator.square ?adsr sr ~volume:v f)

let saw ?adsr sr = create (fun f v -> simple_gen Audio.Mono.Generator.saw ?adsr sr ~volume:v f)

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

module Multichan = struct
  class type t =
  object
    method play_add : MIDI.buffer -> Audio.buffer -> int -> int -> unit

    method play : MIDI.buffer -> Audio.buffer -> int -> int -> unit
  end

  let create n f =
  object (self)
    val synth = Array.init n f

    method play_add evs buf ofs len =
      for c = 0 to Array.length synth - 1 do
        synth.(c)#play_add evs.(c) buf ofs len
      done

    method play evs buf ofs len =
      Audio.clear buf ofs len;
      self#play_add evs buf ofs len
  end
end
