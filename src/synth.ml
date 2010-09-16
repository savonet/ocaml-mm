class type t =
object
  method set_volume : float -> unit

  method note_on : int -> float -> unit

  method note_off : int -> float -> unit

  method fill_add : Audio.buffer -> int -> int -> unit

  method play_add : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit

  method play : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit

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
  method play_add evs eofs buf ofs len =
    let rec play o evs ofs =
      match evs with
        | (t,_)::_ when t >= eofs + len ->
          ()
        | (t,_)::tl when t < eofs ->
          play t tl ofs
        | (t,e)::tl ->
          let delta = t-(max eofs o) in
          self#fill_add buf ofs delta;
          self#event e;
          play t tl (ofs+delta)
        | [] ->
          self#fill_add buf ofs (len-o)
    in
    play 0 (MIDI.data evs) ofs

  method play evs eofs buf ofs len =
    Audio.clear buf ofs len;
    self#play_add evs eofs buf ofs len

  method reset = notes <- []
end

class create g =
object
  inherit base

  method private generator f v = g f v
end

class create_mono g =
  create (fun f v -> new Audio.Generator.of_mono (g f v))

let might_adsr adsr g =
  match adsr with
    | None -> g
    | Some a -> new Audio.Mono.Generator.adsr a g

let simple_gen g ?adsr sr ?(volume=1.) ?(phase=0.) f =
    let g = g sr ?volume:(Some volume) ?phase:(Some phase) f in
    let g = might_adsr adsr g in
    new Audio.Generator.of_mono g

class sine ?adsr sr =
  create_mono
    (fun f v ->
      might_adsr adsr
        (new Audio.Mono.Generator.sine sr ~volume:v f))

class square ?adsr sr =
  create_mono
    (fun f v ->
      might_adsr adsr
        (new Audio.Mono.Generator.square sr ~volume:v f))

class saw ?adsr sr =
  create_mono
    (fun f v ->
      might_adsr adsr
        (new Audio.Mono.Generator.saw sr ~volume:v f))

class monophonic (g:Audio.Generator.t) =
object (self)
  method set_volume v = g#set_volume v

  method note_on n v =
    g#set_frequency (Audio.Note.freq n);
    g#set_volume v

  method note_off (_:int) (_:float) =
    (* TODO: check for the last note? *)
    g#release

  method fill_add buf ofs len =
    g#fill_add buf ofs len

  (* TODO *)
  method play_add (evs:MIDI.buffer) (eofs:int) (buf:Audio.buffer) (ofs:int) (len:int) : unit =
    assert false

  method play evs eofs buf ofs len : unit =
    self#play_add evs eofs buf ofs len;
    assert false

  method reset = g#set_volume 0.
end

module Multitrack = struct
  class type t =
  object
    method play_add : MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit

    method play : MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit
  end

  class create n (f : int -> synth) =
  object (self)
    val synth = Array.init n f

    method play_add (evs:MIDI.Multitrack.buffer) eofs buf ofs len =
      for c = 0 to Array.length synth - 1 do
        synth.(c)#play_add evs.(c) eofs buf ofs len
      done

    method play evs eofs buf ofs len =
      Audio.clear buf ofs len;
      self#play_add evs eofs buf ofs len
  end
end
