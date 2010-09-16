(** Operations on synthesizers. *)

(** A synthesizer. *)
class type t =
object
  (** Set the global volume of the synth. *)
  method set_volume : float -> unit

  (** Play a note. *)
  method note_on : int -> float -> unit

  (** Stop playing a note. *)
  method note_off : int -> float -> unit

  (** Fill a buffer with synthesized data adding to the original data of the buffer. *)
  method fill_add : Audio.buffer -> int -> int -> unit

  (** Synthesize into an audio buffer. Notice that the delta times in the track
      should be in samples (so they do depend on the samplerate). *)
  method play : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit

  (** Same as [play] but keeps data originally present in the buffer. *)
  method play_add : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit

  (** Reset the synthesizer (sets all notes off in particular). *)
  method reset : unit
end

(** A synthesizer. *)
type synth = t

(** Create a synthesizer from a function which creates a generator at given
    frequency and volume. *)
class create : (float -> float -> Audio.Generator.t) -> t

(** Same as [create] with a mono generator. *)
class create_mono : (float -> float -> Audio.Mono.Generator.t) -> t

(** Sine synthesizer. *)
class sine : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Square synthesizer. *)
class square : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Saw synthesizer. *)
class saw : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Synths with only one note at a time. *)
class monophonic : Audio.Generator.t -> t

(** Multichannel synthesizers. *)
module Multitrack : sig
  (** A multichannel synthesizer. *)
  class type t =
  object
    (** Synthesize into an audio buffer. *)
    method play : MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit

    (** Same as [play] but keeps data originally present in the buffer. *)
    method play_add : MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit
  end

  (** Create a multichannel synthesizer with given number of channels and a
      function returning the synthesizer on each channel. *)
  class create : int -> (int -> synth) -> t
end
