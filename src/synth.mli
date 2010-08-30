(** A synthesizer. *)
class type t =
object
  (** Set the global volume of the synth. *)
  method set_volume : float -> unit

  (** Play a note. *)
  method note_on : int -> float -> unit

  (** Stop playing a note. *)
  method note_off : int -> float -> unit

  method fill_add : Audio.buffer -> int -> int -> unit

  (* The deltas are stored in samples. *)
  method play_add : MIDI.Track.t -> Audio.buffer -> int -> int -> unit

  method play : MIDI.Track.t -> Audio.buffer -> int -> int -> unit

  method reset : unit
end

type synth = t

val create : (float -> float -> Audio.Generator.t) -> t

val create_mono : (float -> float -> Audio.Mono.Generator.t) -> t

val sine : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

val square : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

val saw : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Synths with only one note at a time. *)
val monophonic : Audio.Generator.t -> t

module Multichan : sig
  class type t =
  object
    method play_add : MIDI.buffer -> Audio.buffer -> int -> int -> unit

    method play : MIDI.buffer -> Audio.buffer -> int -> int -> unit
  end

  val create : int -> (int -> synth) -> t
end
