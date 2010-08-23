(** Operations on MIDI data. *)

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

(** Delta-time (difference of time with the preceding event). *)
type delta = int

module Track : sig
  type t = (delta * event) list

  val create : unit -> t

  val append : t -> t -> t
end

module Synth : sig
  class type t =
  object
    method feed : Track.t -> unit

    method fill : Audio.buffer -> int -> int -> unit

    method fill_add : Audio.buffer -> int -> int -> unit
  end

  val create : Audio.Generator.Synth.t -> t
end

module IO : sig
  class type reader =
  object
    method read_samples : int -> Track.t array -> int -> unit

    method close : unit
  end

  val reader_of_file : string -> reader
end
