(** Operations on MIDI data. *)

type division =
  | Ticks_per_quarter of int
  | SMPTE of int * int

type event =
  | Note_off of Audio.Note.t * float
  | Note_on of Audio.Note.t * float
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

type buffer = Track.t array

module IO : sig
  module Reader : sig
    class type t =
    object
      method read_samples : int -> Track.t array -> int -> int

      method close : unit
    end

    class of_file : string -> t
  end

  module Writer : sig
    class type t =
    object
      method put : int -> event -> unit

      method note_off : int -> int -> float -> unit
      method note_on : int -> int -> float -> unit

      method advance : int -> unit

      method close : unit
    end

    class to_file : int -> ?tracks:int -> string -> t
  end
end
