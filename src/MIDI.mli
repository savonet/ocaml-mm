(** Operations on MIDI data. *)

(** Units for delta-times. *)
type division =
  | Ticks_per_quarter of int (** Ticks per quarter note. *)
  | SMPTE of int * int (** SMPTE (frames per second, ticks per frame). *)

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

(** A MIDI buffer. *)
type buffer

val data : buffer -> (int * event) list

(** Create a MIDI buffer of given length in samples. *)
val create : int -> buffer

(** Create a copy of a MIDI buffer. *)
val copy : buffer -> buffer

val blit : buffer -> int -> buffer -> int -> int -> unit

val blit_all : buffer -> buffer -> unit

(** [merge b1 b2] merges the buffer [b2] into [b1]. *)
val merge : buffer -> buffer -> unit

val add : buffer -> int -> buffer -> int -> int -> unit

val clear_all : buffer -> unit

val insert : buffer -> (int * event) -> unit

module Multitrack : sig
  type t = buffer array

  type buffer = t

  val channels : buffer -> int

  val duration : buffer -> int

  val create : int -> int -> buffer

  val clear : ?channel:int -> buffer -> int -> int -> unit
end

module IO : sig
  module Reader : sig
    class type t =
    object
      (** Read data at with given samplerate for events, in a given track, with a
          given length in samples. *)
      method read : int -> Multitrack.buffer -> int -> int -> int

      (** Close the stream. *)
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
