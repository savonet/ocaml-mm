(** Operations on ringbuffers. *)

(** Signature for modules describing elements of ringbuffers (used by functors
    creating ringbuffers). *)
module type Elt = sig
  (** Type of an element. *)
  type t

  (** Generate an element. *)
  val create : unit -> t

  (** Blitting function. [Array.blit] should be used in most cases unless some
      optimizations are required. *)
  val blit : t array -> int -> t array -> int -> int -> unit
end

(** Signature for ringbuffer modules. *)
module type R = sig
  (** Type of elements contained in the ringbuffer. *)
  type elt

  (** A buffer of elements. *)
  type buffer = elt array

  (** A ringbuffer. *)
  type t

  (** Create a ringbuffer of given size. *)
  val create : int -> t

  (** Size of data available for reading. *)
  val read_space : t -> int

  (** Size of space available for writing. *)
  val write_space : t -> int

  (** Drop data. *)
  val read_advance : t -> int -> unit

  (** Advance the write pointer. *)
  val write_advance : t -> int -> unit

  (** Read data. *)
  val read : t -> buffer -> int -> int -> unit

  (** Same as [read] but does not advance the read pointer. *)
  val peek : t -> buffer -> int -> int -> unit

  (** Write data. *)
  val write : t -> buffer -> int -> int -> unit

  (** Read all the data in the ringbuffer. *)
  val transmit : t -> (buffer -> int -> int -> int) -> int
end

(** Create a ringbuffer. *)
module Make : functor (E:Elt) -> R with type elt = E.t

(** Create an extensible ringbuffer: the size of the ringbuffer is extended if
    write space is too small at some point. *)
module Make_ext : functor (E:Elt) -> R with type elt = E.t
