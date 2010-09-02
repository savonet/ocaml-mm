(** Ringbuffers. *)

module type Elt = sig
  type t

  val create : unit -> t
end

module type R = sig
  type elt

  type buffer = elt array

  type t

  val create : int -> t

  val read_space : t -> int

  val write_space : t -> int

  val read_advance : t -> int -> unit

  val write_advance : t -> int -> unit

  val peek : t -> buffer -> int -> int -> unit

  val read : t -> buffer -> int -> int -> unit

  val write : t -> buffer -> int -> int -> unit

  val transmit : t -> (buffer -> int -> int -> int) -> int
end

module Make : functor (E:Elt) -> R with type elt = E.t

(** Create an extensible ringbuffer. *)
module Make_ext : functor (E:Elt) -> R with type elt = E.t
