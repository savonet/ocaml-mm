module RGB8 : sig
  module Color : sig
    type t = int * int * int

    val of_int : int -> t
  end
end

(** Operations on images stored in RGBA8 format (ie RGB channels + an alpha
    channel, one byte for each). *)
module RGBA8 : sig
  module Color : sig
    type t = int * int * int * int
  end

  (** An image. *)
  type t

  val create : int -> int -> t

  val copy : t -> t

  val blit : ?blank:bool -> ?x:int -> ?y:int -> ?w:int -> ?h:int -> t -> t -> unit

  val blit_all : t -> t -> unit
end
