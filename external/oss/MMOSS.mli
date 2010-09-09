(** Audio input and output using the OSS sound devices. *)

(** Create a writer on an OSS sound device. *)
class writer : ?device:string -> int -> int -> Audio.IO.Writer.t

class reader : ?device:string -> int -> int -> Audio.IO.Reader.t
