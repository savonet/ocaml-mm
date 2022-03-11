open Mm_image

type device

val open_device : string -> int -> int -> device

val grab_rgba32 : device -> Image.RGBA32.t -> unit

val close : device -> unit
