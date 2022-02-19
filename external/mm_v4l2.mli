open Mm_image

type device

val open_device : string -> int -> int -> device

val grab : device -> Image.RGBA32.t

val close : device -> unit
