let init () =
  Sdl.init [`VIDEO]

module I = Image.RGBA8

(*
(** 8bit surfaces always use a palette *)
let from_8 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let image = Sdlvideo.pixel_data_8 surface in
  let a = I.create width height in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      let r,g,b = Sdlvideo.get_palette_color surface image.{i+j*pitch} in
      I.set_pixel a i j (r,g,b,0xff)
    done
  done;
  a

(** 16bits surfaces contain specially packed RGB *)
let to_16 rgb surface =
  let s = Sdlvideo.pixel_data_16 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/2 in (* initial pitch was in bytes *)
  let fmt = Sdlvideo.surface_format surface in
  assert (width = I.width rgb && height = I.height rgb);
  assert (fmt.Sdlvideo.amask = 0l && not fmt.Sdlvideo.palette) ;
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      let r,g,b,_ = I.get_pixel rgb i j in
      let color =
        ((r lsr fmt.Sdlvideo.rloss) lsl fmt.Sdlvideo.rshift) lor
          ((g lsr fmt.Sdlvideo.gloss) lsl fmt.Sdlvideo.gshift) lor
          ((b lsr fmt.Sdlvideo.bloss) lsl fmt.Sdlvideo.bshift)
      in
      (* let color = Int32.to_int (Sdlvideo.map_RGB surface (r,g,b)) in *)
      s.{i+j*pitch} <- color
    done
  done
*)

(*
(** 24bits surfaces are standard RGB stored in three different bytes, but the
    order might vary. *)
let from_24 surface =
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let fmt = Sdlvideo.surface_format surface in
  let rgb = Sdlvideo.pixel_data_24 surface in
  let a = I.create width height in
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      for c = 0 to 2 do
        let c' = if fmt.Sdlvideo.rshift = 0 then c else 2-c in
        rgba.{c+i*4+j*a.RGB.stride} <- rgb.{c'+i*3+j*pitch}
      done ;
      rgba.{3+i*4+j*a.RGB.stride} <- 0xff
    done
  done ;
  a
*)

(** 32bits surfaces are standard RGBA However, the RGB components are (at least
    sometimes) packed in a different order as in liquidsoap: 0xAARRGGBB.

    An alternative implementation, which is surprisingly not sensibly
    faster, uses SDL blitting directly by casting a char* into an int*.
    The alpha is masked out because we don't want
    to see video frames on top of each other on screen.
    This hack might not work the same on different platforms.
    let s =
    Sdlvideo.create_RGB_surface_from_32
    (Obj.magic rgb.RGB.data)
    ~w:rgb.RGB.width
    ~h:rgb.RGB.height
    ~pitch:rgb.RGB.stride
(* The masks might be endianness dependent *)
        ~rmask:0xffl ~gmask:0xff00l ~bmask:0xff0000l
        ~amask:0l
      in
        Sdlvideo.blit_surface ~src:s ~dst:surface ()
  *)
(*
let to_32 rgb surface =
  let s = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let pitch = pitch/4 in (* initial pitch was in bytes *)
  let fmt = Sdlvideo.surface_format surface in
  assert (width = I.width rgb && height = I.height rgb);
  assert (fmt.Sdlvideo.amask = 0l && not fmt.Sdlvideo.palette);
  for i = 0 to width-1 do
    for j = 0 to height-1 do
      let r,g,b,_ = I.get_pixel rgb i j in
      let color =
        Int32.of_int
          ((r lsl fmt.Sdlvideo.rshift) lor
              (g lsl fmt.Sdlvideo.gshift) lor
              (b lsl fmt.Sdlvideo.bshift))
      in
      s.{i+j*pitch} <- color
    done
  done
*)

external to_32 : Video.frame -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t -> (int * int * int) -> unit = "caml_sdl_rgb_to32"

let to_32 rgb surface =
  let sbuf = Sdlvideo.pixel_data_32 surface in
  let fmt = Sdlvideo.surface_format surface in
  to_32 rgb sbuf (fmt.Sdlvideo.rshift, fmt.Sdlvideo.gshift, fmt.Sdlvideo.bshift)

(*
let from_32 surface =
  let img = Sdlvideo.pixel_data_32 surface in
  let width,height,pitch = Sdlvideo.surface_dims surface in
  let fmt = Sdlvideo.surface_format surface in
  let pitch = pitch/4 in (* pitch is in bytes, convert for int32 array *)
  let a = RGB.create width height in
  let rgba = a.RGB.data in
  assert (fmt.Sdlvideo.rloss = 0 &&
          fmt.Sdlvideo.gloss = 0 &&
          fmt.Sdlvideo.bloss = 0) ;
  let (&&) = Int32.logand in
  let (>>) = Int32.shift_right in
    for i = 0 to width-1 do
      for j = 0 to height-1 do
        let pixel = img.{i+j*pitch} in
        let pos = i*4+j*a.RGB.stride in
        rgba.{0+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.rmask) >> fmt.Sdlvideo.rshift) ;
        rgba.{1+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.gmask) >> fmt.Sdlvideo.gshift) ;
        rgba.{2+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.bmask) >> fmt.Sdlvideo.bshift) ;
        rgba.{3+pos} <-
          Int32.to_int ((pixel && fmt.Sdlvideo.amask) >> fmt.Sdlvideo.ashift)
      done
    done ;
    a
*)

class writer_to_screen w h =
object (self)
  initializer
    Sdlevent.enable_events Sdlevent.quit_mask;
    (* Try to get 32bpp because it's faster (twice as fast here), but accept
     * other formats too. *)
    ignore (Sdlvideo.set_video_mode ~w ~h ~bpp:32 [`ANYFORMAT;`DOUBLEBUF])

  method write buf ofs len =
    if Sdlevent.poll () = Some Sdlevent.QUIT then
      Sdl.quit ()
    else if len > 0 then
      let surface = Sdlvideo.get_video_surface () in
      (* We only display the last image of each frame *)
      let rgb = buf.(ofs+len-1) in
      begin
        match Sdlvideo.surface_bpp surface with
          (* | 16 -> to_16 rgb surface *)
          | 32 -> to_32 rgb surface
          | i -> failwith (Printf.sprintf "Unsupported format %dbpp" i)
      end;
      Sdlvideo.flip surface

  method close =
    Sdl.quit ()
end
