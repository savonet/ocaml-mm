(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

open Mm_base
open Mm_image

module Image = struct
  include Image.YUV420

  let create w h = create w h
end

type t = Image.t array
type buffer = t

let make len width height = Array.init len (fun _ -> Image.create width height)
let single img = [| img |]

let blit sbuf sofs dbuf dofs len =
  for i = 0 to len - 1 do
    Image.blit_all sbuf.(sofs + i) dbuf.(dofs + i)
  done

let copy vid = Array.map Image.copy vid
let length vid = Array.length vid

let size vid =
  let n = ref 0 in
  for i = 0 to Array.length vid - 1 do
    n := !n + Image.size vid.(i)
  done;
  !n

let get vid i = vid.(i)
let set vid i img = vid.(i) <- img

let iter f vid off len =
  for i = off to off + len - 1 do
    f vid.(i)
  done

let blank vid off len = iter Image.blank vid off len
let randomize vid off len = iter Image.randomize vid off len

(*
module RE = struct
  type t = Image.t

  let create () = Image.create 0 0

  let blit = blit
end
*)

(* module Ringbuffer_ext = Ringbuffer.Make_ext (RE) *)

(* module Ringbuffer = Ringbuffer.Make (RE) *)

module FPS = struct
  type t = float

  (* TODO: improve this! *)
  let to_frac f =
    let n = floor ((f *. 100.) +. 0.5) in
    let n = int_of_float n in
    if n mod 100 = 0 then (n / 100, 1) else (n, 100)
end

module IO = struct
  exception Invalid_file

  module Reader = struct
    class type t =
      object
        method width : int

        method height : int

        method frame_rate : float

        (* method set_target_size : int -> int -> unit *)
        method read : buffer -> int -> int -> int

        (* method read_audio : Audio.buffer -> int -> int -> int *)
        method close : unit
      end
  end

  module Writer = struct
    class type t =
      object
        method write : buffer -> int -> int -> unit

        (* method write_audio : Audio.buffer -> int -> int -> unit *)
        method close : unit
      end

    class virtual avi frame_rate w h =
      let frames_per_chunk = int_of_float (frame_rate +. 0.5) in
      let frame_size = w * h * 3 in
      object (self)
        inherit IO.helper

        method virtual private stream_write : string -> int -> int -> int

        method virtual private stream_seek : int -> unit

        method virtual private stream_close : unit

        initializer
        self#output "RIFF";
        self#output_int 0;
        (* TOFILL: file size *)
        self#output "AVI ";
        (* file type *)
        (* Headers *)
        self#output "LIST";
        self#output_int 192;
        (* size of the list *)
        self#output "hdrl";
        (* AVI header *)
        self#output "avih";
        self#output_int 56;
        (* AVI header size *)
        self#output_int (int_of_float (1000000. /. frame_rate));
        (* microseconds per frame *)
        self#output_int 0;
        (* max bytes per sec *)
        self#output_int 0;
        (* pad to multiples of this size *)
        self#output_byte 0;
        (* flags *)
        self#output_byte 1;
        (* flags (interleaved) *)
        self#output_byte 0;
        (* flags *)
        self#output_byte 0;
        (* flags *)
        self#output_int 0;
        (* TOFILL: total number of frames *)
        self#output_int 0;
        (* initial frame *)
        self#output_int 1;
        (* number of streams (TODO: change if audio) *)
        self#output_int 0;
        (* suggested buffer size *)
        self#output_int w;
        (* width *)
        self#output_int h;
        (* height *)
        self#output_int 0;
        (* scale *)
        self#output_int 0;
        (* rate *)
        self#output_int 0;
        (* start *)
        self#output_int 0;
        (* length *)
        (* Stream headers *)
        self#output "LIST";
        self#output_int 116;
        self#output "strl";
        (* Stream header *)
        self#output "strh";
        self#output_int 56;
        self#output "vids";
        self#output "RGB ";
        (* codec *)
        self#output_int 0;
        (* flags *)
        self#output_int 0;
        (* stream priority and language *)
        self#output_int 0;
        (* initial frames *)
        self#output_int 10;
        (* scale : rate / scale = frames / second or samples / second *)
        self#output_int (int_of_float (frame_rate *. 10.));
        (* rate *)
        self#output_int 0;
        (* stream start time (in frames). *)
        self#output_int 0;
        (* TOFILL: stream length (= number of frames) *)
        self#output_int (frames_per_chunk * frame_size);
        (* suggested buffer size *)
        self#output_int 0;
        (* stream quality *)
        self#output_int 0;
        (* size of samples *)
        self#output_short 0;
        (* destination rectangle: left *)
        self#output_short 0;
        (* top *)
        self#output_short w;
        (* right *)
        self#output_short h;
        (* bottom *)
        (* Stream format *)
        self#output "strf";
        self#output_int 40;
        self#output_int 40;
        (* video size (????) *)
        self#output_int w;
        (* width *)
        self#output_int h;
        (* height *)
        self#output_short 1;
        (* panes *)
        self#output_short 24;
        (* color depth *)
        self#output_int 0;
        (* tag1 (????) *)
        self#output_int frame_size;
        (* image size *)
        self#output_int 0;
        (* X pixels per meter *)
        self#output_int 0;
        (* Y pixels per meter *)
        self#output_int 0;
        (* colors used *)
        self#output_int 0;

        (* important colors *)

        (* movie data *)
        self#output "LIST";
        self#output_int 0;
        (* TOFILL: movie size *)
        self#output "movi";

        (* video chunks follow *)
        self#output "00dc";
        self#output_int 0

        (* TOFILL: size *)
        val mutable datalen = 0

        val mutable dataframes = 0

        method write (_ : buffer) ofs len =
          for _ = ofs to ofs + len - 1 do
            (* let s = Image.to_RGB24_string buf.(i) in *)
            let s = failwith "TODO: output YUV420 avi" in
            self#output s;
            datalen <- datalen + String.length s
          done;
          dataframes <- dataframes + len

        method close =
          Printf.printf "completing... (%d frames)\n%!" dataframes;
          self#stream_seek 4;
          self#output_int (datalen + (56 * 4));
          self#stream_seek (12 * 4);
          self#output_int dataframes;
          self#stream_seek (35 * 4);
          self#output_int dataframes;
          self#stream_seek (54 * 4);
          self#output_int (datalen + (3 * 4));
          self#stream_seek (57 * 4);
          self#output_int datalen;
          self#stream_close
      end

    class to_avi_file fname fr w h =
      object
        inherit avi fr w h

        inherit IO.Unix.rw ~write:true fname
      end
  end
end
