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

type t =
  {
    data : bool array array;
    width : int
  }

let create c width height =
  let data = Array.init height (fun _ -> Array.make width c) in
  { data; width }

let create_white = create true

let create = create false

let init width height f =
  let data = Array.init height (fun j -> Array.init width (fun i -> f i j)) in
  { data; width }

let width img = img.width

let height img = Array.length img.data

let get_pixel img i j = img.data.(j).(i)

let set_pixel img i j c = img.data.(j).(i) <- c

(** Bitmap fonts. *)
module Font = struct
  module CharMap = Map.Make(struct type t = char let compare (c:t) (d:t) = Stdlib.compare c d end)

  (** A fixed-size font. *)
  type nonrec t =
    {
      map : t CharMap.t Lazy.t;
      width : int; (** width of a char in pixels *)
      height : int; (** height of a char in pixels *)
      default : t; (** default displayed character when not supported *)
      uppercase : bool; (** whether only uppercase caracters are supported *)
    }

  (** Our native font. *)
  let native : t =
    let prebitmap =
      [
        ('A', [| " * "; "* *"; "***"; "* *"; "* *" |]);
        ('B', [| "** "; "* *"; "** "; "* *"; "** " |]);
        ('C', [| " **"; "*  "; "*  "; "*  "; " **" |]);
        ('D', [| "** "; "* *"; "* *"; "* *"; "** " |]);
        ('E', [| "***"; "*  "; "** "; "*  "; "***" |]);
        ('F', [| "***"; "*  "; "** "; "*  "; "*  " |]);
        ('G', [| " **"; "*  "; "* *"; "* *"; " **" |]);
        ('H', [| "* *"; "* *"; "***"; "* *"; "* *" |]);
        ('I', [| " * "; " * "; " * "; " * "; " * " |]);
        ('J', [| "  *"; "  *"; "  *"; "* *"; " * " |]);
        ('K', [| "* *"; "** "; "*  "; "** "; "* *" |]);
        ('L', [| "*  "; "*  "; "*  "; "*  "; "***" |]);
        ('M', [| "* *"; "***"; "* *"; "* *"; "* *" |]);
        ('N', [| "* *"; "***"; "***"; "***"; "* *" |]);
        ('O', [| " * "; "* *"; "* *"; "* *"; " * " |]);
        ('P', [| "** "; "* *"; "** "; "*  "; "*  " |]);
        ('Q', [| " * "; "* *"; "* *"; "* *"; " **" |]);
        ('R', [| "** "; "* *"; "** "; "* *"; "* *" |]);
        ('S', [| " **"; "*  "; " * "; "  *"; "** " |]);
        ('T', [| "***"; " * "; " * "; " * "; " * " |]);
        ('U', [| "* *"; "* *"; "* *"; "* *"; "***" |]);
        ('V', [| "* *"; "* *"; "* *"; "* *"; " * " |]);
        ('W', [| "* *"; "* *"; "* *"; "***"; "* *" |]);
        ('X', [| "* *"; "* *"; " * "; "* *"; "* *" |]);
        ('Y', [| "* *"; "* *"; " * "; " * "; " * " |]);
        ('Z', [| "***"; "  *"; " * "; "*  "; "***" |]);
        ('0', [| " * "; "* *"; "* *"; "* *"; " * " |]);
        ('1', [| " * "; "** "; " * "; " * "; " * " |]);
        ('2', [| " * "; "* *"; "  *"; " * "; "***" |]);
        ('3', [| "** "; "  *"; " * "; "  *"; "** " |]);
        ('4', [| "  *"; " **"; "***"; "  *"; "  *" |]);
        ('5', [| "***"; "*  "; "** "; "  *"; "** " |]);
        ('6', [| " **"; "*  "; "** "; "* *"; " * " |]);
        ('7', [| "***"; "  *"; " * "; " * "; " * " |]);
        ('8', [| " * "; "* *"; " * "; "* *"; " * " |]);
        ('9', [| " * "; "* *"; " **"; "  *"; " * " |]);
        (' ', [| "   "; "   "; "   "; "   "; "   " |]);
        ('.', [| "   "; "   "; "   "; "   "; " * " |]);
        (',', [| "   "; "   "; "   "; " * "; " * " |]);
        ('!', [| " * "; " * "; " * "; "   "; " * " |]);
        ('?', [| " * "; "* *"; " **"; " * "; " * " |]);
        ('-', [| "   "; "   "; "***"; "   "; "   " |]);
        ('+', [| "   "; " * "; "***"; " * "; "   " |]);
        ('=', [| "   "; "***"; "   "; "***"; "   " |]);
        (':', [| "   "; " * "; "   "; " * "; "   " |]);
        ('<', [| "  *"; " * "; "*  "; " * "; "  *" |]);
        ('>', [| "*  "; " * "; "  *"; " * "; "*  " |]);
      ]
    in
    let width = 3 in
    let height = 5 in
    let map =
      Lazy.from_fun
        (fun () ->
           List.fold_left
             (fun f (c, b) ->
                let bmp = init width height (fun i j -> b.(j).[i] <> ' ') in
                CharMap.add c bmp f
             ) CharMap.empty prebitmap
        )
    in
    let default = create_white width height in
    { map; width; height; default; uppercase = true }
end
