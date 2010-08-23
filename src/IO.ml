(** Helper functions for reading and writing. *)

exception Invalid_data

module Unix = struct
  (** To be inherited to read and write from files. *)
  class virtual rw ?(read=false) ?(write=false) fname =
  object (self)
    val fd =
      let flag, perms =
	match read, write with
	  | false, false -> assert false
	  | true, false -> [Unix.O_RDONLY], 0o644
	  | false, true -> [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC], 0o644
	  | true, true -> [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC], 0o644
      in
      Unix.openfile fname flag perms

    method stream_read buf ofs len = Unix.read fd buf ofs len

    method stream_write buf ofs len = Unix.write fd buf ofs len

    method stream_close = Unix.close fd

    method stream_seek n =
      ignore (Unix.lseek fd n Unix.SEEK_SET)

    method stream_cur_pos =
      Unix.lseek fd 0 Unix.SEEK_CUR
  end
end

class virtual helper =
object (self)
  method virtual stream_read : string -> int -> int -> int

  method input_once n =
    let buf = String.create n in
    let n = self#stream_read buf 0 n in
    if n = String.length buf then
      buf
    else
      String.sub buf 0 n

  method input n =
    let buf = self#input_once n in
    let buflen = String.length buf in
    if buflen = n || buflen = 0 then
      buf
    else
      buf ^ self#input (n - buflen)

  method really_input n =
    let buf = self#input n in
    if String.length buf <> n then
      raise Invalid_data;
    buf

  method input_byte =
    let buf = self#really_input 1 in
    int_of_char buf.[0]

  (* TODO: use really_input instead of input_byte *)
  method input_int_num_bytes n =
    let rec aux = function
      | 0 -> 0
      | n ->
        let b = self#input_byte in
        b + 256 * (aux (n-1))
    in
    aux n

  method input_int = self#input_int_num_bytes 4

  method input_short = self#input_int_num_bytes 2

  method input_int_num_bytes_be n =
    let ans = ref 0 in
    let buf = self#really_input n in
    for i = 0 to n - 1 do
      ans := 256 * !ans + int_of_char buf.[i]
    done;
    !ans

  method input_int_be = self#input_int_num_bytes_be 4

  method input_short_be = self#input_int_num_bytes_be 2

  method virtual stream_write : string -> int -> int -> int

  method output s =
    let len = String.length s in
    assert (self#stream_write s 0 len = len)

  method output_num b n =
    let s = String.create b in
    for i = 0 to b - 1 do
      s.[i] <- char_of_int ((n lsr (8 * i)) land 0xff)
    done;
    self#output s

    method output_short n = self#output_num 2 n

    method output_int n = self#output_num 4 n
end
