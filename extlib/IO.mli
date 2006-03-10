(* 
 * IO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** High-order abstract I/O.

	IO module simply deals with abstract inputs/outputs. It provides a
	set of methods for working with these IO as well as several
	constructors that enable to write to an underlying channel, buffer,
	or enum.
*)

type input
(** The abstract input type. *)

type 'a output
(** The abstract output type, ['a] is the accumulator data, it is returned
	when the [close_out] function is called. *)

exception No_more_input
(** This exception is raised when reading on an input with the [read] or
  [nread] functions while there is no available token to read. *)

exception Input_closed
(** This exception is raised when reading on a closed input. *)

exception Output_closed
(** This exception is raised when reading on a closed output. *)

(** {6 Standard API} *)

val read : input -> char
(** Read a single char from an input or raise [No_more_input] if
  no input available. *)

val nread : input -> int -> string
(** [nread i n] reads a string of size up to [n] from an input.
  The function will raise [No_more_input] if no input is available.
  It will raise [Invalid_argument] if [n] < 0. *)

val really_nread : input -> int -> string
(** [really_nread i n] reads a string of exactly [n] characters
  from the input. Raises [No_more_input] if at least [n] characters are
  not available. Raises [Invalid_argument] if [n] < 0. *)

val input : input -> string -> int -> int -> int
(** [input i s p l] reads up to [l] characters from the given input, storing
  them in string [s], starting at character number [p]. It returns the actual
  number of characters read or raise [No_more_input] if no character can be
  read. It will raise [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val really_input : input -> string -> int -> int -> int
(** [really_input i s p l] reads exactly [l] characters from the given input,
  storing them in the string [s], starting at position [p]. For consistency with
  {!IO.input} it returns [l]. Raises [No_more_input] if at [l] characters are
  not available. Raises [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val close_in : input -> unit
(** Close the input. It can no longer be read from. *)

val write : 'a output -> char -> unit
(** Write a single char to an output. *)

val nwrite : 'a output -> string -> unit
(** Write a string to an output. *)

val output : 'a output -> string -> int -> int -> int
(** [output o s p l] writes up to [l] characters from string [s], starting at
  offset [p]. It returns the number of characters written. It will raise
  [Invalid_argument] if [p] and [l] do not designate a valid substring of [s]. *)

val really_output : 'a output -> string -> int -> int -> int
(** [really_output o s p l] writes exactly [l] characters from string [s] onto
  the the output, starting with the character at offset [p]. For consistency with
  {!IO.output} it returns [l]. Raises [Invalid_argument] if [p] and [l] do not
  designate a valid substring of [s]. *)

val flush : 'a output -> unit
(** Flush an output. *)

val close_out : 'a output -> 'a
(** Close the output and return its accumulator data.
  It can no longer be written. *)

(** {6 Creation of IO Inputs/Outputs} *)

val input_string : string -> input
(** Create an input that will read from a string. *)

val output_string : unit -> string output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it. *)

val input_channel : in_channel -> input
(** Create an input that will read from a channel. *)

val output_channel : out_channel -> unit output
(** Create an output that will write into a channel. *) 

val input_enum : char Enum.t -> input
(** Create an input that will read from an [enum]. *)

val output_enum : unit -> char Enum.t output
(** Create an output that will write into an [enum]. The 
  final enum is returned when the output is closed. *)

val create_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) -> close:(unit -> unit) -> input
(** Fully create an input by giving all the needed functions. *)

val create_out :
  write:(char -> unit) ->
  output:(string -> int -> int -> int) ->   
  flush:(unit -> unit) -> close:(unit -> 'a) -> 'a output
(** Fully create an output by giving all the needed functions. *)

(** {6 Utilities} *)

val printf : 'a output -> ('b, unit, string, unit) format4 -> 'b
(** The printf function works for any output. *)

val read_all : input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val pipe : unit -> input * unit output
(** Create a pipe between an input and an ouput. Data written from
  the output can be read from the input. *)

val pos_in : input -> input * (unit -> int)
(** Create an input that provide a count function of the number of bytes
  read from it. *)

val pos_out : 'a output -> 'a output * (unit -> int)
(** Create an output that provide a count function of the number of bytes
  written through it. *)

external cast_output : 'a output -> unit output = "%identity"
(** You can safely transform any output to an unit output in a safe way 
  by using this function. *)

(** {6 Binary files API}

	Here is some API useful for working with binary files, in particular
	binary files generated by C applications. By default, encoding of
	multibyte integers is low-endian. The BigEndian module provide multibyte
	operations with other encoding.
*)

exception Overflow of string
(** Exception raised when a read or write operation cannot be completed. *)

val read_byte : input -> int
(** Read an unsigned 8-bit integer. *)

val read_signed_byte : input -> int
(** Read an signed 8-bit integer. *)

val read_ui16 : input -> int
(** Read an unsigned 16-bit word. *)

val read_i16 : input -> int
(** Read a signed 16-bit word. *)

val read_i32 : input -> int
(** Read a signed 32-bit integer. Raise [Overflow] if the
  read integer cannot be represented as a Caml 31-bit integer. *)

val read_real_i32 : input -> int32
(** Read a signed 32-bit integer as an OCaml int32. *)

val read_i64 : input -> int64
(** Read a signed 64-bit integer as an OCaml int64. *)

val read_double : input -> float
(** Read an IEEE double precision floating point value. *)

val read_string : input -> string
(** Read a null-terminated string. *)

val read_line : input -> string
(** Read a LF or CRLF terminated string. *)

val write_byte : 'a output -> int -> unit
(** Write an unsigned 8-bit byte. *)

val write_ui16 : 'a output -> int -> unit
(** Write an unsigned 16-bit word. *)

val write_i16 : 'a output -> int -> unit
(** Write a signed 16-bit word. *)

val write_i32 : 'a output -> int -> unit
(** Write a signed 32-bit integer. *) 

val write_real_i32 : 'a output -> int32 -> unit
(** Write an OCaml int32. *)

val write_i64 : 'a output -> int64 -> unit
(** Write an OCaml int64. *)

val write_double : 'a output -> float -> unit
(** Write an IEEE double precision floating point value. *)

val write_string : 'a output -> string -> unit
(** Write a string and append an null character. *)

val write_line : 'a output -> string -> unit
(** Write a line and append a LF (it might be converted
	to CRLF on some systems depending on the underlying IO). *)

(** Same as operations above, but use big-endian encoding *)
module BigEndian :
sig

	val read_ui16 : input -> int
	val read_i16 : input -> int
	val read_i32 : input -> int
	val read_real_i32 : input -> int32
	val read_i64 : input -> int64
	val read_double : input -> float
	
	val write_ui16 : 'a output -> int -> unit
	val write_i16 : 'a output -> int -> unit
	val write_i32 : 'a output -> int -> unit
	val write_real_i32 : 'a output -> int32 -> unit
	val write_i64 : 'a output -> int64 -> unit
	val write_double : 'a output -> float -> unit

end

(** {6 Bits API}

	This enable you to read and write from an IO bit-by-bit or several bits
	at the same time.
*)

type in_bits
type out_bits

exception Bits_error

val input_bits : input -> in_bits
(** Read bits from an input *)

val output_bits : 'a output -> out_bits
(** Write bits to an output *)

val read_bits : in_bits -> int -> int
(** Read up to 31 bits, raise Bits_error if n < 0 or n > 31 *)

val write_bits : out_bits -> nbits:int -> int -> unit
(** Write up to 31 bits represented as a value, raise Bits_error if nbits < 0
 or nbits > 31 or the value representation excess nbits. *)

val flush_bits : out_bits -> unit
(** Flush remaining unwritten bits, adding up to 7 bits which values 0. *)

val drop_bits : in_bits -> unit
(** Drop up to 7 buffered bits and restart to next input character. *)

(** {6 Generic IO Object Wrappers}

	Theses OO Wrappers have been written to provide easy support of ExtLib
	IO by external librairies. If you want your library to support ExtLib
	IO without actually requiring ExtLib to compile, you can should implement
	the classes [in_channel], [out_channel], [poly_in_channel] and/or
	[poly_out_channel] which are the common IO specifications established
	for ExtLib, OCamlNet and Camomile.

	(see http://www.ocaml-programming.de/tmp/IO-Classes.html for more details).
*)

class in_channel : input ->
  object
	method input : string -> int -> int -> int
	method close_in : unit -> unit
  end

class out_channel : 'a output ->
  object
	method output : string -> int -> int -> int
	method flush : unit -> unit
	method close_out : unit -> unit
  end

class in_chars : input ->
  object
	method get : unit -> char
	method close_in : unit -> unit
  end

class out_chars : 'a output ->
  object
	method put : char -> unit
	method flush : unit -> unit
	method close_out : unit -> unit
  end

val from_in_channel : #in_channel -> input
val from_out_channel : #out_channel -> unit output
val from_in_chars : #in_chars -> input
val from_out_chars : #out_chars -> unit output
