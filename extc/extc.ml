(*
 *  Extc : C common OCaml bindings
 *  Copyright (c)2004 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type zstream

type zflush =
	| Z_NO_FLUSH
	| Z_PARTIAL_FLUSH
	| Z_SYNC_FLUSH
	| Z_FULL_FLUSH
	| Z_FINISH


type zresult = {
	z_finish : bool;
	z_read : int;
	z_wrote : int;
}

external zlib_deflate_init : int -> zstream = "zlib_deflate_init"
external zlib_deflate : zstream -> src:string -> spos:int -> slen:int -> dst:string -> dpos:int -> dlen:int -> zflush -> zresult = "zlib_deflate_bytecode" "zlib_deflate"
external zlib_deflate_end : zstream -> unit = "zlib_deflate_end"

external zlib_inflate_init : unit -> zstream = "zlib_inflate_init"
external zlib_inflate : zstream -> src:string -> spos:int -> slen:int -> dst:string -> dpos:int -> dlen:int -> zflush -> zresult = "zlib_inflate_bytecode" "zlib_inflate"
external zlib_inflate_end : zstream -> unit = "zlib_inflate_end"

external _executable_path : string -> string = "executable_path"
external get_full_path : string -> string = "get_full_path"

let executable_path() =
	let p = _executable_path Sys.argv.(0) in
	let p1 = (try String.rindex p '/' with Not_found -> String.length p + 1) in
	let p2 = (try String.rindex p '\\' with Not_found -> String.length p + 1) in
	match min p1 p2 with
	| x when x = String.length p + 1 -> ""
	| pos ->
		String.sub p 0 pos ^ "/"

let zlib_op op z str =
	let bufsize = 1 lsl 14 in
	let tmp = String.create bufsize in
	let total = ref 0 in
	let rec loop pos len acc =
		let r = op z ~src:str ~spos:pos ~slen:len ~dst:tmp ~dpos:0 ~dlen:bufsize (if len = 0 then Z_FINISH else Z_SYNC_FLUSH) in
		total := !total + r.z_wrote;
		let acc = String.sub tmp 0 r.z_wrote :: acc in
		if r.z_finish then
			acc
		else
			loop (pos + r.z_read) (len - r.z_read) acc
	in
	let strings = loop 0 (String.length str) [] in
	let big = String.create !total in
	ignore(List.fold_left (fun p s ->
		let l = String.length s in
		let p = p - l in
		String.unsafe_blit s 0 big p l;
		p
	) !total strings);
	big

let zip str =
	let z = zlib_deflate_init 9 in
	let s = zlib_op zlib_deflate z str in
	zlib_deflate_end z;
	s

let unzip str =
	let z = zlib_inflate_init() in
	let s = zlib_op zlib_inflate z str in
	zlib_inflate_end z;
	s