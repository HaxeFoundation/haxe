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

external zlib_deflate_init2 : int -> int -> zstream = "zlib_deflate_init2"
external zlib_deflate : zstream -> src:string -> spos:int -> slen:int -> dst:string -> dpos:int -> dlen:int -> zflush -> zresult = "zlib_deflate_bytecode" "zlib_deflate"
external zlib_deflate_end : zstream -> unit = "zlib_deflate_end"

external zlib_inflate_init2 : int -> zstream = "zlib_inflate_init"
external zlib_inflate : zstream -> src:string -> spos:int -> slen:int -> dst:string -> dpos:int -> dlen:int -> zflush -> zresult = "zlib_inflate_bytecode" "zlib_inflate"
external zlib_inflate_end : zstream -> unit = "zlib_inflate_end"

external _executable_path : string -> string = "executable_path"
external get_full_path : string -> string = "get_full_path"
external get_real_path : string -> string = "get_real_path"

external zlib_deflate_bound : zstream -> int -> int = "zlib_deflate_bound"

external time : unit -> float = "sys_time"

type library
type sym
type value

external dlopen : string -> library = "sys_dlopen"
external dlsym : library -> string -> sym = "sys_dlsym"
external dlcall0 : sym -> value = "sys_dlcall0"
external dlcall1 : sym -> value -> value = "sys_dlcall1"
external dlcall2 : sym -> value -> value -> value = "sys_dlcall2"
external dlcall3 : sym -> value -> value -> value -> value = "sys_dlcall3"
external dlcall4 : sym -> value -> value -> value -> value -> value = "sys_dlcall4"
external dlcall5 : sym -> value -> value -> value -> value -> value -> value = "sys_dlcall5_bc" "sys_dlcall5"
external dlint : int -> value = "sys_dlint"
external dltoint : value -> int = "sys_dltoint"
external dlstring : string -> value = "%identity"
external dladdr : value -> int -> value = "sys_dladdr"
external dlptr : value -> value = "sys_dlptr"
external dlsetptr : value -> value -> unit = "sys_dlsetptr"
external dlalloc_string : value -> string = "sys_dlalloc_string"
external dlmemcpy : value -> value -> int -> unit = "sys_dlmemcpy"
external dlcallback : int -> value = "sys_dlcallback"
external dlcaml_callback : int -> value = "sys_dlcaml_callback"
external dlint32 : int32 -> value = "sys_dlint32"
external getch : bool -> int = "sys_getch"

(* support for backward compatibility *)
let zlib_deflate_init lvl = zlib_deflate_init2 lvl 15
let zlib_inflate_init() = zlib_inflate_init2 15

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
	let z = zlib_inflate_init()  in
	let s = zlib_op zlib_inflate z str in
	zlib_inflate_end z;
	s

let input_zip ?(bufsize=65536) ch =
	let tmp_out = String.create bufsize in
	let tmp_in = String.create bufsize in
	let tmp_buf = Buffer.create bufsize in
	let buf = ref "" in
	let p = ref 0 in
	let z = zlib_inflate_init() in
	let rec fill_buffer() =
		let rec loop pos len =
			if len > 0 || pos = 0 then begin
				let r = zlib_inflate z tmp_in pos len tmp_out 0 bufsize (if pos = 0 && len = 0 then Z_FINISH else Z_SYNC_FLUSH) in
				Buffer.add_substring tmp_buf tmp_out 0 r.z_wrote;
				loop (pos + r.z_read) (len - r.z_read);
			end
		in
		loop 0 (IO.input ch tmp_in 0 bufsize);
		p := 0;
		buf := Buffer.contents tmp_buf;
		Buffer.clear tmp_buf;
	in
	let read() =
		if !p = String.length !buf then fill_buffer();
		let c = String.unsafe_get !buf !p in
		incr p;
		c
	in
	let rec input str pos len =
		let b = String.length !buf - !p in
		if b >= len then begin
			String.blit !buf !p str pos len;
			p := !p + len;
			len;
		end else begin
			String.blit !buf !p str pos b;
			fill_buffer();
			if !p = String.length !buf then
				b
			else
				b + input str (pos + b) (len - b)
		end;
	in
	let close() =
		zlib_inflate_end z
	in
	IO.create_in ~read ~input ~close

let output_zip ?(bufsize=65536) ?(level=9) ch =
	let z = zlib_deflate_init level in
	let out = String.create bufsize in
	let tmp_out = String.create bufsize in
	let p = ref 0 in
	let rec flush finish =
		let r = zlib_deflate z out 0 !p tmp_out 0 bufsize (if finish then Z_FINISH else Z_SYNC_FLUSH) in
		ignore(IO.really_output ch tmp_out 0 r.z_wrote);
		let remain = !p - r.z_read in
		String.blit out r.z_read out 0 remain;
		p := remain;
		if finish && not r.z_finish then flush true
	in
	let write c =
		if !p = bufsize then flush false;
		String.unsafe_set out !p c;
		incr p
	in
	let rec output str pos len =
		let b = bufsize - !p in
		if len <= b then begin
			String.blit str pos out !p len;
			p := !p + len;
			len
		end else begin
			String.blit str pos out !p b;
			p := !p + b;
			flush false;
			b + output str (pos + b) (len - b);
		end;
	in
	let close() =
		flush true;
		zlib_deflate_end z
	in
	IO.create_out ~write ~output ~flush:(fun() -> flush false; IO.flush ch) ~close

