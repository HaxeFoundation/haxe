(*
 * Std - Additional functions
 * Copyright (C) 2003 Nicolas Cannasse and Markus Mottl
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

let input_lines ch =
  Enum.from (fun () ->
    try input_line ch with End_of_file -> raise Enum.No_more_elements)

let input_chars ch =
  Enum.from (fun () ->
    try input_char ch with End_of_file -> raise Enum.No_more_elements)

type 'a _mut_list = {
  hd : 'a;
  mutable tl : 'a _mut_list;
}

let input_list ch =
  let _empty = Obj.magic [] in
  let rec loop dst =
    let r = { hd = input_line ch; tl = _empty } in
    dst.tl <- r;
    loop r in
  let r = { hd = Obj.magic(); tl = _empty } in
  try loop r
  with
    End_of_file ->
      Obj.magic r.tl

let buf_len = 8192

let input_all ic =
  let rec loop acc total buf ofs =
    let n = input ic buf ofs (buf_len - ofs) in
    if n = 0 then
      let res = String.create total in
      let pos = total - ofs in
      let _ = String.blit buf 0 res pos ofs in
      let coll pos buf =
        let new_pos = pos - buf_len in
        String.blit buf 0 res new_pos buf_len;
        new_pos in
      let _ = List.fold_left coll pos acc in
      res
    else
      let new_ofs = ofs + n in
      let new_total = total + n in
      if new_ofs = buf_len then
        loop (buf :: acc) new_total (String.create buf_len) 0
      else loop acc new_total buf new_ofs in
  loop [] 0 (String.create buf_len) 0

let input_file ?(bin=false) fname =
  let ch = (if bin then open_in_bin else open_in) fname in
  let str = input_all ch in
  close_in ch;
  str

let output_file ~filename ~text =
  let ch = open_out filename in
  output_string ch text;
  close_out ch

let print_bool = function
  | true -> print_string "true"
  | false -> print_string "false"

let prerr_bool = function
  | true -> prerr_string "true"
  | false -> prerr_string "false"

let string_of_char c = String.make 1 c

external identity : 'a -> 'a = "%identity"

let rec dump r =
	if Obj.is_int r then
		string_of_int (Obj.magic r : int)
	else (* Block. *)
	let rec get_fields acc = function
		| 0 -> acc
		| n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
	in
    let rec is_list r =
		if Obj.is_int r then
			r = Obj.repr 0 (* [] *)
		else
			let s = Obj.size r and t = Obj.tag r in
			t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
	in
    let rec get_list r =
		if Obj.is_int r then
			[]
		else 
			let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
			h :: t
    in
    let opaque name =
		(* XXX In future, print the address of value 'r'.  Not possible in
		* pure OCaml at the moment.
		*)
		"<" ^ name ^ ">"
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
	match t with 
	| _ when is_list r ->
		let fields = get_list r in
		"[" ^ String.concat "; " (List.map dump fields) ^ "]"
	| 0 ->
		let fields = get_fields [] s in
		"(" ^ String.concat ", " (List.map dump fields) ^ ")"
	| x when x = Obj.lazy_tag ->
		(* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
		* clear if very large constructed values could have the same
		* tag. XXX *)
		opaque "lazy"
	| x when x = Obj.closure_tag ->
		opaque "closure"
	| x when x = Obj.object_tag ->
		let fields = get_fields [] s in
		let clasz, id, slots =
			match fields with
			| h::h'::t -> h, h', t 
			| _ -> assert false
		in
		(* No information on decoding the class (first field).  So just print
		* out the ID and the slots. *)
		"Object #" ^ dump id ^ " (" ^ String.concat ", " (List.map dump slots) ^ ")"
    | x when x = Obj.infix_tag ->
		opaque "infix"
    | x when x = Obj.forward_tag ->
		opaque "forward"
	| x when x < Obj.no_scan_tag ->
		let fields = get_fields [] s in
		"Tag" ^ string_of_int t ^
		" (" ^ String.concat ", " (List.map dump fields) ^ ")"
	| x when x = Obj.string_tag ->
		"\"" ^ String.escaped (Obj.magic r : string) ^ "\""
	| x when x = Obj.double_tag ->
		string_of_float (Obj.magic r : float)
	| x when x = Obj.abstract_tag ->
		opaque "abstract"
	| x when x = Obj.custom_tag ->
		opaque "custom"
	| x when x = Obj.final_tag ->
		opaque "final"
	| _ ->
		failwith ("Std.dump: impossible tag (" ^ string_of_int t ^ ")")

let dump v = dump (Obj.repr v)

let print v = print_endline (dump v)

let finally handler f x =
	let r = (
		try
			f x
		with
			e -> handler(); raise e
	) in
	handler();
	r

let __unique_counter = ref 0

let unique() =
  incr __unique_counter;
  !__unique_counter