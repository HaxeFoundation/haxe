(*
 * Copyright (C)2005-2014 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
open Extlib_leftovers
open TTFData

type glyf_transformation_matrix = {
	mutable a : float;
	mutable b : float;
	mutable c : float;
	mutable d : float;
	mutable tx : float;
	mutable ty : float;
}

type glyf_path = {
	gp_type : int;
	gp_x : float;
	gp_y : float;
	gp_cx : float;
	gp_cy : float;
}

type simple_point = {
	x : float;
	y : float;
}

let mk_path t x y cx cy = {
	gp_type = t;
	gp_x = x;
	gp_y = y;
	gp_cx = cx;
	gp_cy = cy;
}

let identity () = {
	a = 1.0;
	b = 0.0;
	c = 0.0;
	d = 1.0;
	tx = 0.0;
	ty = 0.0;
}

let multiply m x y =
	x *. m.a +. y *. m.b +. m.tx,
	x *. m.c +. y *. m.d +. m.ty

(* TODO: check if this can be done in the parser directly *)
let matrix_from_composite gc =
	let a,b,c,d = match gc.gc_transformation with
		| NoScale -> 1.0,0.0,0.0,1.0
		| Scale f -> f,0.0,0.0,f
		| ScaleXY(fx,fy) -> fx,0.0,0.0,fy
		| ScaleMatrix (a,b,c,d) -> a,b,c,d
	in
	let arg1 = float_of_int gc.gc_arg1 in
	let arg2 = float_of_int gc.gc_arg2 in
	{
		a = a;
		b = b;
		c = c;
		d = d;
		(* TODO: point offsets *)
		tx = arg1 *. a +. arg2 *. b;
		ty = arg1 *. c +. arg2 *. d;
	}

let relative_matrix m = {m with tx = 0.0; ty = 0.0}

let make_coords relative mo g = match mo with
	| None ->
		Array.init (Array.length g.gs_x_coordinates) (fun i -> float_of_int g.gs_x_coordinates.(i),float_of_int g.gs_y_coordinates.(i))
	| Some m ->
		let m = if relative then relative_matrix m else m in
		Array.init (Array.length g.gs_x_coordinates) (fun i ->
			let x,y = float_of_int g.gs_x_coordinates.(i),float_of_int g.gs_y_coordinates.(i) in
			multiply m x y
		)

let build_paths relative mo g =
	let len = Array.length g.gs_x_coordinates in
	let current_end = ref 0 in
	let end_pts = Array.init len (fun i ->
		if g.gs_end_pts_of_contours.(!current_end) = i then begin
			incr current_end;
			true
		end else
			false
	) in
	let is_on i = g.gs_flags.(i) land 0x01 <> 0 in
	let is_end i = end_pts.(i) in
	let arr = DynArray.create () in
	let tx,ty = match mo with None -> 0.0,0.0 | Some m -> m.tx,m.ty in
	let last_added = ref {
		x = 0.0;
		y = 0.0;
	} in
	let add_rel t x y cx cy =
		let p = match t with
			| 0 ->
				mk_path t (x +. tx) (y +. ty) cx cy
			| 1 ->
				mk_path t (x -. !last_added.x) (y -. !last_added.y) cx cy
			| 2 ->
				mk_path t (x -. cx) (y -. cy) (cx -. !last_added.x) (cy -. !last_added.y)
			| _ ->
				assert false
		in
		last_added := { x = x; y = y; };
		DynArray.add arr p
	in
	let add_abs t x y cx cy = DynArray.add arr (mk_path t x y cx cy) in
	let add = if relative then add_rel else add_abs in
	let coords = make_coords relative mo g in

	let left = ref [] in
	let right = ref [] in
	let new_contour = ref true in
	let p = ref { x = 0.0; y = 0.0 } in
	for i = 0 to len - 1 do
		p := {
			x = !p.x +. fst coords.(i);
			y = !p.y +. snd coords.(i);
		};
		let p = !p in
		let is_on = is_on i in
		let is_end = is_end i in
		let rec flush pl = match pl with
			| c :: a :: [] -> add 2 a.x a.y c.x c.y
			| a :: [] -> add 1 a.x a.y 0.0 0.0
			| c1 :: c2 :: pl ->
				add 2 (c1.x +. (c2.x -. c1.x) /. 2.0) (c1.y +. (c2.y -. c1.y) /. 2.0) c1.x c1.y;
				flush (c2 :: pl)
			| _ ->
				Printf.printf "Fail, len: %i\n" (List.length pl);
		in
		if !new_contour then begin
			if is_on then begin
				new_contour := false;
				add 0 p.x p.y 0.0 0.0;
			end;
			left := p :: !left
		end else if is_on || is_end then begin
			right := p :: !right;
			if is_on then begin
				flush (List.rev !right);
				right := []
			end;
			if is_end then begin
				new_contour := true;
				flush ((List.rev !right) @ (List.rev !left));
				left := [];
				right := [];
			end
		end else
			right := p :: !right
	done;
	DynArray.to_list arr

let rec build_glyph_paths ttf relative ?(transformation=None) glyf =
	match glyf with
	| TGlyfSimple (h,g) ->
		build_paths relative transformation g
	| TGlyfComposite (h,gl) ->
		List.concat (List.map (fun g ->
			let t = Some (matrix_from_composite g) in
			build_glyph_paths ttf relative ~transformation:t (ttf.ttf_glyfs.(g.gc_glyf_index))
		) gl)
	| TGlyfNull ->
		[]

let map_char_code cc c4 =
	let index = ref 0 in
	let seg_count = c4.c4_seg_count_x2 / 2 in
	if cc >= 0xFFFF then 0 else begin
		for i = 0 to seg_count - 1 do
			if c4.c4_end_code.(i) >= cc && c4.c4_start_code.(i) <= cc then begin
				if c4.c4_id_range_offset.(i) > 0 then
					let v = c4.c4_id_range_offset.(i)/2 + cc - c4.c4_start_code.(i) - seg_count + i in
					index := c4.c4_glyph_index_array.(v)
				else
					index := (c4.c4_id_delta.(i) + cc) mod 65536
			end
		done;
		!index
	end

let parse_range_str str =
	let last = ref (Char.code '\\') in
	let range = ref false in
	let lut = Hashtbl.create 0 in
	UTF8.iter (fun code ->
		let code = UCharExt.code code in
		if code = Char.code '-' && !last <> Char.code '\\' then
			range := true
		else if !range then begin
			range := false;
			for i = !last to code do
				Hashtbl.replace lut i true;
			done;
		end else begin
			Hashtbl.replace lut code true;
			last := code;
		end
	) str;
	if !range then Hashtbl.replace lut (Char.code '-') true;
	lut

let build_lut ttf range_str =
	let lut = Hashtbl.create 0 in
	Hashtbl.add lut 0 0;
	Hashtbl.add lut 1 1;
	Hashtbl.add lut 2 2;
	let add_character = if range_str = "" then
			fun k v -> Hashtbl.replace lut k v
		else begin
			let range = parse_range_str range_str in
			fun k v -> if Hashtbl.mem range k then Hashtbl.replace lut k v
		end
	in
	let make_cmap4_map c4 =
		let seg_count = c4.c4_seg_count_x2 / 2 in
		for i = 0 to seg_count - 1 do
			for j = c4.c4_start_code.(i) to c4.c4_end_code.(i) do
				let index = map_char_code j c4 in
				add_character j index;
			done;
		done
	in
(*  	let make_cmap12_map c12 =
		List.iter (fun group ->
			let rec loop cc gi =
				add_character cc gi;
				if cc < (Int32.to_int group.c12g_end_char_code) then loop (cc + 1) (gi + 1)
			in
			loop (Int32.to_int group.c12g_start_char_code) (Int32.to_int group.c12g_start_glyph_code)
		) c12.c12_groups
	in *)
	List.iter (fun st -> match st.cs_def with
		| Cmap0 c0 ->
			Array.iteri (fun i c -> add_character i (int_of_char c)) c0.c0_glyph_index_array;
		| Cmap4 c4 ->
			make_cmap4_map c4;
		| Cmap12 c12 ->
			(*
				TODO: this causes an exception with some fonts:
				Fatal error: exception IO.Overflow("write_ui16")
			*)
			(* make_cmap12_map ctx lut c12; *)
			()
		| _ ->
			(* TODO *)
			()
	) ttf.ttf_cmap.cmap_subtables;
	lut
