(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Ast
open Type
open Globals
open Extlib_leftovers

(* -------------------------------------------------------------------------- *)
(* TOOLS *)

let rec has_properties c =
	List.exists (fun f ->
		match f.cf_kind with
		| Var { v_read = AccCall } -> true
		| Var { v_write = AccCall } -> true
		| _ when Meta.has Meta.Accessor f.cf_meta -> true
		| _ -> false
	) c.cl_ordered_fields || (match c.cl_super with Some (c,_) -> has_properties c | _ -> false)

let get_properties fields =
	List.fold_left (fun acc f ->
		if Meta.has Meta.Accessor f.cf_meta then
			(f.cf_name, f.cf_name) :: acc
		else
			let acc = (match f.cf_kind with
			| Var { v_read = AccCall } -> ("get_" ^ f.cf_name , "get_" ^ f.cf_name) :: acc
			| _ -> acc) in
			match f.cf_kind with
			| Var { v_write = AccCall } -> ("set_" ^ f.cf_name , "set_" ^ f.cf_name) :: acc
			| _ -> acc
	) [] fields

(* -------------------------------------------------------------------------- *)
(* MISC FEATURES *)

let rec is_volatile t =
	match t with
	| TMono r ->
		(match r.tm_type with
		| Some t -> is_volatile t
		| _ -> false)
	| TLazy f ->
		is_volatile (lazy_type f)
	| TType (t,tl) ->
		(match t.t_path with
		| _ -> is_volatile (apply_typedef t tl))
	| _ ->
		false

let bytes_serialize data =
	let b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
	let tbl = Array.init (String.length b64) (fun i -> String.get b64 i) in
	Bytes.unsafe_to_string (Base64.str_encode ~tbl data)

(*
	Build a default safe-cast expression :
	{ var $t = <e>; if( Std.is($t,<t>) ) $t else throw "Class cast error"; }
*)
let default_cast ?(vtmp="$t") api std e texpr t p =
	let vtmp = alloc_var VGenerated vtmp e.etype e.epos in
	let var = mk (TVar (vtmp,Some e)) api.tvoid p in
	let vexpr = mk (TLocal vtmp) e.etype p in
	let texpr = Texpr.Builder.make_typeexpr texpr p in
	let is = Texpr.Builder.resolve_and_make_static_call std "isOfType" [vexpr;texpr] p in
	let enull = Texpr.Builder.make_null vexpr.etype p in
	let eop = Texpr.Builder.binop OpEq vexpr enull api.tbool p in
	let echeck = Texpr.Builder.binop OpBoolOr is eop api.tbool p in
	let exc = mk (TThrow (mk (TConst (TString "Class cast error")) api.tstring p)) t p in
	let check = mk (TIf (Texpr.Builder.mk_parent echeck,mk (TCast (vexpr,None)) t p,Some exc)) t p in
	mk (TBlock [var;check;vexpr]) t p

module UnificationCallback = struct
	let check_call_params f el tl =
		let rec loop acc el tl = match el,tl with
			| e :: el, (n,_,t) :: tl ->
				loop ((f e t) :: acc) el tl
			| [], [] ->
				acc
			| [],_ ->
				acc
			| e :: el, [] ->
				loop (e :: acc) el []
		in
		List.rev (loop [] el tl)

	let check_call f el t = match follow t with
		| TFun(args,_) ->
			check_call_params f el args
		| _ ->
			List.map (fun e -> f e t_dynamic) el
end;;

let interpolate_code error code tl f_string f_expr p =
	let exprs = Array.of_list tl in
	let i = ref 0 in
	let err msg =
		let pos = { p with pmin = p.pmin + !i } in
		error msg pos
	in
	let regex = Str.regexp "[{}]" in
	let rec loop m = match m with
		| [] ->
			()
		| Str.Text txt :: tl ->
			i := !i + String.length txt;
			f_string txt;
			loop tl
		| Str.Delim "{" :: Str.Text n :: Str.Delim "}" :: tl ->
			begin try
				let expr = Array.get exprs (int_of_string n) in
				f_expr expr;
			with
			| Failure _ ->
				f_string ("{" ^ n ^ "}");
			| Invalid_argument _ ->
				err ("Out-of-bounds special parameter: " ^ n)
			end;
			i := !i + 2 + String.length n;
			loop tl
		| Str.Delim x :: tl ->
			f_string x;
			incr i;
			loop tl
	in
	loop (Str.full_split regex code)

(* Static extensions for classes *)
module ExtClass = struct
	let add_static_init c cf e p =
		let ethis = Texpr.Builder.make_static_this c p in
		let ef1 = mk (TField(ethis,FStatic(c,cf))) cf.cf_type p in
		let e_assign = mk (TBinop(OpAssign,ef1,e)) e.etype p in
		TClass.add_cl_init c e_assign
end
