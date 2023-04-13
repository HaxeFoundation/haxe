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
open Common
open Error
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

let add_property_field com c =
	let p = c.cl_pos in
	let props = get_properties (c.cl_ordered_statics @ c.cl_ordered_fields) in
	match props with
	| [] -> ()
	| _ ->
		let fields,values = List.fold_left (fun (fields,values) (n,v) ->
			let cf = mk_field n com.basic.tstring p null_pos in
			PMap.add n cf fields,((n,null_pos,NoQuotes),Texpr.Builder.make_string com.basic v p) :: values
		) (PMap.empty,[]) props in
		let t = mk_anon ~fields (ref Closed) in
		let e = mk (TObjectDecl values) t p in
		let cf = mk_field ~static:true "__properties__" t p null_pos in
		cf.cf_expr <- Some e;
		c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
		c.cl_ordered_statics <- cf :: c.cl_ordered_statics

let escape_res_name name allow_dirs =
	ExtString.String.replace_chars (fun chr ->
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') || chr = '_' || chr = '.' then
			Char.escaped chr
		else if chr = '/' && allow_dirs then
			"/"
		else
			"-x" ^ (string_of_int (Char.code chr))) name

(* -------------------------------------------------------------------------- *)
(* FIX OVERRIDES *)

(*
	on some platforms which doesn't support type parameters, we must have the
	exact same type for overridden/implemented function as the original one
*)

let rec find_field com c f =
	try
		(match c.cl_super with
		| None ->
			raise Not_found
		| Some ( {cl_path = (["cpp"],"FastIterator")}, _ ) ->
			raise Not_found (* This is a strongly typed 'extern' and the usual rules don't apply *)
		| Some (c,_) ->
			find_field com c f)
	with Not_found -> try
		if com.platform = Cpp || com.platform = Hl then (* uses delegation for interfaces *)
			raise Not_found;
		let rec loop = function
			| [] ->
				raise Not_found
			| (c,_) :: l ->
				try
					find_field com c f
				with
					Not_found -> loop l
		in
		loop c.cl_implements
	with Not_found ->
		let f = PMap.find f.cf_name c.cl_fields in
		(match f.cf_kind with Var { v_read = AccRequire _ } -> raise Not_found | _ -> ());
		f

let fix_override com c f fd =
	let f2 = (try Some (find_field com c f) with Not_found -> None) in
	match f2,fd with
		| Some (f2), Some(fd) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> die "" __LOC__) in
			let changed_args = ref [] in
			let prefix = "_tmp_" in
			let nargs = List.map2 (fun ((v,ct) as cur) (_,_,t2) ->
				try
					type_eq EqStrict (monomorphs c.cl_params (monomorphs f.cf_params v.v_type)) t2;
					(* Flash generates type parameters with a single constraint as that constraint type, so we
					   have to detect this case and change the variable (issue #2712). *)
					begin match follow v.v_type with
						| TInst({cl_kind = KTypeParameter [tc]} as cp,_) when com.platform = Flash ->
							if List.exists (fun tp -> tp.ttp_name = (snd cp.cl_path)) c.cl_params then raise (Unify_error [])
						| _ ->
							()
					end;
					cur
				with Unify_error _ ->
					let v2 = alloc_var VGenerated (prefix ^ v.v_name) t2 v.v_pos in
					changed_args := (v,v2) :: !changed_args;
					v2,ct
			) fd.tf_args targs in
			let fd2 = {
				tf_args = nargs;
				tf_type = tret;
				tf_expr = (match List.rev !changed_args with
					| [] -> fd.tf_expr
					| args ->
						let e = fd.tf_expr in
						let el = (match e.eexpr with TBlock el -> el | _ -> [e]) in
						let p = (match el with [] -> e.epos | e :: _ -> e.epos) in
						let el_v = List.map (fun (v,v2) ->
							mk (TVar (v,Some (mk (TCast (mk (TLocal v2) v2.v_type p,None)) v.v_type p))) com.basic.tvoid p
						) args in
						{ e with eexpr = TBlock (el_v @ el) }
				);
			} in
			let targs = List.map (fun(v,c) -> (v.v_name, Option.is_some c, v.v_type)) nargs in
			let fde = (match f.cf_expr with None -> die "" __LOC__ | Some e -> e) in
			f.cf_expr <- Some { fde with eexpr = TFunction fd2 };
			f.cf_type <- TFun(targs,tret);
		| Some(f2), None when (has_class_flag c CInterface) ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> die "" __LOC__) in
			f.cf_type <- TFun(targs,tret)
		| _ ->
			()

let fix_overrides com t =
	match t with
	| TClassDecl c ->
		(* overrides can be removed from interfaces *)
		if (has_class_flag c CInterface) then
			c.cl_ordered_fields <- List.filter (fun f ->
				try
					if find_field com c f == f then raise Not_found;
					c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
					false;
				with Not_found ->
					true
			) c.cl_ordered_fields;
		List.iter (fun f ->
			match f.cf_expr, f.cf_kind with
			| Some { eexpr = TFunction fd }, Method (MethNormal | MethInline) ->
				fix_override com c f (Some fd)
			| None, Method (MethNormal | MethInline) when (has_class_flag c CInterface) ->
				fix_override com c f None
			| _ ->
				()
		) c.cl_ordered_fields
	| _ ->
		()

(*
	PHP does not allow abstract classes extending other abstract classes to override any fields, so these duplicates
	must be removed from the child interface
*)
let fix_abstract_inheritance com t =
	match t with
	| TClassDecl c when (has_class_flag c CInterface) ->
		c.cl_ordered_fields <- List.filter (fun f ->
			let b = try (find_field com c f) == f
			with Not_found -> false in
			if not b then c.cl_fields <- PMap.remove f.cf_name c.cl_fields;
			b;
		) c.cl_ordered_fields
	| _ -> ()

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

module Dump = struct
	(*
		Make a dump of the full typed AST of all types
	*)
	let create_dumpfile acc l =
		let ch = Path.create_file false ".dump" acc l in
		let buf = Buffer.create 0 in
		buf, (fun () ->
			output_string ch (Buffer.contents buf);
			close_out ch)

	let create_dumpfile_from_path com path =
		let buf,close = create_dumpfile [] ((dump_path com) :: (platform_name_macro com) :: fst path @ [snd path]) in
		buf,close

	let dump_types com s_expr =
		let s_type = s_type (Type.print_context()) in
		let params tl = match tl with
			| [] -> ""
			| l -> Printf.sprintf "<%s>" (String.concat ", " (List.map Printer.s_type_param l))
		in
		List.iter (fun mt ->
			let path = Type.t_path mt in
			let buf,close = create_dumpfile_from_path com path in
			let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
			let s_metas ml tabs =
				let args el =
					match el with
					| [] -> ""
					| el -> Printf.sprintf "(%s)" (String.concat ", " (List.map (fun e -> Ast.Printer.s_expr e) el)) in
				match ml with
				| [] -> ""
				| ml -> String.concat " " (List.map (fun me -> match me with (m,el,_) -> "@" ^ Meta.to_string m ^ args el) ml) ^ "\n" ^ tabs in
			(match mt with
			| Type.TClassDecl c ->
				let s_cf_expr f =
					match f.cf_expr with
					| None -> ""
					| Some e -> Printf.sprintf "%s" (s_expr s_type e) in
				let is_inline_var v : bool = v = Var { v_read = AccInline; v_write = AccNever } in
				let rec print_field stat f =
					print "\n\t%s%s%s%s%s %s%s"
						(s_metas f.cf_meta "\t")
						(if (has_class_field_flag f CfPublic && not ((has_class_flag c CExtern) || (has_class_flag c CInterface))) then "public " else "")
						(if stat then "static " else "")
						(match f.cf_kind with
							| Var v when (is_inline_var f.cf_kind) -> "inline "
							| Var v -> ""
							| Method m ->
								match m with
								| MethNormal -> ""
								| MethDynamic -> "dynamic "
								| MethInline -> "inline "
								| MethMacro -> "macro ")
						(match f.cf_kind with Var v -> "var" | Method m -> "function")
						(f.cf_name ^ match f.cf_kind with
							| Var { v_read = AccNormal; v_write = AccNormal } -> ""
							| Var v when (is_inline_var f.cf_kind) -> ""
							| Var v -> "(" ^ s_access true v.v_read ^ "," ^ s_access false v.v_write ^ ")"
							| _ -> "")
						(params f.cf_params);
					(match f.cf_kind with
						| Var v -> print ":%s%s;" (s_type f.cf_type)
							(match f.cf_expr with
							| None -> ""
							| Some e -> " = " ^ (s_cf_expr f));
						| Method m -> if ((has_class_flag c CExtern) || (has_class_flag c CInterface)) then (
							match f.cf_type with
							| TFun(al,t) -> print "(%s):%s;" (String.concat ", " (
								List.map (fun (n,o,t) -> n ^ ":" ^ (s_type t)) al))
								(s_type t)
							| _ -> ()
						) else print "%s" (s_cf_expr f));
					print "\n";
					List.iter (fun f -> print_field stat f) f.cf_overloads
				in
				print "%s%s%s%s %s%s" (s_metas c.cl_meta "") (if c.cl_private then "private " else "") (if (has_class_flag c CExtern) then "extern " else "") (if (has_class_flag c CInterface) then "interface" else "class") (s_type_path path) (params c.cl_params);
				(match c.cl_super with None -> () | Some (c,pl) -> print " extends %s" (s_type (TInst (c,pl))));
				List.iter (fun (c,pl) -> print " implements %s" (s_type (TInst (c,pl)))) c.cl_implements;
				(match c.cl_array_access with None -> () | Some t -> print " implements ArrayAccess<%s>" (s_type t));
				print " {\n";
				(match c.cl_constructor with
				| None -> ()
				| Some f -> print_field false f);
				List.iter (print_field false) c.cl_ordered_fields;
				List.iter (print_field true) c.cl_ordered_statics;
				(match c.cl_init with
				| None -> ()
				| Some e ->
					print "\n\tstatic function __init__() ";
					print "%s" (s_expr s_type e);
					print "\n");
				print "}";
			| Type.TEnumDecl e ->
				print "%s%s%senum %s%s {\n" (s_metas e.e_meta "") (if e.e_private then "private " else "") (if e.e_extern then "extern " else "") (s_type_path path) (params e.e_params);
				List.iter (fun n ->
					let f = PMap.find n e.e_constrs in
					print "\t%s%s;\n" f.ef_name (
						match f.ef_type with
						| TFun (al,t) -> Printf.sprintf "(%s)" (String.concat ", "
							(List.map (fun (n,o,t) -> (if o then "?" else "") ^ n ^ ":" ^ (s_type t)) al))
						| _ -> "")
				) e.e_names;
				print "}"
			| Type.TTypeDecl t ->
				print "%s%stypedef %s%s = %s" (s_metas t.t_meta "") (if t.t_private then "private " else "") (s_type_path path) (params t.t_params) (s_type t.t_type);
			| Type.TAbstractDecl a ->
				print "%s%sabstract %s%s%s%s {}" (s_metas a.a_meta "") (if a.a_private then "private " else "") (s_type_path path) (params a.a_params)
				(String.concat " " (List.map (fun t -> " from " ^ s_type t) a.a_from))
				(String.concat " " (List.map (fun t -> " to " ^ s_type t) a.a_to));
			);
			close();
		) com.types

	let dump_record com =
		List.iter (fun mt ->
			let buf,close = create_dumpfile_from_path com (t_path mt) in
			let s = match mt with
				| TClassDecl c -> Printer.s_tclass "" c
				| TEnumDecl en -> Printer.s_tenum "" en
				| TTypeDecl t -> Printer.s_tdef "" t
				| TAbstractDecl a -> Printer.s_tabstract "" a
			in
			Buffer.add_string buf s;
			close();
		) com.types

	let dump_position com =
		List.iter (fun mt ->
			match mt with
				| TClassDecl c ->
					let buf,close = create_dumpfile_from_path com (t_path mt) in
					Printf.bprintf buf "%s\n" (s_type_path c.cl_path);
					let field cf =
						Printf.bprintf buf "\t%s\n" cf.cf_name;
						begin match cf.cf_expr with
						| None -> ()
						| Some e ->
							Printf.bprintf buf "%s\n" (Texpr.dump_with_pos "\t" e);
						end
					in
					Option.may field c.cl_constructor;
					List.iter field c.cl_ordered_statics;
					List.iter field c.cl_ordered_fields;
					close();
				| _ ->
					()
		) com.types

	let dump_types com =
		match Common.defined_value_safe com Define.Dump with
			| "pretty" -> dump_types com (Type.s_expr_pretty false "\t" true)
			| "legacy" -> dump_types com Type.s_expr
			| "record" -> dump_record com
			| "position" -> dump_position com
			| _ -> dump_types com (Type.s_expr_ast (not (Common.defined com Define.DumpIgnoreVarIds)) "\t")

	let dump_dependencies ?(target_override=None) com =
		let target_name = match target_override with
			| None -> platform_name_macro com
			| Some s -> s
		in
		let dump_dependencies_path = [dump_path com;target_name;"dependencies"] in
		let buf,close = create_dumpfile [] dump_dependencies_path in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		let dep = Hashtbl.create 0 in
		List.iter (fun m ->
			print "%s:\n" (Path.UniqueKey.lazy_path m.m_extra.m_file);
			PMap.iter (fun _ m2 ->
				let file = Path.UniqueKey.lazy_path m2.m_extra.m_file in
				print "\t%s\n" file;
				let l = try Hashtbl.find dep file with Not_found -> [] in
				Hashtbl.replace dep file (m :: l)
			) m.m_extra.m_deps;
		) com.Common.modules;
		close();
		let dump_dependants_path = [dump_path com;target_name;"dependants"] in
		let buf,close = create_dumpfile [] dump_dependants_path in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		Hashtbl.iter (fun n ml ->
			print "%s:\n" n;
			List.iter (fun m ->
				print "\t%s\n" (Path.UniqueKey.lazy_path m.m_extra.m_file);
			) ml;
		) dep;
		close()
end

(*
	Build a default safe-cast expression :
	{ var $t = <e>; if( Std.is($t,<t>) ) $t else throw "Class cast error"; }
*)
let default_cast ?(vtmp="$t") com e texpr t p =
	let api = com.basic in
	let mk_texpr = function
		| TClassDecl c -> mk_anon (ref (Statics c))
		| TEnumDecl e -> mk_anon (ref (EnumStatics e))
		| TAbstractDecl a -> mk_anon (ref (AbstractStatics a))
		| TTypeDecl _ -> die "" __LOC__
	in
	let vtmp = alloc_var VGenerated vtmp e.etype e.epos in
	let var = mk (TVar (vtmp,Some e)) api.tvoid p in
	let vexpr = mk (TLocal vtmp) e.etype p in
	let texpr = mk (TTypeExpr texpr) (mk_texpr texpr) p in
	let std = (try List.find (fun t -> t_path t = ([],"Std")) com.types with Not_found -> die "" __LOC__) in
	let fis = (try
			let c = (match std with TClassDecl c -> c | _ -> die "" __LOC__) in
			FStatic (c, PMap.find "isOfType" c.cl_statics)
		with Not_found ->
			die "" __LOC__
	) in
	let std = mk (TTypeExpr std) (mk_texpr std) p in
	let is = mk (TField (std,fis)) (tfun [t_dynamic;t_dynamic] api.tbool) p in
	let is = mk (TCall (is,[vexpr;texpr])) api.tbool p in
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

let interpolate_code com code tl f_string f_expr p =
	let exprs = Array.of_list tl in
	let i = ref 0 in
	let err msg =
		let pos = { p with pmin = p.pmin + !i } in
		com.error_msg msg pos
	in
	let regex = Str.regexp "[{}]" in
	let rec loop m = match m with
		| [] ->
			()
		| Str.Text txt :: tl ->
			i := !i + String.length txt;
			f_string txt;
			loop tl
		| Str.Delim a :: Str.Delim b :: tl when a = b ->
			i := !i + 2;
			f_string a;
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

let map_source_header com f =
	match Common.defined_value_safe com Define.SourceHeader with
	| "" -> ()
	| s -> f s


(* Static extensions for classes *)
module ExtClass = struct

	let add_cl_init c e = match c.cl_init with
			| None -> c.cl_init <- Some e
			| Some e' -> c.cl_init <- Some (concat e' e)

	let add_static_init c cf e p =
		let ethis = Texpr.Builder.make_static_this c p in
		let ef1 = mk (TField(ethis,FStatic(c,cf))) cf.cf_type p in
		let e_assign = mk (TBinop(OpAssign,ef1,e)) e.etype p in
		add_cl_init c e_assign
end
