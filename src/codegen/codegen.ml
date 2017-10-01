(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

(* -------------------------------------------------------------------------- *)
(* TOOLS *)

(* Collection of functions that return expressions *)
module ExprBuilder = struct
	let make_static_this c p =
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		mk (TTypeExpr (TClassDecl c)) ta p

	let make_typeexpr mt pos =
		let t =
			match mt with
			| TClassDecl c -> TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) }
			| TEnumDecl e -> TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }
			| TAbstractDecl a -> TAnon { a_fields = PMap.empty; a_status = ref (AbstractStatics a) }
			| _ -> assert false
		in
		mk (TTypeExpr mt) t pos

	let make_static_field c cf p =
		let e_this = make_static_this c p in
		mk (TField(e_this,FStatic(c,cf))) cf.cf_type p

	let make_throw e p =
		mk (TThrow e) t_dynamic p

	let make_int com i p =
		mk (TConst (TInt (Int32.of_int i))) com.basic.tint p

	let make_float com f p =
		mk (TConst (TFloat f)) com.basic.tfloat p

	let make_bool com b p =
		mk (TConst(TBool b)) com.basic.tbool p

	let make_string com s p =
		mk (TConst (TString s)) com.basic.tstring p

	let make_null t p =
		mk (TConst TNull) t p

	let make_local v p =
		mk (TLocal v) v.v_type p

	let make_const_texpr com ct p = match ct with
		| TString s -> mk (TConst (TString s)) com.basic.tstring p
		| TInt i -> mk (TConst (TInt i)) com.basic.tint p
		| TFloat f -> mk (TConst (TFloat f)) com.basic.tfloat p
		| TBool b -> mk (TConst (TBool b)) com.basic.tbool p
		| TNull -> mk (TConst TNull) (com.basic.tnull (mk_mono())) p
		| _ -> error "Unsupported constant" p
end

let field e name t p =
	mk (TField (e,try quick_field e.etype name with Not_found -> assert false)) t p

let fcall e name el ret p =
	let ft = tfun (List.map (fun e -> e.etype) el) ret in
	mk (TCall (field e name ft p,el)) ret p

let mk_parent e =
	mk (TParenthesis e) e.etype e.epos

let mk_return e =
	mk (TReturn (Some e)) t_dynamic e.epos

let binop op a b t p =
	mk (TBinop (op,a,b)) t p

let index com e index t p =
	mk (TArray (e,mk (TConst (TInt (Int32.of_int index))) com.basic.tint p)) t p

let maybe_cast e t =
	try
		type_eq EqDoNotFollowNull e.etype t;
		e
	with
		Unify_error _ -> mk (TCast(e,None)) t e.epos

let type_constant com c p =
	let t = com.basic in
	match c with
	| Int s ->
		if String.length s > 10 && String.sub s 0 2 = "0x" then error "Invalid hexadecimal integer" p;
		(try mk (TConst (TInt (Int32.of_string s))) t.tint p
		with _ -> mk (TConst (TFloat s)) t.tfloat p)
	| Float f -> mk (TConst (TFloat f)) t.tfloat p
	| String (s,_) -> mk (TConst (TString s)) t.tstring p
	| Ident "true" -> mk (TConst (TBool true)) t.tbool p
	| Ident "false" -> mk (TConst (TBool false)) t.tbool p
	| Ident "null" -> mk (TConst TNull) (t.tnull (mk_mono())) p
	| Ident t -> error ("Invalid constant :  " ^ t) p
	| Regexp _ -> error "Invalid constant" p

let rec type_constant_value com (e,p) =
	match e with
	| EConst c ->
		type_constant com c p
	| EParenthesis e ->
		type_constant_value com e
	| EObjectDecl el ->
		mk (TObjectDecl (List.map (fun (k,e) -> k,type_constant_value com e) el)) (TAnon { a_fields = PMap.empty; a_status = ref Closed }) p
	| EArrayDecl el ->
		mk (TArrayDecl (List.map (type_constant_value com) el)) (com.basic.tarray t_dynamic) p
	| _ ->
		error "Constant value expected" p

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
			PMap.add n cf fields,((n,null_pos,NoQuotes),ExprBuilder.make_string com v p) :: values
		) (PMap.empty,[]) props in
		let t = mk_anon fields in
		let e = mk (TObjectDecl values) t p in
		let cf = mk_field "__properties__" t p null_pos in
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
(* BUILD META DATA OBJECT *)

let build_metadata com t =
	let api = com.basic in
	let p, meta, fields, statics = (match t with
		| TClassDecl c ->
			let fields = List.map (fun f -> f.cf_name,f.cf_meta) (c.cl_ordered_fields @ (match c.cl_constructor with None -> [] | Some f -> [{ f with cf_name = "_" }])) in
			let statics =  List.map (fun f -> f.cf_name,f.cf_meta) c.cl_ordered_statics in
			(c.cl_pos, ["",c.cl_meta],fields,statics)
		| TEnumDecl e ->
			(e.e_pos, ["",e.e_meta],List.map (fun n -> n, (PMap.find n e.e_constrs).ef_meta) e.e_names, [])
		| TTypeDecl t ->
			(t.t_pos, ["",t.t_meta],(match follow t.t_type with TAnon a -> PMap.fold (fun f acc -> (f.cf_name,f.cf_meta) :: acc) a.a_fields [] | _ -> []),[])
		| TAbstractDecl a ->
			(a.a_pos, ["",a.a_meta],[],[])
	) in
	let filter l =
		let l = List.map (fun (n,ml) -> n, ExtList.List.filter_map (fun (m,el,p) -> match m with Meta.Custom s when String.length s > 0 && s.[0] <> ':' -> Some (s,el,p) | _ -> None) ml) l in
		List.filter (fun (_,ml) -> ml <> []) l
	in
	let meta, fields, statics = filter meta, filter fields, filter statics in
	let make_meta_field ml =
		let h = Hashtbl.create 0 in
		mk (TObjectDecl (List.map (fun (f,el,p) ->
			if Hashtbl.mem h f then error ("Duplicate metadata '" ^ f ^ "'") p;
			Hashtbl.add h f ();
			(f,null_pos,NoQuotes), mk (match el with [] -> TConst TNull | _ -> TArrayDecl (List.map (type_constant_value com) el)) (api.tarray t_dynamic) p
		) ml)) t_dynamic p
	in
	let make_meta l =
		mk (TObjectDecl (List.map (fun (f,ml) -> (f,null_pos,NoQuotes),make_meta_field ml) l)) t_dynamic p
	in
	if meta = [] && fields = [] && statics = [] then
		None
	else
		let meta_obj = [] in
		let meta_obj = (if fields = [] then meta_obj else (("fields",null_pos,NoQuotes),make_meta fields) :: meta_obj) in
		let meta_obj = (if statics = [] then meta_obj else (("statics",null_pos,NoQuotes),make_meta statics) :: meta_obj) in
		let meta_obj = (try (("obj",null_pos,NoQuotes), make_meta_field (List.assoc "" meta)) :: meta_obj with Not_found -> meta_obj) in
		Some (mk (TObjectDecl meta_obj) t_dynamic p)

let update_cache_dependencies t =
	let rec check_t m t = match t with
		| TInst(c,tl) ->
			add_dependency m c.cl_module;
			List.iter (check_t m) tl;
		| TEnum(en,tl) ->
			add_dependency m en.e_module;
			List.iter (check_t m) tl;
		| TType(t,tl) ->
			add_dependency m t.t_module;
			List.iter (check_t m) tl;
		| TAbstract(a,tl) ->
			add_dependency m a.a_module;
			List.iter (check_t m) tl;
		| TFun(targs,tret) ->
			List.iter (fun (_,_,t) -> check_t m t) targs;
			check_t m tret;
		| TAnon an ->
			PMap.iter (fun _ cf -> check_field m cf) an.a_fields
		| TMono r ->
			(match !r with
			| Some t -> check_t m t
			| _ -> ())
		| TLazy f ->
			check_t m (lazy_type f)
		| TDynamic t ->
			if t == t_dynamic then
				()
			else
				check_t m t
	and check_field m cf =
		check_t m cf.cf_type
	in
	match t with
		| TClassDecl c ->
			List.iter (check_field c.cl_module) c.cl_ordered_statics;
			List.iter (check_field c.cl_module) c.cl_ordered_fields;
			(match c.cl_constructor with None -> () | Some cf -> check_field c.cl_module cf);
		| _ ->
			()

(* -------------------------------------------------------------------------- *)
(* STACK MANAGEMENT EMULATION *)

type stack_context = {
	stack_var : string;
	stack_exc_var : string;
	stack_pos_var : string;
	stack_pos : pos;
	stack_expr : texpr;
	stack_pop : texpr;
	stack_save_pos : texpr;
	stack_restore : texpr list;
	stack_push : tclass -> string -> texpr;
	stack_return : texpr -> texpr;
}

let stack_context_init com stack_var exc_var pos_var tmp_var use_add p =
	let t = com.basic in
	let st = t.tarray t.tstring in
	let stack_var = alloc_var stack_var st p in
	let exc_var = alloc_var exc_var st p in
	let pos_var = alloc_var pos_var t.tint p in
	let stack_e = mk (TLocal stack_var) st p in
	let exc_e = mk (TLocal exc_var) st p in
	let stack_pop = fcall stack_e "pop" [] t.tstring p in
	let stack_push c m =
		fcall stack_e "push" [
			if use_add then
				binop OpAdd (ExprBuilder.make_string com (s_type_path c.cl_path ^ "::") p) (ExprBuilder.make_string com m p) t.tstring p
			else
				ExprBuilder.make_string com (s_type_path c.cl_path ^ "::" ^ m) p
		] t.tvoid p
	in
	let stack_return e =
		let tmp = alloc_var tmp_var e.etype e.epos in
		mk (TBlock [
			mk (TVar (tmp, Some e)) t.tvoid e.epos;
			stack_pop;
			mk (TReturn (Some (mk (TLocal tmp) e.etype e.epos))) e.etype e.epos
		]) e.etype e.epos
	in
	{
		stack_var = stack_var.v_name;
		stack_exc_var = exc_var.v_name;
		stack_pos_var = pos_var.v_name;
		stack_pos = p;
		stack_expr = stack_e;
		stack_pop = stack_pop;
		stack_save_pos = mk (TVar (pos_var, Some (field stack_e "length" t.tint p))) t.tvoid p;
		stack_push = stack_push;
		stack_return = stack_return;
		stack_restore = [
			binop OpAssign exc_e (mk (TArrayDecl []) st p) st p;
			mk (TWhile (
				mk_parent (binop OpGte (field stack_e "length" t.tint p) (mk (TLocal pos_var) t.tint p) t.tbool p),
				fcall exc_e "unshift" [fcall stack_e "pop" [] t.tstring p] t.tvoid p,
				NormalWhile
			)) t.tvoid p;
			fcall stack_e "push" [index com exc_e 0 t.tstring p] t.tvoid p
		];
	}

let stack_init com use_add =
	stack_context_init com "$s" "$e" "$spos" "$tmp" use_add null_pos

let rec stack_block_loop ctx e =
	match e.eexpr with
	| TFunction _ ->
		e
	| TReturn None | TReturn (Some { eexpr = TConst _ }) | TReturn (Some { eexpr = TLocal _ }) ->
		mk (TBlock [
			ctx.stack_pop;
			e;
		]) e.etype e.epos
	| TReturn (Some e) ->
		ctx.stack_return (stack_block_loop ctx e)
	| TTry (v,cases) ->
		let v = stack_block_loop ctx v in
		let cases = List.map (fun (v,e) ->
			let e = stack_block_loop ctx e in
			let e = (match (mk_block e).eexpr with
				| TBlock l -> mk (TBlock (ctx.stack_restore @ l)) e.etype e.epos
				| _ -> assert false
			) in
			v , e
		) cases in
		mk (TTry (v,cases)) e.etype e.epos
	| _ ->
		map_expr (stack_block_loop ctx) e

let stack_block ctx c m e =
	match (mk_block e).eexpr with
	| TBlock l ->
		mk (TBlock (
			ctx.stack_push c m ::
			ctx.stack_save_pos ::
			List.map (stack_block_loop ctx) l
			@ [ctx.stack_pop]
		)) e.etype e.epos
	| _ ->
		assert false

(* -------------------------------------------------------------------------- *)
(* FIX OVERRIDES *)

(*
	on some platforms which doesn't support type parameters, we must have the
	exact same type for overriden/implemented function as the original one
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
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			let changed_args = ref [] in
			let prefix = "_tmp_" in
			let nargs = List.map2 (fun ((v,ct) as cur) (_,_,t2) ->
				try
					type_eq EqStrict (monomorphs c.cl_params (monomorphs f.cf_params v.v_type)) t2;
					(* Flash generates type parameters with a single constraint as that constraint type, so we
					   have to detect this case and change the variable (issue #2712). *)
					begin match follow v.v_type with
						| TInst({cl_kind = KTypeParameter [tc]} as cp,_) when com.platform = Flash ->
							if List.mem_assoc (snd cp.cl_path) c.cl_params then raise (Unify_error [])
						| _ ->
							()
					end;
					cur
				with Unify_error _ ->
					let v2 = alloc_var (prefix ^ v.v_name) t2 v.v_pos in
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
			(* as3 does not allow wider visibility, so the base method has to be made public *)
			if Common.defined com Define.As3 && f.cf_public then f2.cf_public <- true;
			let targs = List.map (fun(v,c) -> (v.v_name, Option.is_some c, v.v_type)) nargs in
			let fde = (match f.cf_expr with None -> assert false | Some e -> e) in
			f.cf_expr <- Some { fde with eexpr = TFunction fd2 };
			f.cf_type <- TFun(targs,tret);
		| Some(f2), None when c.cl_interface ->
			let targs, tret = (match follow f2.cf_type with TFun (args,ret) -> args, ret | _ -> assert false) in
			f.cf_type <- TFun(targs,tret)
		| _ ->
			()

let fix_overrides com t =
	match t with
	| TClassDecl c ->
		(* overrides can be removed from interfaces *)
		if c.cl_interface then
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
			| None, Method (MethNormal | MethInline) when c.cl_interface ->
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
	| TClassDecl c when c.cl_interface ->
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
		(match !r with
		| Some t -> is_volatile t
		| _ -> false)
	| TLazy f ->
		is_volatile (lazy_type f)
	| TType (t,tl) ->
		(match t.t_path with
		| _ -> is_volatile (apply_params t.t_params tl t.t_type))
	| _ ->
		false

let set_default ctx a c p =
	let t = a.v_type in
	let ve = mk (TLocal a) t p in
	let cond =  TBinop (OpEq,ve,mk (TConst TNull) t p) in
	mk (TIf (mk_parent (mk cond ctx.basic.tbool p), mk (TBinop (OpAssign,ve,mk (TConst c) t p)) t p,None)) ctx.basic.tvoid p

let bytes_serialize data =
	let b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
	let tbl = Array.init (String.length b64) (fun i -> String.get b64 i) in
	Bytes.unsafe_to_string (Base64.str_encode ~tbl data)

(*
	Tells if the constructor might be called without any issue whatever its parameters
*)
let rec constructor_side_effects e =
	match e.eexpr with
	| TBinop (op,_,_) when op <> OpAssign ->
		true
	| TField (_,FEnum _) ->
		false
	| TUnop _ | TArray _ | TField _ | TEnumParameter _ | TEnumIndex _ | TCall _ | TNew _ | TFor _ | TWhile _ | TSwitch _ | TReturn _ | TThrow _ ->
		true
	| TBinop _ | TTry _ | TIf _ | TBlock _ | TVar _
	| TFunction _ | TArrayDecl _ | TObjectDecl _
	| TParenthesis _ | TTypeExpr _ | TLocal _ | TMeta _
	| TConst _ | TContinue | TBreak | TCast _ | TIdent _ ->
		try
			Type.iter (fun e -> if constructor_side_effects e then raise Exit) e;
			false;
		with Exit ->
			true

module Dump = struct
	let make_valid_filename s =
		let r = Str.regexp "[^A-Za-z0-9_\\-\\.,]" in
		Str.global_substitute r (fun s -> "_") s

	let rec create_file ext acc = function
		| [] -> assert false
		| d :: [] ->
			let d = make_valid_filename d in
			let maxlen = 200 - String.length ext in
			let d = if String.length d > maxlen then String.sub d 0 maxlen else d in
			let ch = open_out (String.concat "/" (List.rev (d :: acc)) ^ ext) in
			ch
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create_file ext (d :: acc) l

	(*
		Make a dump of the full typed AST of all types
	*)
	let create_dumpfile acc l =
		let ch = create_file ".dump" acc l in
		let buf = Buffer.create 0 in
		buf, (fun () ->
			output_string ch (Buffer.contents buf);
			close_out ch)

	let create_dumpfile_from_path com path =
		let buf,close = create_dumpfile [] ("dump" :: (platform_name com.platform) :: fst path @ [snd path]) in
		buf,close

	let dump_types com s_expr =
		let s_type = s_type (Type.print_context()) in
		let params tl = match tl with [] -> "" | l -> Printf.sprintf "<%s>" (String.concat "," (List.map (fun (n,t) -> n ^ " : " ^ s_type t) l)) in
		List.iter (fun mt ->
			let path = Type.t_path mt in
			let buf,close = create_dumpfile_from_path com path in
			let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
			let s_metas ml tabs =
				let args el =
					match el with
					| [] -> ""
					| el -> Printf.sprintf "(%s)" (String.concat ", " (List.map (fun e -> Ast.s_expr e) el)) in
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
						(if (f.cf_public && not (c.cl_extern || c.cl_interface)) then "public " else "")
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
						| Method m -> if (c.cl_extern || c.cl_interface) then (
							match f.cf_type with
							| TFun(al,t) -> print "(%s):%s;" (String.concat ", " (
								List.map (fun (n,o,t) -> n ^ ":" ^ (s_type t)) al))
								(s_type t)
							| _ -> ()
						) else print "%s" (s_cf_expr f));
					print "\n";
					List.iter (fun f -> print_field stat f) f.cf_overloads
				in
				print "%s%s%s%s %s%s" (s_metas c.cl_meta "") (if c.cl_private then "private " else "") (if c.cl_extern then "extern " else "") (if c.cl_interface then "interface" else "class") (s_type_path path) (params c.cl_params);
				(match c.cl_super with None -> () | Some (c,pl) -> print " extends %s" (s_type (TInst (c,pl))));
				List.iter (fun (c,pl) -> print " implements %s" (s_type (TInst (c,pl)))) c.cl_implements;
				(match c.cl_dynamic with None -> () | Some t -> print " implements Dynamic<%s>" (s_type t));
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

	let dump_types com =
		match Common.defined_value_safe com Define.Dump with
			| "pretty" -> dump_types com (Type.s_expr_pretty false "\t" true)
			| "legacy" -> dump_types com Type.s_expr
			| "record" -> dump_record com
			| _ -> dump_types com (Type.s_expr_ast (not (Common.defined com Define.DumpIgnoreVarIds)) "\t")

	let dump_dependencies ?(target_override=None) com =
		let target_name = match target_override with
			| None -> platform_name com.platform
			| Some s -> s
		in
		let buf,close = create_dumpfile [] ["dump";target_name;".dependencies"] in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		let dep = Hashtbl.create 0 in
		List.iter (fun m ->
			print "%s:\n" m.m_extra.m_file;
			PMap.iter (fun _ m2 ->
				print "\t%s\n" (m2.m_extra.m_file);
				let l = try Hashtbl.find dep m2.m_extra.m_file with Not_found -> [] in
				Hashtbl.replace dep m2.m_extra.m_file (m :: l)
			) m.m_extra.m_deps;
		) com.Common.modules;
		close();
		let buf,close = create_dumpfile [] ["dump";target_name;".dependants"] in
		let print fmt = Printf.kprintf (fun s -> Buffer.add_string buf s) fmt in
		Hashtbl.iter (fun n ml ->
			print "%s:\n" n;
			List.iter (fun m ->
				print "\t%s\n" (m.m_extra.m_file);
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
		| TClassDecl c -> TAnon { a_fields = PMap.empty; a_status = ref (Statics c) }
		| TEnumDecl e -> TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }
		| TAbstractDecl a -> TAnon { a_fields = PMap.empty; a_status = ref (AbstractStatics a) }
		| TTypeDecl _ -> assert false
	in
	let vtmp = alloc_var vtmp e.etype e.epos in
	let var = mk (TVar (vtmp,Some e)) api.tvoid p in
	let vexpr = mk (TLocal vtmp) e.etype p in
	let texpr = mk (TTypeExpr texpr) (mk_texpr texpr) p in
	let std = (try List.find (fun t -> t_path t = ([],"Std")) com.types with Not_found -> assert false) in
	let fis = (try
			let c = (match std with TClassDecl c -> c | _ -> assert false) in
			FStatic (c, PMap.find "is" c.cl_statics)
		with Not_found ->
			assert false
	) in
	let std = mk (TTypeExpr std) (mk_texpr std) p in
	let is = mk (TField (std,fis)) (tfun [t_dynamic;t_dynamic] api.tbool) p in
	let is = mk (TCall (is,[vexpr;texpr])) api.tbool p in
	let exc = mk (TThrow (mk (TConst (TString "Class cast error")) api.tstring p)) t p in
	let check = mk (TIf (mk_parent is,mk (TCast (vexpr,None)) t p,Some exc)) t p in
	mk (TBlock [var;check;vexpr]) t p

module UnificationCallback = struct
	let tf_stack = ref []

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

	let rec run ff e =
		let f e t =
			if not (type_iseq e.etype t) then
				ff e t
			else
				e
		in
		let check e = match e.eexpr with
			| TBinop((OpAssign | OpAssignOp _),e1,e2) ->
				assert false; (* this trigger #4347, to be fixed before enabling
				let e2 = f e2 e1.etype in
				{e with eexpr = TBinop(op,e1,e2)} *)
			| TVar(v,Some ev) ->
				let eo = Some (f ev v.v_type) in
				{ e with eexpr = TVar(v,eo) }
			| TCall(e1,el) ->
				let el = check_call f el e1.etype in
				{e with eexpr = TCall(e1,el)}
			| TNew(c,tl,el) ->
				begin try
					let tcf,_ = get_constructor (fun cf -> apply_params c.cl_params tl cf.cf_type) c in
					let el = check_call f el tcf in
					{e with eexpr = TNew(c,tl,el)}
				with Not_found ->
					e
				end
			| TArrayDecl el ->
				begin match follow e.etype with
					| TInst({cl_path=[],"Array"},[t]) -> {e with eexpr = TArrayDecl(List.map (fun e -> f e t) el)}
					| _ -> e
				end
			| TObjectDecl fl ->
				begin match follow e.etype with
					| TAnon an ->
						let fl = List.map (fun ((n,p,qs),e) ->
							let e = try
								let t = (PMap.find n an.a_fields).cf_type in
								f e t
							with Not_found ->
								e
							in
							(n,p,qs),e
						) fl in
						{ e with eexpr = TObjectDecl fl }
					| _ -> e
				end
			| TReturn (Some e1) ->
				begin match !tf_stack with
					| tf :: _ -> { e with eexpr = TReturn (Some (f e1 tf.tf_type))}
					| _ -> e
				end
			| _ ->
				e
		in
		match e.eexpr with
			| TFunction tf ->
				tf_stack := tf :: !tf_stack;
				let etf = {e with eexpr = TFunction({tf with tf_expr = run f tf.tf_expr})} in
				tf_stack := List.tl !tf_stack;
				etf
			| _ ->
				check (Type.map_expr (run ff) e)
end;;

let interpolate_code com code tl f_string f_expr p =
	let exprs = Array.of_list tl in
	let i = ref 0 in
	let err msg =
		let pos = { p with pmin = p.pmin + !i } in
		com.error msg pos
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
		let ethis = ExprBuilder.make_static_this c p in
		let ef1 = mk (TField(ethis,FStatic(c,cf))) cf.cf_type p in
		let e_assign = mk (TBinop(OpAssign,ef1,e)) e.etype p in
		add_cl_init c e_assign
end

let for_remap com v e1 e2 p =
	let v' = alloc_var v.v_name e1.etype e1.epos in
	let ev' = mk (TLocal v') e1.etype e1.epos in
	let t1 = (Abstract.follow_with_abstracts e1.etype) in
	let ehasnext = mk (TField(ev',quick_field t1 "hasNext")) (tfun [] com.basic.tbool) e1.epos in
	let ehasnext = mk (TCall(ehasnext,[])) com.basic.tbool ehasnext.epos in
	let enext = mk (TField(ev',quick_field t1 "next")) (tfun [] v.v_type) e1.epos in
	let enext = mk (TCall(enext,[])) v.v_type e1.epos in
	let eassign = mk (TVar(v,Some enext)) com.basic.tvoid p in
	let ebody = Type.concat eassign e2 in
	mk (TBlock [
		mk (TVar (v',Some e1)) com.basic.tvoid e1.epos;
		mk (TWhile((mk (TParenthesis ehasnext) ehasnext.etype ehasnext.epos),ebody,NormalWhile)) com.basic.tvoid e1.epos;
	]) com.basic.tvoid p