(*
 * Copyright (C)2005-2013 Haxe Foundation
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

open Ast
open Type
open Common
open Typecore

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

type switch_mode =
	| CMatch of (tenum_field * (string * t) option list option * pos)
	| CExpr of texpr

type access_mode =
	| MGet
	| MSet
	| MCall

exception DisplayTypes of t list
exception DisplayFields of (string * t * documentation) list
exception DisplayMetadata of metadata_entry list
exception WithTypeError of unify_error list * pos

type access_kind =
	| AKNo of string
	| AKExpr of texpr
	| AKField of texpr * tclass_field * tfield_access
	| AKSet of texpr * string * t * string
	| AKInline of texpr * tclass_field * tfield_access * t
	| AKMacro of texpr * tclass_field
	| AKUsing of texpr * tclass * tclass_field * texpr
	| AKAccess of texpr * texpr

let mk_infos ctx p params =
	let file = if ctx.in_macro then p.pfile else if Common.defined ctx.com Define.AbsolutePath then Common.get_full_path p.pfile else Filename.basename p.pfile in
	(EObjectDecl (
		("fileName" , (EConst (String file) , p)) ::
		("lineNumber" , (EConst (Int (string_of_int (Lexer.get_error_line p))),p)) ::
		("className" , (EConst (String (s_type_path ctx.curclass.cl_path)),p)) ::
		if ctx.curfield.cf_name = "" then
			params
		else
			("methodName", (EConst (String ctx.curfield.cf_name),p)) :: params
	) ,p)

let check_assign ctx e =
	match e.eexpr with
	| TLocal _ | TArray _ | TField _ ->
		()
	| TConst TThis | TTypeExpr _ when ctx.untyped ->
		()
	| _ ->
		error "Invalid assign" e.epos

type type_class =
	| KInt
	| KFloat
	| KString
	| KUnk
	| KDyn
	| KOther
	| KParam of t
	| KAbstract of tabstract

let rec classify t =
	match follow t with
	| TInst ({ cl_path = ([],"Int") },[]) -> KInt
	| TInst ({ cl_path = ([],"Float") },[]) -> KFloat
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TAbstract({a_impl = Some _} as a,_) -> KAbstract a
	| TAbstract ({ a_path = [],"Int" },[]) -> KInt
	| TAbstract ({ a_path = [],"Float" },[]) -> KFloat
	| TAbstract (a,[]) when List.exists (fun (t,_) -> match classify t with KInt | KFloat -> true | _ -> false) a.a_to -> KParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) ctl -> KParam t
	| TMono r when !r = None -> KUnk
	| TDynamic _ -> KDyn
	| _ -> KOther

let object_field f =
	let pf = Parser.quoted_ident_prefix in
	let pflen = String.length pf in
	if String.length f >= pflen && String.sub f 0 pflen = pf then String.sub f pflen (String.length f - pflen), false else f, true

let rec is_pos_infos = function
	| TMono r ->
		(match !r with
		| Some t -> is_pos_infos t
		| _ -> false)
	| TLazy f ->
		is_pos_infos (!f())
	| TType ({ t_path = ["haxe"] , "PosInfos" },[]) ->
		true
	| TType (t,tl) ->
		is_pos_infos (apply_params t.t_types tl t.t_type)
	| _ ->
		false

let add_constraint_checks ctx ctypes pl f tl p =
	List.iter2 (fun m (name,t) ->
		match follow t with
		| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
			let constr = List.map (fun t ->
				let t = apply_params f.cf_params tl t in
				(* only apply params if not static : in that case no param is passed *)
				let t = (if pl = [] then t else apply_params ctypes pl t) in
				t
			) constr in
			delay ctx PCheckConstraint (fun() ->
				List.iter (fun ct ->
					try
						Type.unify m ct
					with Unify_error l ->
						display_error ctx (error_msg (Unify (Constraint_failure (f.cf_name ^ "." ^ name) :: l))) p;
				) constr
			);
		| _ -> ()
	) tl f.cf_params

let field_type ctx c pl f p =
	match f.cf_params with
	| [] -> f.cf_type
	| l ->
		let monos = List.map (fun _ -> mk_mono()) l in
		if not (Meta.has Meta.Generic f.cf_meta) then add_constraint_checks ctx c.cl_types pl f monos p;
		apply_params l monos f.cf_type

let class_field ctx c pl name p =
	raw_class_field (fun f -> field_type ctx c pl f p) c name

(* checks if we can access to a given class field using current context *)
let rec can_access ctx c cf stat =
	if cf.cf_public then
		true
	else
	(* has metadata path *)
	let make_path c f =
		fst c.cl_path @ [snd c.cl_path; f.cf_name]
	in
	let rec expr_path acc e =
		match fst e with
		| EField (e,f) -> expr_path (f :: acc) e
		| EConst (Ident n) -> n :: acc
		| _ -> []
	in
	let rec chk_path psub pfull =
		match psub, pfull with
		| [], _ -> true
		| a :: l1, b :: l2 when a = b -> chk_path l1 l2
		| _ -> false
	in
	let has m c f path =
		let rec loop = function
			| (m2,[e],_) :: l when m = m2 ->
				let p = expr_path [] e in
				(p <> [] && chk_path p path) || loop l
			| _ :: l -> loop l
			| [] -> false
		in
		loop c.cl_meta || loop f.cf_meta
	in
	let cur_path = make_path ctx.curclass ctx.curfield in
	let is_constr = cf.cf_name = "new" in
	let rec loop c =
		(try
			(* if our common ancestor declare/override the field, then we can access it *)
			let f = if is_constr then (match c.cl_constructor with None -> raise Not_found | Some c -> c) else PMap.find cf.cf_name (if stat then c.cl_statics else c.cl_fields) in
			is_parent c ctx.curclass || has Meta.Allow c f cur_path
		with Not_found ->
			false
		)
		|| (match c.cl_super with
		| Some (csup,_) -> loop csup
		| None -> false)
		|| has Meta.Access ctx.curclass ctx.curfield (make_path c cf)
	in
	let b = loop c
	(* access is also allowed of we access a type parameter which is constrained to our (base) class *)
	|| (match c.cl_kind with
		| KTypeParameter tl ->
			List.exists (fun t -> match follow t with TInst(c,_) -> loop c | _ -> false) tl
		| _ -> false)
	|| (Meta.has Meta.PrivateAccess ctx.meta) in
	if b && Common.defined ctx.com Common.Define.As3 && not (Meta.has Meta.Public cf.cf_meta) then cf.cf_meta <- (Meta.Public,[],cf.cf_pos) :: cf.cf_meta;
	b

(* removes the first argument of the class field's function type and all its overloads *)
let prepare_using_field cf = match cf.cf_type with
	| TFun((_,_,tf) :: args,ret) ->
		let rec loop acc overloads = match overloads with
			| ({cf_type = TFun((_,_,tfo) :: args,ret)} as cfo) :: l ->
				let tfo = apply_params cfo.cf_params (List.map snd cf.cf_params) tfo in
				(* ignore overloads which have a different first argument *)
				if Type.type_iseq tf tfo then loop ({cfo with cf_type = TFun(args,ret)} :: acc) l else loop acc l
			| _ :: l ->
				loop acc l
			| [] ->
				acc
		in
		{cf with cf_overloads = loop [] cf.cf_overloads; cf_type = TFun(args,ret)}
	| _ -> cf

let find_array_access a pl c t1 t2 is_set =
	let ta = apply_params a.a_types pl a.a_this in
	let rec loop cfl = match cfl with
		| [] -> raise Not_found
		| cf :: cfl when not (Meta.has Meta.ArrayAccess cf.cf_meta) ->
			loop cfl
		| cf :: cfl ->
			match follow (apply_params a.a_types pl (monomorphs cf.cf_params cf.cf_type)) with
			| TFun([(_,_,tab);(_,_,ta1);(_,_,ta2)],r) as tf when is_set && type_iseq tab ta && type_iseq ta1 t1 && type_iseq ta2 t2 ->
				cf,tf,r
			| TFun([(_,_,tab);(_,_,ta1)],r) as tf when not is_set && type_iseq tab ta && type_iseq ta1 t1 ->
				cf,tf,r
			| _ -> loop cfl
	in
	loop a.a_array

let parse_string ctx s p inlined =
	let old = Lexer.save() in
	let old_file = (try Some (Hashtbl.find Lexer.all_files p.pfile) with Not_found -> None) in
	let old_display = !Parser.resume_display in
	let old_de = !Parser.display_error in
	let restore() =
		(match old_file with
		| None -> ()
		| Some f -> Hashtbl.replace Lexer.all_files p.pfile f);
		if not inlined then Parser.resume_display := old_display;
		Lexer.restore old;
		Parser.display_error := old_de
	in
	Lexer.init p.pfile;
	Parser.display_error := (fun e p -> raise (Parser.Error (e,p)));
	if not inlined then Parser.resume_display := null_pos;
	let _, decls = try
		Parser.parse ctx.com (Lexing.from_string s)
	with Parser.Error (e,pe) ->
		restore();
		error (Parser.error_msg e) (if inlined then pe else p)
	| Lexer.Error (e,pe) ->
		restore();
		error (Lexer.error_msg e) (if inlined then pe else p)
	in
	restore();
	match decls with
	| [(d,_)] -> d
	| _ -> assert false

let parse_expr_string ctx s p inl =
	let head = "class X{static function main() " in
	let head = (if p.pmin > String.length head then head ^ String.make (p.pmin - String.length head) ' ' else head) in
	let rec loop e = let e = Ast.map_expr loop e in (fst e,p) in
	match parse_string ctx (head ^ s ^ "}") p inl with
	| EClass { d_data = [{ cff_name = "main"; cff_kind = FFun { f_expr = Some e } }]} -> if inl then e else loop e
	| _ -> assert false

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let rec base_types t =
	let tl = ref [] in
	let rec loop t = (match t with
	| TInst(cl, params) ->
		(match cl.cl_kind with
		| KTypeParameter tl -> List.iter loop tl
		| _ -> ());
		List.iter (fun (ic, ip) ->
			let t = apply_params cl.cl_types params (TInst (ic,ip)) in
			loop t
		) cl.cl_implements;
		(match cl.cl_super with None -> () | Some (csup, pl) ->
			let t = apply_params cl.cl_types params (TInst (csup,pl)) in
			loop t);
		tl := t :: !tl;
	| TType (td,pl) ->
		loop (apply_params td.t_types pl td.t_type);
		(* prioritize the most generic definition *)
		tl := t :: !tl;
	| TLazy f -> loop (!f())
	| TMono r -> (match !r with None -> () | Some t -> loop t)
	| _ -> tl := t :: !tl) in
	loop t;
	!tl

let rec unify_min_raise ctx (el:texpr list) : t =
	match el with
	| [] -> mk_mono()
	| [e] -> e.etype
	| _ ->
		let rec chk_null e = is_null e.etype ||
			match e.eexpr with
			| TConst TNull -> true
			| TBlock el ->
				(match List.rev el with
				| [] -> false
				| e :: _ -> chk_null e)
			| TParenthesis e -> chk_null e
			| _ -> false
		in

		(* First pass: Try normal unification and find out if null is involved. *)
		let rec loop t = function
			| [] ->
				false, t
			| e :: el ->
				let t = if chk_null e then ctx.t.tnull t else t in
				try
					unify_raise ctx e.etype t e.epos;
					loop t el
				with Error (Unify _,_) -> try
					unify_raise ctx t e.etype e.epos;
					loop (if is_null t then ctx.t.tnull e.etype else e.etype) el
				with Error (Unify _,_) ->
					true, t
		in
		let has_error, t = loop (mk_mono()) el in
		if not has_error then
			t
		else try
			(* specific case for const anon : we don't want to hide fields but restrict their common type *)
			let fcount = ref (-1) in
			let field_count a =
				PMap.fold (fun _ acc -> acc + 1) a.a_fields 0
			in
			let expr f = match f.cf_expr with None -> mk (TBlock []) f.cf_type f.cf_pos | Some e -> e in
			let fields = List.fold_left (fun acc e ->
				match follow e.etype with
				| TAnon a when !(a.a_status) = Const ->
					a.a_status := Closed;
					if !fcount = -1 then begin
						fcount := field_count a;
						PMap.map (fun f -> [expr f]) a.a_fields
					end else begin
						if !fcount <> field_count a then raise Not_found;
						PMap.mapi (fun n el -> expr (PMap.find n a.a_fields) :: el) acc
					end
				| _ ->
					raise Not_found
			) PMap.empty el in
			let fields = PMap.foldi (fun n el acc ->
				let t = try unify_min_raise ctx el with Error (Unify _, _) -> raise Not_found in
				PMap.add n (mk_field n t (List.hd el).epos) acc
			) fields PMap.empty in
			TAnon { a_fields = fields; a_status = ref Closed }
		with Not_found ->
			(* Second pass: Get all base types (interfaces, super classes and their interfaces) of most general type.
			   Then for each additional type filter all types that do not unify. *)
			let common_types = base_types t in
			let dyn_types = List.fold_left (fun acc t ->
				let rec loop c =
					Meta.has Meta.UnifyMinDynamic c.cl_meta || (match c.cl_super with None -> false | Some (c,_) -> loop c)
				in
				match t with
				| TInst (c,params) when params <> [] && loop c ->
					TInst (c,List.map (fun _ -> t_dynamic) params) :: acc
				| _ -> acc
			) [] common_types in
			let common_types = ref (match List.rev dyn_types with [] -> common_types | l -> common_types @ l) in
			let loop e =
				let first_error = ref None in
				let filter t = (try unify_raise ctx e.etype t e.epos; true
					with Error (Unify l, p) as err -> if !first_error = None then first_error := Some(err); false)
				in
				common_types := List.filter filter !common_types;
				match !common_types, !first_error with
				| [], Some err -> raise err
				| _ -> ()
			in
			match !common_types with
			| [] ->
				error "No common base type found" (punion (List.hd el).epos (List.hd (List.rev el)).epos)
			| _ ->
				List.iter loop (List.tl el);
				List.hd !common_types

let unify_min ctx el =
	try unify_min_raise ctx el
	with Error (Unify l,p) ->
		if not ctx.untyped then display_error ctx (error_msg (Unify l)) p;
		(List.hd el).etype

let rec unify_call_params ctx cf el args r p inline =
	let next() =
		match cf with
		| Some (TInst(c,pl),{ cf_overloads = o :: l }) ->
			let args, ret = (match field_type ctx c pl o p with
				| TFun (tl,t) -> tl, t
				| _ -> assert false
			) in
			Some (unify_call_params ctx (Some (TInst(c,pl),{ o with cf_overloads = l })) el args ret p inline)
		| Some (t,{ cf_overloads = o :: l }) ->
			let args, ret = (match Type.field_type o with
				| TFun (tl,t) -> tl, t
				| _ -> assert false
			) in
			Some (unify_call_params ctx (Some (t, { o with cf_overloads = l })) el args ret p inline)
		| _ ->
			None
	in
	let fun_details() =
		let format_arg = (fun (name,opt,_) -> (if opt then "?" else "") ^ name) in
		"Function " ^ (match cf with None -> "" | Some (_,f) -> "'" ^ f.cf_name ^ "' ") ^ "requires " ^ (if args = [] then "no arguments" else "arguments : " ^ String.concat ", " (List.map format_arg args))
	in
	let error acc txt =
		match next() with
		| Some l -> l
		| None ->
		display_error ctx (txt ^ " arguments\n" ^ (fun_details())) p;
		List.rev (List.map fst acc), (TFun(args,r))
	in
	let arg_error ul name opt p =
		match next() with
		| Some l -> l
		| None -> raise (Error (Stack (Unify ul,Custom ("For " ^ (if opt then "optional " else "") ^ "function argument '" ^ name ^ "'")), p))
	in
	let rec no_opt = function
		| [] -> []
		| ({ eexpr = TConst TNull },true) :: l -> no_opt l
		| l -> List.map fst l
	in
	let rec default_value t =
		if is_pos_infos t then
			let infos = mk_infos ctx p [] in
			let e = type_expr ctx infos (WithType t) in
			(e, true)
		else
			(null (ctx.t.tnull t) p, true)
	in
	let rec loop acc l l2 skip =
		match l , l2 with
		| [] , [] ->
			if not (inline && ctx.g.doinline) && not ctx.com.config.pf_pad_nulls then
				List.rev (no_opt acc), (TFun(args,r))
			else
				List.rev (List.map fst acc), (TFun(args,r))
		| [] , (_,false,_) :: _ ->
			error (List.fold_left (fun acc (_,_,t) -> default_value t :: acc) acc l2) "Not enough"
		| [] , (name,true,t) :: l ->
			loop (default_value t :: acc) [] l skip
		| _ , [] ->
			(match List.rev skip with
			| [] -> error acc "Too many"
			| [name,ul] -> arg_error ul name true p
			| (name,ul) :: _ -> arg_error (Unify_custom ("Invalid arguments\n" ^ fun_details()) :: ul) name true p)
		| ee :: l, (name,opt,t) :: l2 ->
			try
				let e = type_expr ctx ee (WithTypeResume t) in
				(try unify_raise ctx e.etype t e.epos with Error (Unify l,p) -> raise (WithTypeError (l,p)));
				loop ((e,false) :: acc) l l2 skip
			with
				WithTypeError (ul,p) ->
					if opt then
						loop (default_value t :: acc) (ee :: l) l2 ((name,ul) :: skip)
					else
						arg_error ul name false p
	in
	loop [] el args []

let fast_enum_field e ef p =
	let et = mk (TTypeExpr (TEnumDecl e)) (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }) p in
	TField (et,FEnum (e,ef))

let rec type_module_type ctx t tparams p =
	match t with
	| TClassDecl c ->
		let t_tmp = {
			t_path = fst c.cl_path, "#" ^ snd c.cl_path;
			t_module = c.cl_module;
			t_doc = None;
			t_pos = c.cl_pos;
			t_type = TAnon {
				a_fields = c.cl_statics;
				a_status = ref (Statics c);
			};
			t_private = true;
			t_types = [];
			t_meta = no_meta;
		} in
		mk (TTypeExpr (TClassDecl c)) (TType (t_tmp,[])) p
	| TEnumDecl e ->
		let types = (match tparams with None -> List.map (fun _ -> mk_mono()) e.e_types | Some l -> l) in
		let fl = PMap.fold (fun f acc ->
			PMap.add f.ef_name {
				cf_name = f.ef_name;
				cf_public = true;
				cf_type = f.ef_type;
				cf_kind = (match follow f.ef_type with
					| TFun _ -> Method MethNormal
					| _ -> Var { v_read = AccNormal; v_write = AccNo }
				);
				cf_pos = e.e_pos;
				cf_doc = None;
				cf_meta = no_meta;
				cf_expr = None;
				cf_params = [];
				cf_overloads = [];
			} acc
		) e.e_constrs PMap.empty in
		let t_tmp = {
			t_path = fst e.e_path, "#" ^ snd e.e_path;
			t_module = e.e_module;
			t_doc = None;
			t_pos = e.e_pos;
			t_type = TAnon {
				a_fields = fl;
				a_status = ref (EnumStatics e);
			};
			t_private = true;
			t_types = e.e_types;
			t_meta = no_meta;
		} in
		mk (TTypeExpr (TEnumDecl e)) (TType (t_tmp,types)) p
	| TTypeDecl s ->
		let t = apply_params s.t_types (List.map (fun _ -> mk_mono()) s.t_types) s.t_type in
		(match follow t with
		| TEnum (e,params) ->
			type_module_type ctx (TEnumDecl e) (Some params) p
		| TInst (c,params) ->
			type_module_type ctx (TClassDecl c) (Some params) p
		| TAbstract (a,params) ->
			type_module_type ctx (TAbstractDecl a) (Some params) p
		| _ ->
			error (s_type_path s.t_path ^ " is not a value") p)
	| TAbstractDecl { a_impl = Some c } ->
		type_module_type ctx (TClassDecl c) tparams p
	| TAbstractDecl a ->
		if not (Meta.has Meta.RuntimeValue a.a_meta) then error (s_type_path a.a_path ^ " is not a value") p;
		let t_tmp = {
			t_path = fst a.a_path, "#" ^ snd a.a_path;
			t_module = a.a_module;
			t_doc = None;
			t_pos = a.a_pos;
			t_type = TAnon {
				a_fields = PMap.empty;
				a_status = ref (AbstractStatics a);
			};
			t_private = true;
			t_types = [];
			t_meta = no_meta;
		} in
		mk (TTypeExpr (TAbstractDecl a)) (TType (t_tmp,[])) p

let type_type ctx tpath p =
	type_module_type ctx (Typeload.load_type_def ctx p { tpackage = fst tpath; tname = snd tpath; tparams = []; tsub = None }) None p

let get_constructor ctx c params p =
	match c.cl_kind with
	| KAbstractImpl a ->
		let f = (try PMap.find "_new" c.cl_statics with Not_found -> error (s_type_path a.a_path ^ " does not have a constructor") p) in
		let ct = field_type ctx c params f p in
		apply_params a.a_types params ct, f
	| _ ->
		let ct, f = (try Type.get_constructor (fun f -> field_type ctx c params f p) c with Not_found -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
		apply_params c.cl_types params ct, f

let make_call ctx e params t p =
	try
		let ethis, fname = (match e.eexpr with TField (ethis,f) -> ethis, field_name f | _ -> raise Exit) in
		let f, cl = (match follow ethis.etype with
			| TInst (c,params) -> (try let _,_,f = Type.class_field c fname in f with Not_found -> raise Exit), Some c
			| TAnon a -> (try PMap.find fname a.a_fields with Not_found -> raise Exit), (match !(a.a_status) with Statics c -> Some c | _ -> None)
			| _ -> raise Exit
		) in
		if f.cf_kind <> Method MethInline then raise Exit;
		let is_extern = (match cl with
			| Some { cl_extern = true } -> true
			| Some { cl_kind = KAbstractImpl _ } -> true
			| _ when Meta.has Meta.Extern f.cf_meta -> true
			| _ -> false
		) in
		let config = match cl with
			| Some ({cl_kind = KAbstractImpl _ }) when Meta.has Meta.Impl f.cf_meta ->
				(match if fname = "_new" then t else follow (List.hd params).etype with
					| TAbstract(a,pl) ->
						Some (a.a_types <> [] || f.cf_params <> [], fun t -> apply_params a.a_types pl (monomorphs f.cf_params t))
					| _ ->
						None);
			| _ ->
				None
		in
		ignore(follow f.cf_type); (* force evaluation *)
		let params = List.map (ctx.g.do_optimize ctx) params in
		(match f.cf_expr with
		| Some { eexpr = TFunction fd } ->
			(match Optimizer.type_inline ctx f fd ethis params t config p is_extern with
			| None ->
				if is_extern then error "Inline could not be done" p;
				raise Exit;
			| Some e -> e)
		| _ ->
			(*
				we can't inline because there is most likely a loop in the typing.
				this can be caused by mutually recursive vars/functions, some of them
				being inlined or not. In that case simply ignore inlining.
			*)
			raise Exit)
	with Exit ->
		mk (TCall (e,params)) t p

let rec acc_get ctx g p =
	match g with
	| AKNo f -> error ("Field " ^ f ^ " cannot be accessed for reading") p
	| AKExpr e | AKField (e,_,_) -> e
	| AKSet _ | AKAccess _ -> assert false
	| AKUsing (et,_,_,e) ->
		(* build a closure with first parameter applied *)
		(match follow et.etype with
		| TFun (_ :: args,ret) ->
			let tcallb = TFun (args,ret) in
			let twrap = TFun ([("_e",false,e.etype)],tcallb) in
			let args = List.map (fun (n,_,t) -> alloc_var n t) args in
			let ve = alloc_var "_e" e.etype in
			let ecall = make_call ctx et (List.map (fun v -> mk (TLocal v) v.v_type p) (ve :: args)) ret p in
			let ecallb = mk (TFunction {
				tf_args = List.map (fun v -> v,None) args;
				tf_type = ret;
				tf_expr = mk (TReturn (Some ecall)) t_dynamic p;
			}) tcallb p in
			let ewrap = mk (TFunction {
				tf_args = [ve,None];
				tf_type = tcallb;
				tf_expr = mk (TReturn (Some ecallb)) t_dynamic p;
			}) twrap p in
			make_call ctx ewrap [e] tcallb p
		| _ -> assert false)
	| AKInline (e,f,fmode,t) ->
		(* do not create a closure for static calls *)
		let cmode = (match fmode with FStatic _ -> fmode | FInstance (c,f) -> FClosure (Some c,f) | _ -> assert false) in
		ignore(follow f.cf_type); (* force computing *)
		(match f.cf_expr with
		| None ->
			if ctx.com.display then
				mk (TField (e,cmode)) t p
			else
				error "Recursive inline is not supported" p
		| Some { eexpr = TFunction _ } ->
			let chk_class c = if (c.cl_extern || Meta.has Meta.Extern f.cf_meta) && not (Meta.has Meta.Runtime f.cf_meta) then display_error ctx "Can't create closure on an inline extern method" p in
			(match follow e.etype with
			| TInst (c,_) -> chk_class c
			| TAnon a -> (match !(a.a_status) with Statics c -> chk_class c | _ -> ())
			| _ -> ());
			mk (TField (e,cmode)) t p
		| Some e ->
			let rec loop e = Type.map_expr loop { e with epos = p } in
			loop e)
	| AKMacro _ ->
		assert false

let error_require r p =
	let r = if r = "sys" then
		"a system platform (php,neko,cpp,etc.)"
	else try
		if String.sub r 0 5 <> "flash" then raise Exit;
		let _, v = ExtString.String.replace (String.sub r 5 (String.length r - 5)) "_" "." in
		"flash version " ^ v ^ " (use -swf-version " ^ v ^ ")"
	with _ ->
		"'" ^ r ^ "' to be enabled"
	in
	error ("Accessing this field requires " ^ r) p

let field_access ctx mode f fmode t e p =
	let fnormal() = AKField ((mk (TField (e,fmode)) t p),f,fmode) in
	let normal() =
		match follow e.etype with
		| TAnon a ->
			(match !(a.a_status) with
			| EnumStatics en ->
				let c = (try PMap.find f.cf_name en.e_constrs with Not_found -> assert false) in
				let fmode = FEnum (en,c) in
				AKField ((mk (TField (e,fmode)) t p),f, fmode)
			| _ -> fnormal())
		| _ -> fnormal()
	in
	match f.cf_kind with
	| Method m ->
		if mode = MSet && m <> MethDynamic && not ctx.untyped then error "Cannot rebind this method : please use 'dynamic' before method declaration" p;
		(match m, mode with
		| MethInline, _ -> AKInline (e,f,fmode,t)
		| MethMacro, MGet -> display_error ctx "Macro functions must be called immediately" p; normal()
		| MethMacro, MCall -> AKMacro (e,f)
		| _ , MGet ->
			let cmode = (match fmode with
				| FInstance (c,cf) -> FClosure (Some c,cf)
				| FStatic _ | FEnum _ -> fmode
				| FAnon f -> FClosure (None, f)
				| FDynamic _ | FClosure _ -> assert false
			) in
			AKExpr (mk (TField (e,cmode)) t p)
		| _ -> normal())
	| Var v ->
		match (match mode with MGet | MCall -> v.v_read | MSet -> v.v_write) with
		| AccNo ->
			(match follow e.etype with
			| TInst (c,_) when is_parent c ctx.curclass || can_access ctx c { f with cf_public = false } false -> normal()
			| TAnon a ->
				(match !(a.a_status) with
				| Opened when mode = MSet ->
					f.cf_kind <- Var { v with v_write = AccNormal };
					normal()
				| Statics c2 when ctx.curclass == c2 || can_access ctx c2 { f with cf_public = false } true -> normal()
				| _ -> if ctx.untyped then normal() else AKNo f.cf_name)
			| _ ->
				if ctx.untyped then normal() else AKNo f.cf_name)
		| AccNormal ->
			(*
				if we are reading from a read-only variable on an anonymous object, it might actually be a method, so make sure to create a closure
			*)
			let is_maybe_method() =
				match v.v_write, follow t, follow e.etype with
				| (AccNo | AccNever), TFun _, TAnon a ->
					(match !(a.a_status) with
					| Statics _ | EnumStatics _ -> false
					| _ -> true)
				| _ -> false
			in
			if mode = MGet && is_maybe_method() then
				AKExpr (mk (TField (e,FClosure (None,f))) t p)
			else
				normal()
		| AccCall m ->
			if m = ctx.curfield.cf_name && (match e.eexpr with TConst TThis -> true | TTypeExpr (TClassDecl c) when c == ctx.curclass -> true | _ -> false) then
				let prefix = (match ctx.com.platform with Flash when Common.defined ctx.com Define.As3 -> "$" | _ -> "") in
				if is_extern_field f then begin
					display_error ctx "This field cannot be accessed because it is not a real variable" p;
					display_error ctx "Add @:isVar here to enable it" f.cf_pos;
				end;
				AKExpr (mk (TField (e,if prefix = "" then fmode else FDynamic (prefix ^ f.cf_name))) t p)
			else if mode = MSet then
				AKSet (e,m,t,f.cf_name)
			else
				AKExpr (make_call ctx (mk (TField (e,FDynamic m)) (tfun [] t) p) [] t p)
		| AccResolve ->
			let fstring = mk (TConst (TString f.cf_name)) ctx.t.tstring p in
			let tresolve = tfun [ctx.t.tstring] t in
			AKExpr (make_call ctx (mk (TField (e,FDynamic "resolve")) tresolve p) [fstring] t p)
		| AccNever ->
			if ctx.untyped then normal() else AKNo f.cf_name
		| AccInline ->
			AKInline (e,f,fmode,t)
		| AccRequire (r,msg) ->
			match msg with
			| None -> error_require r p
			| Some msg -> error msg p

let using_field ctx mode e i p =
	if mode = MSet then raise Not_found;
	(* do not try to find using fields if the type is a monomorph, which could lead to side-effects *)
	let is_dynamic = match follow e.etype with
		| TMono _ -> raise Not_found
		| t -> t == t_dynamic
	in
	let rec loop = function
	| [] ->
		raise Not_found
	| c :: l ->
		try
			let cf = PMap.find i c.cl_statics in
			if Meta.has Meta.NoUsing cf.cf_meta then raise Not_found;
			let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
			let map = apply_params cf.cf_params monos in
			let t = map cf.cf_type in
			begin match follow t with
				| TFun((_,_,(TType({t_path = ["haxe";"macro"],"ExprOf"},[t0]) | t0)) :: args,r) ->
					if is_dynamic && follow t0 != t_dynamic then raise Not_found;
					Type.unify e.etype t0;
					(* early constraints check is possible because e.etype has no monomorphs *)
		 			List.iter2 (fun m (name,t) -> match follow t with
						| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
							List.iter (fun tc -> Type.unify m (map tc)) constr
						| _ -> ()
					) monos cf.cf_params;
					let et = type_module_type ctx (TClassDecl c) None p in
					AKUsing (mk (TField (et,FStatic (c,cf))) t p,c,cf,e)
				| _ ->
					raise Not_found
			end
		with Not_found | Unify_error _ ->
			loop l
	in
	try loop ctx.m.module_using
	with Not_found -> loop ctx.g.global_using

let get_this ctx p =
	match ctx.curfun with
	| FunStatic ->
		error "Cannot access this from a static function" p
	| FunMemberLocal ->
		let v = (match ctx.vthis with
			| None ->
				(* we might be in a closure of an abstract member, so check for local "this" first *)
				let v = try PMap.find "this" ctx.locals with Not_found -> gen_local ctx ctx.tthis in
				ctx.vthis <- Some v;
				v
			| Some v ->
				ctx.locals <- PMap.add v.v_name v ctx.locals;
				v
		) in
		mk (TLocal v) ctx.tthis p
	| FunMemberAbstract ->
		let v = (try PMap.find "this" ctx.locals with Not_found -> assert false) in
		mk (TLocal v) v.v_type p
	| FunConstructor | FunMember ->
		mk (TConst TThis) ctx.tthis p

let rec type_ident_raise ?(imported_enums=true) ctx i p mode =
	match i with
	| "true" ->
		if mode = MGet then
			AKExpr (mk (TConst (TBool true)) ctx.t.tbool p)
		else
			AKNo i
	| "false" ->
		if mode = MGet then
			AKExpr (mk (TConst (TBool false)) ctx.t.tbool p)
		else
			AKNo i
	| "this" ->
		(match mode, ctx.curclass.cl_kind with
		| MSet, KAbstractImpl _ ->
			(match ctx.curfield.cf_kind with
			| Method MethInline -> ()
			| Method _ when ctx.curfield.cf_name = "_new" -> ()
			| _ -> error "You can only modify 'this' inside an inline function" p);
			AKExpr (get_this ctx p)
		| _ ->
		if mode = MGet then
			AKExpr (get_this ctx p)
		else
			AKNo i)
	| "super" ->
		let t = (match ctx.curclass.cl_super with
			| None -> error "Current class does not have a superclass" p
			| Some (c,params) -> TInst(c,params)
		) in
		(match ctx.curfun with
		| FunMember | FunConstructor -> ()
		| FunMemberAbstract -> error "Cannot access super inside an abstract function" p
		| FunStatic -> error "Cannot access super inside a static function" p;
		| FunMemberLocal -> error "Cannot access super inside a local function" p);
		if mode <> MSet && ctx.in_super_call then ctx.in_super_call <- false;
		AKExpr (mk (TConst TSuper) t p)
	| "null" ->
		if mode = MGet then
			AKExpr (null (mk_mono()) p)
		else
			AKNo i
	| _ ->
	try
		let v = PMap.find i ctx.locals in
		(match v.v_extra with
		| Some (params,e) ->
			let t = monomorphs params v.v_type in
			(match e with
			| Some ({ eexpr = TFunction f } as e) ->
				(* create a fake class with a fake field to emulate inlining *)
				let c = mk_class ctx.m.curmod (["local"],v.v_name) e.epos in
				let cf = { (mk_field v.v_name v.v_type e.epos) with cf_params = params; cf_expr = Some e; cf_kind = Method MethInline } in
				c.cl_extern <- true;
				c.cl_fields <- PMap.add cf.cf_name cf PMap.empty;
				AKInline (mk (TConst TNull) (TInst (c,[])) p, cf, FInstance(c,cf), t)
			| _ ->
				AKExpr (mk (TLocal v) t p))
		| _ ->
			AKExpr (mk (TLocal v) v.v_type p))
	with Not_found -> try
		(* member variable lookup *)
		if ctx.curfun = FunStatic then raise Not_found;
		let c , t , f = class_field ctx ctx.curclass [] i p in
		field_access ctx mode f (match c with None -> FAnon f | Some c -> FInstance (c,f)) t (get_this ctx p) p
	with Not_found -> try
		(* lookup using on 'this' *)
		if ctx.curfun = FunStatic then raise Not_found;
		(match using_field ctx mode (mk (TConst TThis) ctx.tthis p) i p with
		| AKUsing (et,c,f,_) -> AKUsing (et,c,f,get_this ctx p)
		| _ -> assert false)
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		let e = type_type ctx ctx.curclass.cl_path p in
		(* check_locals_masking already done in type_type *)
		field_access ctx mode f (FStatic (ctx.curclass,f)) (field_type ctx ctx.curclass [] f p) e p
	with Not_found -> try
		if not imported_enums then raise Not_found;
		(* lookup imported enums *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| t :: l ->
				match t with
				| TClassDecl _ | TAbstractDecl _ ->
					loop l
				| TTypeDecl t ->
					(match follow t.t_type with
					| TEnum (e,_) -> loop ((TEnumDecl e) :: l)
					| _ -> loop l)
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						let et = type_module_type ctx t None p in
						mk (TField (et,FEnum (e,ef))) (monomorphs ef.ef_params (monomorphs e.e_types ef.ef_type)) p
					with
						Not_found -> loop l
		in
		let e = (try loop (List.rev ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_types) in
		if mode = MSet then
			AKNo i
		else
			AKExpr e
	with Not_found ->
		(* lookup imported globals *)
		let t, name = PMap.find i ctx.m.module_globals in
		let e = type_module_type ctx t None p in
		type_field ctx e name p mode

and type_field ctx e i p mode =
	let no_field() =
		let t = match follow e.etype with
			| TAnon a -> (match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a} -> TAbstract(a,[])
				| _ -> e.etype)
			| TInst({cl_kind = KAbstractImpl a},_) -> TAbstract(a,[])
			| _ -> e.etype
		in
		if not ctx.untyped then display_error ctx (string_error i (string_source t) (s_type (print_context()) t ^ " has no field " ^ i)) p;
		AKExpr (mk (TField (e,FDynamic i)) (mk_mono()) p)
	in
	match follow e.etype with
	| TInst (c,params) ->
		let rec loop_dyn c params =
			match c.cl_dynamic with
			| Some t ->
				let t = apply_params c.cl_types params t in
				if (mode = MGet || mode = MCall) && PMap.mem "resolve" c.cl_fields then begin
					let f = PMap.find "resolve" c.cl_fields in
					AKExpr (make_call ctx (mk (TField (e,FInstance (c,f))) (tfun [ctx.t.tstring] t) p) [Codegen.type_constant ctx.com (String i) p] t p)
				end else
					AKExpr (mk (TField (e,FDynamic i)) t p)
			| None ->
				match c.cl_super with
				| None -> raise Not_found
				| Some (c,params) -> loop_dyn c params
		in
		(try
			let c2, t , f = class_field ctx c params i p in
			if e.eexpr = TConst TSuper then (match mode,f.cf_kind with
				| MGet,Var {v_read = AccCall _}
				| MSet,Var {v_write = AccCall _}
				| MCall,Var {v_read = AccCall _} ->
					()
				| MCall, Var _ ->
					error "Cannot access superclass variable for calling: needs to be a proper method" p
				| MCall, _ ->
					()
				| MGet,Var _
				| MSet,Var _ when (match c2 with Some { cl_extern = true; cl_path = ("flash" :: _,_) } -> true | _ -> false) ->
					()
				| _, Method _ ->
					error "Cannot create closure on super method" p
				| _ ->
					error "Normal variables cannot be accessed with 'super', use 'this' instead" p);
			if not (can_access ctx c f false) && not ctx.untyped then display_error ctx ("Cannot access private field " ^ i) p;
			field_access ctx mode f (match c2 with None -> FAnon f | Some c -> FInstance (c,f)) (apply_params c.cl_types params t) e p
		with Not_found -> try
			using_field ctx mode e i p
		with Not_found -> try
			loop_dyn c params
		with Not_found ->
			if PMap.mem i c.cl_statics then error ("Cannot access static field " ^ i ^ " from a class instance") p;
			(*
				This is a fix to deal with optimize_completion which will call iterator()
				on the expression for/in, which vectors do no have.
			*)
			if ctx.com.display && i = "iterator" && c.cl_path = (["flash"],"Vector") then begin
				let it = TAnon {
					a_fields = PMap.add "next" (mk_field "next" (TFun([],List.hd params)) p) PMap.empty;
					a_status = ref Closed;
				} in
				AKExpr (mk (TField (e,FDynamic i)) (TFun([],it)) p)
			end else
			no_field())
	| TDynamic t ->
		(try
			using_field ctx mode e i p
		with Not_found ->
			AKExpr (mk (TField (e,FDynamic i)) t p))
	| TAnon a ->
		(try
			let f = PMap.find i a.a_fields in
			if not f.cf_public && not ctx.untyped then begin
				match !(a.a_status) with
				| Closed -> () (* always allow anon private fields access *)
				| Statics c when can_access ctx c f true -> ()
				| _ -> display_error ctx ("Cannot access private field " ^ i) p
			end;
			let fmode, ft = (match !(a.a_status) with
				| Statics c -> FStatic (c,f), field_type ctx c [] f p
				| EnumStatics e -> FEnum (e,try PMap.find f.cf_name e.e_constrs with Not_found -> assert false), Type.field_type f
				| _ ->
					match f.cf_params with
					| [] ->
						FAnon f, Type.field_type f
					| l ->
						(* handle possible constraints *)
						let monos = List.map (fun _ -> mk_mono()) l in
						let t = apply_params f.cf_params monos f.cf_type in
						add_constraint_checks ctx [] [] f monos p;
						FAnon f, t
			) in
			field_access ctx mode f fmode ft e p
		with Not_found ->
			if is_closed a then try
				using_field ctx mode e i p
			with Not_found ->
				no_field()
			else
			let f = {
				cf_name = i;
				cf_type = mk_mono();
				cf_doc = None;
				cf_meta = no_meta;
				cf_public = true;
				cf_pos = p;
				cf_kind = Var { v_read = AccNormal; v_write = (match mode with MSet -> AccNormal | MGet | MCall -> AccNo) };
				cf_expr = None;
				cf_params = [];
				cf_overloads = [];
			} in
			a.a_fields <- PMap.add i f a.a_fields;
			field_access ctx mode f (FAnon f) (Type.field_type f) e p
		)
	| TMono r ->
		if ctx.untyped && (match ctx.com.platform with Flash8 -> Common.defined ctx.com Define.SwfMark | _ -> false) then ctx.com.warning "Mark" p;
		let f = {
			cf_name = i;
			cf_type = mk_mono();
			cf_doc = None;
			cf_meta = no_meta;
			cf_public = true;
			cf_pos = p;
			cf_kind = Var { v_read = AccNormal; v_write = (match mode with MSet -> AccNormal | MGet | MCall -> AccNo) };
			cf_expr = None;
			cf_params = [];
			cf_overloads = [];
		} in
		let x = ref Opened in
		let t = TAnon { a_fields = PMap.add i f PMap.empty; a_status = x } in
		ctx.opened <- x :: ctx.opened;
		r := Some t;
		field_access ctx mode f (FAnon f) (Type.field_type f) e p
	| TAbstract (a,pl) ->
		(try
			let c = (match a.a_impl with None -> raise Not_found | Some c -> c) in
			let f = PMap.find i c.cl_statics in
			let t = field_type ctx c [] f p in
			let t = apply_params a.a_types pl t in
			if not (Meta.has Meta.Impl f.cf_meta) then (match follow t with
				| TFun((_,_,ta) :: _,_) -> unify ctx e.etype ta p
				| _ -> raise Not_found);
			let et = type_module_type ctx (TClassDecl c) None p in
			AKUsing ((mk (TField (et,FStatic (c,f))) t p),c,f,e)
		with Not_found -> try
			using_field ctx mode e i p
		with Not_found -> try
			(match ctx.curclass.cl_kind with
			| KAbstractImpl a2 when a == a2 -> type_field ctx {e with etype = apply_params a.a_types pl a.a_this} i p mode;
			| _ -> raise Not_found)
		with Not_found ->
			no_field())
	| _ ->
		try using_field ctx mode e i p with Not_found -> no_field()

let type_bind ctx (e : texpr) params p =
	let args,ret = match follow e.etype with TFun(args, ret) -> args, ret | _ -> error "First parameter of callback is not a function" p in
	let vexpr v = mk (TLocal v) v.v_type p in
	let acount = ref 0 in
	let alloc_name n =
		if n = "" || String.length n > 2 then begin
			incr acount;
			"a" ^ string_of_int !acount;
		end else
			n
	in
	let rec loop args params given_args missing_args ordered_args = match args, params with
		| [], [] -> given_args,missing_args,ordered_args
		| [], _ -> error "Too many callback arguments" p
		| (n,o,t) :: args , [] when o ->
			let a = if is_pos_infos t then
					let infos = mk_infos ctx p [] in
					ordered_args @ [type_expr ctx infos (WithType t)]
				else if ctx.com.config.pf_pad_nulls then
					(ordered_args @ [(mk (TConst TNull) t_dynamic p)])
				else
					ordered_args
			in
			loop args [] given_args missing_args a
		| (n,o,t) :: _ , (EConst(Ident "_"),p) :: _ when ctx.com.platform = Flash && o && not (is_nullable t) ->
			error "Usage of _ is currently not supported for optional non-nullable arguments on flash9" p
		| (n,o,t) :: args , ([] as params)
		| (n,o,t) :: args , (EConst(Ident "_"),_) :: params ->
			let v = alloc_var (alloc_name n) (if o then ctx.t.tnull t else t) in
			loop args params given_args (missing_args @ [v,o]) (ordered_args @ [vexpr v])
		| (n,o,t) :: args , param :: params ->
			let e = type_expr ctx param (WithType t) in
			unify ctx e.etype t p;
			let v = alloc_var (alloc_name n) t in
			loop args params (given_args @ [v,o,Some e]) missing_args (ordered_args @ [vexpr v])
	in
	let given_args,missing_args,ordered_args = loop args params [] [] [] in
	let rec gen_loc_name n =
		let name = if n = 0 then "f" else "f" ^ (string_of_int n) in
		if List.exists (fun (n,_,_) -> name = n) args then gen_loc_name (n + 1) else name
	in
	let loc = alloc_var (gen_loc_name 0) e.etype in
	let given_args = (loc,false,Some e) :: given_args in
	let inner_fun_args l = List.map (fun (v,o) -> v.v_name, o, v.v_type) l in
	let t_inner = TFun(inner_fun_args missing_args, ret) in
	let call = make_call ctx (vexpr loc) ordered_args ret p in
	let func = mk (TFunction {
		tf_args = List.map (fun (v,o) -> v, if o then Some TNull else None) missing_args;
		tf_type = ret;
		tf_expr = mk (TReturn (Some call)) ret p;
	}) t_inner p in
	let outer_fun_args l = List.map (fun (v,o,_) -> v.v_name, o, v.v_type) l in
	let func = mk (TFunction {
		tf_args = List.map (fun (v,_,_) -> v,None) given_args;
		tf_type = t_inner;
		tf_expr = mk (TReturn (Some func)) t_inner p;
	}) (TFun(outer_fun_args given_args, t_inner)) p in
	make_call ctx func (List.map (fun (_,_,e) -> (match e with Some e -> e | None -> assert false)) given_args) t_inner p

(*
	We want to try unifying as an integer and apply side effects.
	However, in case the value is not a normal Monomorph but one issued
	from a Dynamic relaxation, we will instead unify with float since
	we don't want to accidentaly truncate the value
*)
let unify_int ctx e k =
	let is_dynamic t =
		match follow t with
		| TDynamic _ -> true
		| _ -> false
	in
	let is_dynamic_array t =
		match follow t with
		| TInst (_,[p]) -> is_dynamic p
		| _ -> true
	in
	let is_dynamic_field t f =
		match follow t with
		| TAnon a ->
			(try is_dynamic (PMap.find f a.a_fields).cf_type with Not_found -> false)
		| TInst (c,pl) ->
			(try is_dynamic (apply_params c.cl_types pl ((let _,t,_ = Type.class_field c f in t))) with Not_found -> false)
		| _ ->
			true
	in
	let is_dynamic_return t =
		match follow t with
		| TFun (_,r) -> is_dynamic r
		| _ -> true
	in
	(*
		This is some quick analysis that matches the most common cases of dynamic-to-mono convertions
	*)
	let rec maybe_dynamic_mono e =
		match e.eexpr with
		| TLocal _ -> is_dynamic e.etype
		| TArray({ etype = t } as e,_) -> is_dynamic_array t || maybe_dynamic_rec e t
		| TField({ etype = t } as e,f) -> is_dynamic_field t (field_name f) || maybe_dynamic_rec e t
		| TCall({ etype = t } as e,_) -> is_dynamic_return t || maybe_dynamic_rec e t
		| TParenthesis e -> maybe_dynamic_mono e
		| TIf (_,a,Some b) -> maybe_dynamic_mono a || maybe_dynamic_mono b
		| _ -> false
	and maybe_dynamic_rec e t =
		match follow t with
		| TMono _ | TDynamic _ -> maybe_dynamic_mono e
		(* we might have inferenced a tmono into a single field *)
		| TAnon a when !(a.a_status) = Opened -> maybe_dynamic_mono e
		| _ -> false
	in
	match k with
	| KUnk | KDyn when maybe_dynamic_mono e ->
		unify ctx e.etype ctx.t.tfloat e.epos;
		false
	| _ ->
		unify ctx e.etype ctx.t.tint e.epos;
		true

let type_generic_function ctx (e,cf) el p =
	if cf.cf_params = [] then error "Function has no type parameters and cannot be generic" p;
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	let c,stat = match follow e.etype with
		| (TInst (c,_)) -> c,false
		| (TAnon a) -> (match !(a.a_status) with Statics c -> c,true | _ -> assert false)
		| _ -> assert false
	in
	let t = apply_params cf.cf_params monos cf.cf_type in
	add_constraint_checks ctx c.cl_types [] cf monos p;
	let args,ret = match t with
		| TFun(args,ret) -> args,ret
		| _ ->  error "Invalid field type for generic call" p
	in
	let el,tfunc = unify_call_params ctx None el args ret p false in
	(try
		let gctx = Codegen.make_generic ctx cf.cf_params monos p in
		let name = cf.cf_name ^ "_" ^ gctx.Codegen.name in
		let cf2 = try
			let cf2 = PMap.find name (if stat then c.cl_statics else c.cl_fields) in
			unify ctx cf2.cf_type t cf2.cf_pos;
			cf2
		with Not_found ->
			let cf2 = mk_field name t cf.cf_pos in
			if stat then begin
				c.cl_statics <- PMap.add name cf2 c.cl_statics;
				c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics
			end else begin
				c.cl_fields <- PMap.add name cf2 c.cl_fields;
				c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields
			end;
			ignore(follow cf.cf_type);
			cf2.cf_expr <- (match cf.cf_expr with
				| None -> None
				| Some e -> Some (Codegen.generic_substitute_expr gctx e));
			cf2.cf_kind <- cf.cf_kind;
			cf2.cf_public <- cf.cf_public;
			let metadata = List.filter (fun (m,_,_) -> match m with
				| Meta.Generic -> false
				| _ -> true
			) cf.cf_meta in
			cf2.cf_meta <- (Meta.NoCompletion,[],p) :: (Meta.NoUsing,[],p) :: metadata;
			cf2
		in
		let e = if stat then type_type ctx c.cl_path p else e in
		let e = acc_get ctx (field_access ctx MCall cf2 (if stat then FStatic (c,cf2) else FInstance (c,cf2)) cf2.cf_type e p) p in
		(el,ret,e)
	with Codegen.Generic_Exception (msg,p) ->
		error msg p)

let rec type_binop ctx op e1 e2 is_assign_op p =
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) MSet in
		let tt = (match e1 with AKNo _ | AKInline _ | AKUsing _ | AKMacro _ | AKAccess _ -> Value | AKSet(_,_,t,_) -> WithType t | AKExpr e | AKField (e,_,_) -> WithType e.etype) in
		let e2 = type_expr ctx e2 tt in
		(match e1 with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e1 | AKField (e1,_,_) ->
			unify ctx e2.etype e1.etype p;
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2 when i1 == i2 -> error "Assigning a value to itself" p
			| TField ({ eexpr = TConst TThis },FInstance (_,f1)) , TField ({ eexpr = TConst TThis },FInstance (_,f2)) when f1 == f2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AKSet (e,m,t,_) ->
			unify ctx e2.etype t p;
			make_call ctx (mk (TField (e,FDynamic m)) (tfun [t] t) p) [e2] t p
		| AKAccess(ebase,ekey) ->
			let a,pl,c = match follow ebase.etype with TAbstract({a_impl = Some c} as a,pl) -> a,pl,c | _ -> error "Invalid operation" p in
			let cf,tf,r =
				try find_array_access a pl c ekey.etype e2.etype true
				with Not_found -> error ("No @:arrayAccess function accepts arguments of " ^ (s_type (print_context()) ekey.etype) ^ " and " ^ (s_type (print_context()) e2.etype)) p
			in
			let et = type_module_type ctx (TClassDecl c) None p in
			let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
			make_call ctx ef [ebase;ekey;e2] r p
		| AKInline _ | AKUsing _ | AKMacro _ ->
			assert false)
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) MSet with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e | AKField (e,_,_) ->
			let eop = type_binop ctx op e1 e2 true p in
			(match eop.eexpr with
			| TBinop (_,_,e2) ->
				unify ctx eop.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssignOp op,e,e2)) e.etype p;
			| TField(e2,FDynamic ":needsAssign") ->
				unify ctx e2.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssign,e,e2)) e.etype p;
			| _ ->
				(* this must be an abstract cast *)
				check_assign ctx e;
				eop)
		| AKSet (e,m,t,f) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype in
			let ev = mk (TLocal v) e.etype p in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),f),p) e2 true p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,Some e]) ctx.t.tvoid p;
				make_call ctx (mk (TField (ev,FDynamic m)) (tfun [t] t) p) [get] t p
			]) t p
		| AKAccess(ebase,ekey) ->
			let a,pl,c = match follow ebase.etype with TAbstract({a_impl = Some c} as a,pl) -> a,pl,c | _ -> error "Invalid operation" p in
			let et = type_module_type ctx (TClassDecl c) None p in
			let cf_get,tf_get,r_get =
				try find_array_access a pl c ekey.etype t_dynamic false
				with Not_found -> error ("No @:arrayAccess function accepts an argument of " ^ (s_type (print_context()) ekey.etype)) p
			in
			(* bind complex keys to a variable so they do not make it into the output twice *)
			let ekey,l = match Optimizer.make_constant_expression ctx ekey with
				| Some e -> e, fun () -> None
				| None ->
					let save = save_locals ctx in
					let v = gen_local ctx ekey.etype in
					let e = mk (TLocal v) ekey.etype p in
					e, fun () -> (save(); Some (mk (TVars [v,Some ekey]) ctx.t.tvoid p))
			in
			let ast_call = ECall((EField(Interp.make_ast ebase,cf_get.cf_name),p),[Interp.make_ast ekey]),p in
			let eget = type_binop ctx op ast_call e2 true p in
			unify ctx eget.etype r_get p;
			let cf_set,tf_set,r_set =
				try find_array_access a pl c ekey.etype eget.etype true
				with Not_found -> error ("No @:arrayAccess function accepts arguments of " ^ (s_type (print_context()) ekey.etype) ^ " and " ^ (s_type (print_context()) eget.etype)) p
			in
			let ef_set = mk (TField(et,(FStatic(c,cf_set)))) tf_set p in
			(match l() with
			| None -> make_call ctx ef_set [ebase;ekey;eget] r_set p
			| Some e ->
				mk (TBlock [
					e;
					make_call ctx ef_set [ebase;ekey;eget] r_set p
				]) r_set p)
		| AKInline _ | AKUsing _ | AKMacro _ ->
			assert false)
	| _ ->
	let e1 = type_expr ctx e1 Value in
	let e2 = type_expr ctx e2 (if op == OpEq || op == OpNotEq then WithType e1.etype else Value) in
	let tint = ctx.t.tint in
	let tfloat = ctx.t.tfloat in
	let tstring = ctx.t.tstring in
	let to_string e =
		match classify e.etype with
		| KAbstract {a_impl = Some c} when PMap.mem "toString" c.cl_statics ->
			let et = type_module_type ctx (TClassDecl c) None e.epos in
			let cf = PMap.find "toString" c.cl_statics in
			make_call ctx (mk (TField(et,FStatic(c,cf))) cf.cf_type e.epos) [e] ctx.t.tstring e.epos
		| KUnk | KDyn | KParam _ | KOther | KAbstract _ ->
			let std = type_type ctx ([],"Std") e.epos in
			let acc = acc_get ctx (type_field ctx std "string" e.epos MCall) e.epos in
			ignore(follow acc.etype);
			let acc = (match acc.eexpr with TField (e,FClosure (Some c,f)) -> { acc with eexpr = TField (e,FInstance (c,f)) } | _ -> acc) in
			make_call ctx acc [e] ctx.t.tstring e.epos
		| KInt | KFloat | KString -> e
	in
	let mk_op t =
		if op = OpAdd && (classify t) = KString then
			let e1 = to_string e1 in
			let e2 = to_string e2 in
			mk (TBinop (op,e1,e2)) t p
		else
			mk (TBinop (op,e1,e2)) t p
	in
	let make e1 e2 = match op with
	| OpAdd ->
		mk_op (match classify e1.etype, classify e2.etype with
		| KInt , KInt ->
			tint
		| KFloat , KInt
		| KInt, KFloat
		| KFloat, KFloat ->
			tfloat
		| KUnk , KInt ->
			if unify_int ctx e1 KUnk then tint else tfloat
		| KUnk , KFloat
		| KUnk , KString  ->
			unify ctx e1.etype e2.etype e1.epos;
			e1.etype
		| KInt , KUnk ->
			if unify_int ctx e2 KUnk then tint else tfloat
		| KFloat , KUnk
		| KString , KUnk ->
			unify ctx e2.etype e1.etype e2.epos;
			e2.etype
		| _ , KString
		| KString , _ ->
			tstring
		| _ , KDyn ->
			e2.etype
		| KDyn , _ ->
			e1.etype
		| KUnk , KUnk ->
			let ok1 = unify_int ctx e1 KUnk in
			let ok2 = unify_int ctx e2 KUnk in
			if ok1 && ok2 then tint else tfloat
		| KParam t1, KParam t2 when Type.type_iseq t1 t2 ->
			t1
		| KParam t, KInt | KInt, KParam t ->
			t
		| KParam _, KFloat | KFloat, KParam _ | KParam _, KParam _ ->
			tfloat
		| KParam t, KUnk ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KUnk, KParam t ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
		| KAbstract _,_
		| _,KAbstract _
		| KParam _, _
		| _, KParam _
		| KOther, _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot add " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		)
	| OpAnd
	| OpOr
	| OpXor
	| OpShl
	| OpShr
	| OpUShr ->
		let i = tint in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk_op i
	| OpMod
	| OpMult
	| OpDiv
	| OpSub ->
		let result = ref (if op = OpDiv then tfloat else tint) in
		(match classify e1.etype, classify e2.etype with
		| KFloat, KFloat ->
			result := tfloat
		| KParam t1, KParam t2 when Type.type_iseq t1 t2 ->
			if op <> OpDiv then result := t1
		| KParam _, KParam _ ->
			result := tfloat
		| KParam t, KInt | KInt, KParam t ->
			if op <> OpDiv then result := t
		| KParam _, KFloat | KFloat, KParam _ ->
			result := tfloat
		| KFloat, k ->
			ignore(unify_int ctx e2 k);
			result := tfloat
		| k, KFloat ->
			ignore(unify_int ctx e1 k);
			result := tfloat
		| k1 , k2 ->
			let ok1 = unify_int ctx e1 k1 in
			let ok2 = unify_int ctx e2 k2 in
			if not ok1 || not ok2  then result := tfloat;
		);
		mk_op !result
	| OpEq
	| OpNotEq ->
		(try
			unify_raise ctx e1.etype e2.etype p;
			(* we only have to check one type here, because unification fails if one is Void and the other is not *)
			(match follow e2.etype with TAbstract({a_path=[],"Void"},_) -> error "Cannot compare Void" p | _ -> ())
		with
			Error (Unify _,_) -> unify ctx e2.etype e1.etype p);
		mk_op ctx.t.tbool
	| OpGt
	| OpGte
	| OpLt
	| OpLte ->
		(match classify e1.etype, classify e2.etype with
		| KInt , KInt | KInt , KFloat | KFloat , KInt | KFloat , KFloat | KString , KString -> ()
		| KInt , KUnk -> ignore(unify_int ctx e2 KUnk)
		| KFloat , KUnk | KString , KUnk -> unify ctx e2.etype e1.etype e2.epos
		| KUnk , KInt -> ignore(unify_int ctx e1 KUnk)
		| KUnk , KFloat | KUnk , KString -> unify ctx e1.etype e2.etype e1.epos
		| KUnk , KUnk ->
			ignore(unify_int ctx e1 KUnk);
			ignore(unify_int ctx e2 KUnk);
		| KDyn , KInt | KDyn , KFloat | KDyn , KString -> ()
		| KInt , KDyn | KFloat , KDyn | KString , KDyn -> ()
		| KDyn , KDyn -> ()
		| KParam _ , x | x , KParam _ when x <> KString && x <> KOther -> ()
		| KAbstract _,_
		| _,KAbstract _
		| KDyn , KUnk
		| KUnk , KDyn
		| KString , KInt
		| KString , KFloat
		| KInt , KString
		| KFloat , KString
		| KParam _ , _
		| _ , KParam _
		| KOther , _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot compare " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		);
		mk_op ctx.t.tbool
	| OpBoolAnd
	| OpBoolOr ->
		let b = ctx.t.tbool in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op b
	| OpInterval ->
		let t = Typeload.load_core_type ctx "IntIterator" in
		unify ctx e1.etype tint e1.epos;
		unify ctx e2.etype tint e2.epos;
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[e1;e2])) t p
	| OpArrow ->
		error "Unexpected =>" p
	| OpAssign
	| OpAssignOp _ ->
		assert false
	in
	let find_overload a c t left =
		let rec loop ops = match ops with
			| [] -> raise Not_found
			| (o,cf) :: ops when is_assign_op && o = OpAssignOp(op) || o == op ->
				(match follow (monomorphs cf.cf_params cf.cf_type) with
				| TFun([(_,_,t1);(_,_,t2)],r) when (left || Meta.has Meta.Commutative cf.cf_meta) && type_iseq t t2 && can_access ctx c cf true ->
					cf,r,o = OpAssignOp(op)
				| _ -> loop ops)
			| _ :: ops ->
				loop ops
		in
		loop a.a_ops
	in
	let mk_cast_op c f a pl e1 e2 r assign =
		let t = field_type ctx c [] f p in
		let t = apply_params a.a_types pl t in
		let et = type_module_type ctx (TClassDecl c) None p in
		let ef = mk (TField (et,FStatic (c,f))) t p in
		let ec = make_call ctx ef [e1;e2] r p in
		(* obviously a hack to report back that we need an assignment *)
		if is_assign_op && not assign then mk (TField(ec,FDynamic ":needsAssign")) t_dynamic p else ec
	in
	let cast_rec e1t e2t r =
		let e = make e1t e2t in
		begin try
			unify_raise ctx e.etype r p
		with Error (Unify _,_) ->
			error ("The result of this operation (" ^ (s_type (print_context()) e.etype) ^ ") is not compatible with declared return type " ^ (s_type (print_context()) r)) p;
		end;
		{e with etype = r}
	in
	try (match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) ->
			let f,r,assign = find_overload a c e2.etype true in
			begin match f.cf_expr with
				| None ->
					let e2 = match e2.etype with TAbstract(a,pl) -> {e2 with etype = apply_params a.a_types pl a.a_this} | _ -> e2 in
					cast_rec {e1 with etype = apply_params a.a_types pl a.a_this} e2 r
				| Some _ ->
					mk_cast_op c f a pl e1 e2 r assign
			end
		| _ ->
			raise Not_found)
	with Not_found -> try (match follow e2.etype with
		| TAbstract ({a_impl = Some c} as a,pl) ->
			let f,r,assign = find_overload a c e1.etype false in
			begin match f.cf_expr with
				| None ->
					let e1 = match e1.etype with TAbstract(a,pl) -> {e1 with etype = apply_params a.a_types pl a.a_this} | _ -> e1 in
					cast_rec e1 {e2 with etype = apply_params a.a_types pl a.a_this} r
				| Some _ ->
					mk_cast_op c f a pl e2 e1 r assign
			end
		| _ ->
			raise Not_found)
	with Not_found ->
		make e1 e2


and type_unop ctx op flag e p =
	let set = (op = Increment || op = Decrement) in
	let acc = type_access ctx (fst e) (snd e) (if set then MSet else MGet) in
	let access e =
		let make e =
			let t = (match op with
			| Not ->
				unify ctx e.etype ctx.t.tbool e.epos;
				ctx.t.tbool
			| Increment
			| Decrement
			| Neg
			| NegBits ->
				if set then check_assign ctx e;
				(match classify e.etype with
				| KFloat -> ctx.t.tfloat
				| KParam t ->
					unify ctx e.etype ctx.t.tfloat e.epos;
					t
				| k ->
					if unify_int ctx e k then ctx.t.tint else ctx.t.tfloat)
			) in
			mk (TUnop (op,flag,e)) t p
		in
		try (match follow e.etype with
			| TAbstract ({a_impl = Some c} as a,pl) ->
				let _,_,cf = List.find (fun (op2,flag2,cf) -> op2 == op && flag2 == flag) a.a_unops in
				if not (can_access ctx c cf true) then error ("Cannot access " ^ cf.cf_name) p;
				let t = field_type ctx c [] cf p in
				let t = apply_params a.a_types pl t in
				let r = match t with TFun (_,r) -> r | _ -> error "Invalid operation" p in
				(match cf.cf_expr with
				| None ->
					let e = make {e with etype = apply_params a.a_types pl a.a_this} in
					unify ctx r e.etype p;
					{e with etype = r}
				| Some _ ->
					let et = type_module_type ctx (TClassDecl c) None p in
					let ef = mk (TField (et,FStatic (c,cf))) t p in
					make_call ctx ef [e] r p)
			| _ -> raise Not_found
		) with Not_found ->
			make e
	in
	match acc with
	| AKExpr e | AKField (e,_,_) -> access e
	| AKInline _ | AKUsing _ when not set -> access (acc_get ctx acc p)
	| AKNo s ->
		error ("The field or identifier " ^ s ^ " is not accessible for " ^ (if set then "writing" else "reading")) p
	| AKInline _ | AKUsing _ | AKMacro _ | AKAccess _ ->
		error "This kind of operation is not supported" p
	| AKSet (e,m,t,f) ->
		let l = save_locals ctx in
		let v = gen_local ctx e.etype in
		let ev = mk (TLocal v) e.etype p in
		let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in
		let one = (EConst (Int "1"),p) in
		let eget = (EField ((EConst (Ident v.v_name),p),f),p) in
		match flag with
		| Prefix ->
			let get = type_binop ctx op eget one false p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,Some e]) ctx.t.tvoid p;
				make_call ctx (mk (TField (ev,FDynamic m)) (tfun [t] t) p) [get] t p
			]) t p
		| Postfix ->
			let v2 = gen_local ctx t in
			let ev2 = mk (TLocal v2) t p in
			let get = type_expr ctx eget Value in
			let plusone = type_binop ctx op (EConst (Ident v2.v_name),p) one false p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,Some e; v2,Some get]) ctx.t.tvoid p;
				make_call ctx (mk (TField (ev,FDynamic m)) (tfun [plusone.etype] t) p) [plusone] t p;
				ev2
			]) t p

and type_switch_old ctx e cases def with_type p =
	let eval = type_expr ctx e Value in
	let old_m = ctx.m in
	let enum = ref None in
	let used_cases = Hashtbl.create 0 in
	let is_fake_enum e =
		e.e_path = ([],"Bool") || Meta.has Meta.FakeEnum e.e_meta
	in
	(match follow eval.etype with
	| TEnum (e,_) when is_fake_enum e -> ()
	| TEnum (e,params) ->
		enum := Some (Some (e,params));
		(* hack to prioritize enum lookup *)
		ctx.m <- { ctx.m with module_types = TEnumDecl e :: ctx.m.module_types }
	| TMono _ ->
		enum := Some None;
	| t ->
		if t == t_dynamic then enum := Some None
	);
	let case_expr c =
		enum := None;
		(* this inversion is needed *)
		unify ctx eval.etype c.etype c.epos;
		CExpr c
	in
	let type_match e en s pl =
		let p = e.epos in
		let params = (match !enum with
			| None ->
				assert false
			| Some None when is_fake_enum en ->
				raise Exit
			| Some None ->
				let params = List.map (fun _ -> mk_mono()) en.e_types in
				enum := Some (Some (en,params));
				unify ctx eval.etype (TEnum (en,params)) p;
				params
			| Some (Some (en2,params)) ->
				if en != en2 then error ("This constructor is part of enum " ^ s_type_path en.e_path ^ " but is matched with enum " ^ s_type_path en2.e_path) p;
				params
		) in
		if Hashtbl.mem used_cases s then error "This constructor has already been used" p;
		Hashtbl.add used_cases s ();
		let cst = (try PMap.find s en.e_constrs with Not_found -> assert false) in
		let et = apply_params en.e_types params (monomorphs cst.ef_params cst.ef_type) in
		let pl, rt = (match et with
		| TFun (l,rt) ->
			let pl = (if List.length l = List.length pl then pl else
				match pl with
				| [None] -> List.map (fun _ -> None) l
				| _ -> error ("This constructor requires " ^ string_of_int (List.length l) ^ " arguments") p
			) in
			Some (List.map2 (fun p (_,_,t) -> match p with None -> None | Some p -> Some (p, t)) pl l), rt
		| TEnum _ ->
			if pl <> [] then error "This constructor does not require any argument" p;
			None, et
		| _ -> assert false
		) in
		unify ctx rt eval.etype p;
		CMatch (cst,pl,p)
	in
	let type_case efull e pl p =
		try
			let e = (match !enum, e with
			| None, _ -> raise Exit
			| Some (Some (en,params)), (EConst (Ident i),p) ->
				let ef = (try
					PMap.find i en.e_constrs
				with Not_found ->
					display_error ctx ("This constructor is not part of the enum " ^ s_type_path en.e_path) p;
					raise Exit
				) in
				mk (fast_enum_field en ef p) (apply_params en.e_types params ef.ef_type) (snd e)
			| _ ->
				type_expr ctx e Value
			) in
			let pl = List.map (fun e ->
				match fst e with
				| EConst (Ident "_") -> None
				| EConst (Ident i) -> Some i
				| _ -> raise Exit
			) pl in
			(match e.eexpr with
			| TField (_,FEnum (en,c)) -> type_match e en c.ef_name pl
			| _ -> if pl = [] then case_expr e else raise Exit)
		with Exit ->
			case_expr (type_expr ctx efull Value)
	in
	let cases = List.map (fun (el,eg,e2) ->
		if el = [] then error "Case must match at least one expression" (punion_el el);
		let el = List.map (fun e ->
			match e with
			| (ECall (c,pl),p) -> type_case e c pl p
			| e -> type_case e e [] (snd e)
		) el in
		el, e2
	) cases in
	ctx.m <- old_m;
	let el = ref [] in
	let type_case_code e =
		let e = (match e with
			| Some e -> type_expr ctx e with_type
			| None -> mk (TBlock []) ctx.com.basic.tvoid Ast.null_pos
		) in
		el := e :: !el;
		e
	in
	let def() = (match def with
		| None -> None
		| Some e ->
			let locals = save_locals ctx in
			let e = type_case_code e in
			locals();
			Some e
	) in
	match !enum with
	| Some (Some (enum,enparams)) ->
		let same_params p1 p2 =
			let l1 = (match p1 with None -> [] | Some l -> l) in
			let l2 = (match p2 with None -> [] | Some l -> l) in
			let rec loop = function
				| [] , [] -> true
				| None :: l , [] | [] , None :: l -> loop (l,[])
				| None :: l1, None :: l2 -> loop (l1,l2)
				| Some (n1,t1) :: l1, Some (n2,t2) :: l2 ->
					n1 = n2 && type_iseq t1 t2 && loop (l1,l2)
				| _ -> false
			in
			loop (l1,l2)
		in
		let matchs (el,e) =
			match el with
			| CMatch (c,params,p1) :: l ->
				let params = ref params in
				let cl = List.map (fun c ->
					match c with
					| CMatch (c,p,p2) ->
						if not (same_params p !params) then display_error ctx "Constructors parameters differs : should be same name, same type, and same position" p2;
						if p <> None then params := p;
						c
					| _ -> assert false
				) l in
				let locals = save_locals ctx in
				let params = (match !params with
					| None -> None
					| Some l ->
						let has = ref false in
						let l = List.map (fun v ->
							match v with
							| None -> None
							| Some (v,t) -> has := true; Some (add_local ctx v t)
						) l in
						if !has then Some l else None
				) in
				let e = type_case_code e in
				locals();
				(c :: cl) , params, e
			| _ ->
				assert false
		in
		let indexes (el,vars,e) =
			List.map (fun c -> c.ef_index) el, vars, e
		in
		let cases = List.map matchs cases in
		let def = def() in
		(match def with
		| Some _ -> ()
		| None ->
			let tenum = TEnum(enum,enparams) in
			let l = PMap.fold (fun c acc ->
				let t = monomorphs enum.e_types (monomorphs c.ef_params (match c.ef_type with TFun (_,t) -> t | t -> t)) in
				if Hashtbl.mem used_cases c.ef_name || not (try unify_raise ctx t tenum c.ef_pos; true with Error (Unify _,_) -> false) then acc else c.ef_name :: acc
			) enum.e_constrs [] in
			match l with
			| [] -> ()
			| _ -> display_error ctx ("Some constructors are not matched : " ^ String.concat "," l) p
		);
		let t = if with_type = NoValue then (mk_mono()) else unify_min ctx (List.rev !el) in
		mk (TMatch (eval,(enum,enparams),List.map indexes cases,def)) t p
	| _ ->
		let consts = Hashtbl.create 0 in
		let exprs (el,e) =
			let el = List.map (fun c ->
				match c with
				| CExpr (({ eexpr = TConst c }) as e) ->
					if Hashtbl.mem consts c then error "Duplicate constant in switch" e.epos;
					Hashtbl.add consts c true;
					e
				| CExpr c -> c
				| CMatch (_,_,p) -> error "You cannot use a normal switch on an enum constructor" p
			) el in
			let locals = save_locals ctx in
			let e = type_case_code e in
			locals();
			el, e
		in
		let cases = List.map exprs cases in
		let def = def() in
		let t = if with_type = NoValue then (mk_mono()) else unify_min ctx (List.rev !el) in
		mk (TSwitch (eval,cases,def)) t p

and type_switch ctx e cases def (with_type:with_type) p =
	try
		if (Common.defined ctx.com Common.Define.NoPatternMatching) then raise Exit;
		match_expr ctx e cases def with_type p
	with Exit ->
		type_switch_old ctx e cases def with_type p

and type_ident ctx i p mode =
	try
		type_ident_raise ctx i p mode
	with Not_found -> try
		(* lookup type *)
		if is_lower_ident i then raise Not_found;
		let e = (try type_type ctx ([],i) p with Error (Module_not_found ([],name),_) when name = i -> raise Not_found) in
		AKExpr e
	with Not_found ->
		if ctx.untyped then begin
			if i = "__this__" then
				AKExpr (mk (TConst TThis) ctx.tthis p)
			else
				let t = mk_mono() in
				AKExpr (mk (TLocal (alloc_var i t)) t p)
		end else begin
			if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			let err = Unknown_ident i in
			if ctx.in_display then raise (Error (err,p));
			if ctx.com.display then begin
				display_error ctx (error_msg err) p;
				let t = mk_mono() in
				AKExpr (mk (TLocal (add_local ctx i t)) t p)
			end else begin
				if List.exists (fun (i2,_) -> i2 = i) ctx.type_params then
					display_error ctx ("Type parameter " ^ i ^ " is only available at compilation and is not a runtime value") p
				else
					display_error ctx (error_msg err) p;
				AKExpr (mk (TConst TNull) t_dynamic p)
			end
		end

and type_access ctx e p mode =
	match e with
	| EConst (Ident s) ->
		type_ident ctx s p mode
	| EField _ ->
		let fields path e =
			List.fold_left (fun e (f,_,p) ->
				let e = acc_get ctx (e MGet) p in
				type_field ctx e f p
			) e path
		in
		let type_path path =
			let rec loop acc path =
				match path with
				| [] ->
					(match List.rev acc with
					| [] -> assert false
					| (name,flag,p) :: path ->
						try
							fields path (type_access ctx (EConst (Ident name)) p)
						with
							Error (Unknown_ident _,p2) as e when p = p2 ->
								try
									let path = ref [] in
									let name , _ , _ = List.find (fun (name,flag,p) ->
										if flag then
											true
										else begin
											path := name :: !path;
											false
										end
									) (List.rev acc) in
									raise (Error (Module_not_found (List.rev !path,name),p))
								with
									Not_found ->
										if ctx.in_display then raise (Parser.TypePath (List.map (fun (n,_,_) -> n) (List.rev acc),None));
										raise e)
				| (_,false,_) as x :: path ->
					loop (x :: acc) path
				| (name,true,p) as x :: path ->
					let pack = List.rev_map (fun (x,_,_) -> x) acc in
					let def() =
						try
							let e = type_type ctx (pack,name) p in
							fields path (fun _ -> AKExpr e)
						with
							Error (Module_not_found m,_) when m = (pack,name) ->
								loop ((List.rev path) @ x :: acc) []
					in
					match path with
					| (sname,true,p) :: path ->
						let get_static t =
							fields ((sname,true,p) :: path) (fun _ -> AKExpr (type_module_type ctx t None p))
						in
						let check_module m v =
							try
								let md = Typeload.load_module ctx m p in
								(* first look for existing subtype *)
								(try
									let t = List.find (fun t -> not (t_infos t).mt_private && t_path t = (fst m,sname)) md.m_types in
									Some (fields path (fun _ -> AKExpr (type_module_type ctx t None p)))
								with Not_found -> try
								(* then look for main type statics *)
									if fst m = [] then raise Not_found; (* ensure that we use def() to resolve local types first *)
									let t = List.find (fun t -> not (t_infos t).mt_private && t_path t = m) md.m_types in
									Some (get_static t)
								with Not_found ->
									None)
							with Error (Module_not_found m2,_) when m = m2 ->
								None
						in
						let rec loop pack =
							match check_module (pack,name) sname with
							| Some r -> r
							| None ->
								match List.rev pack with
								| [] -> def()
								| _ :: l -> loop (List.rev l)
						in
						(match pack with
						| [] ->
							(try
								let t = List.find (fun t -> snd (t_infos t).mt_path = name) (ctx.m.curmod.m_types @ ctx.m.module_types) in
								get_static t
							with Not_found ->
								loop (fst ctx.m.curmod.m_path))
						| _ ->
							match check_module (pack,name) sname with
							| Some r -> r
							| None -> def());
					| _ -> def()
			in
			match path with
			| [] -> assert false
			| (name,_,p) :: pnext ->
				try
					fields pnext (fun _ -> type_ident_raise ctx name p MGet)
				with
					Not_found -> loop [] path
		in
		let rec loop acc e =
			match fst e with
			| EField (e,s) ->
				loop ((s,not (is_lower_ident s),p) :: acc) e
			| EConst (Ident i) ->
				type_path ((i,not (is_lower_ident i),p) :: acc)
			| _ ->
				fields acc (type_access ctx (fst e) (snd e))
		in
		loop [] (e,p) mode
	| EArray (e1,e2) ->
		let e1 = type_expr ctx e1 Value in
		let e2 = type_expr ctx e2 Value in
		(try (match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) when a.a_array <> [] ->
			(match mode with
			| MSet ->
				(* resolve later *)
				AKAccess (e1, e2)
			| _ ->
				let cf,tf,r = find_array_access a pl c e2.etype t_dynamic false in
				let et = type_module_type ctx (TClassDecl c) None p in
				let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
				AKExpr (make_call ctx ef [e1;e2] r p))
		| _ -> raise Not_found)
		with Not_found ->
		unify ctx e2.etype ctx.t.tint e2.epos;
		let rec loop et =
			match follow et with
			| TInst ({ cl_array_access = Some t; cl_types = pl },tl) ->
				apply_params pl tl t
			| TInst ({ cl_super = Some (c,stl); cl_types = pl },tl) ->
				apply_params pl tl (loop (TInst (c,stl)))
			| TInst ({ cl_path = [],"ArrayAccess" },[t]) ->
				t
			| TAbstract(a,tl) when Meta.has Meta.ArrayAccess a.a_meta ->
				loop (apply_params a.a_types tl a.a_this)
			| _ ->
				let pt = mk_mono() in
				let t = ctx.t.tarray pt in
				(try unify_raise ctx et t p
				with Error(Unify _,_) -> if not ctx.untyped then error ("Array access is not allowed on " ^ (s_type (print_context()) e1.etype)) e1.epos);
				pt
		in
		let pt = loop e1.etype in
		AKExpr (mk (TArray (e1,e2)) pt p))
	| _ ->
		AKExpr (type_expr ctx (e,p) Value)

and type_vars ctx vl p in_block =
	let save = if in_block then (fun() -> ()) else save_locals ctx in
	let vl = List.map (fun (v,t,e) ->
		try
			let t = Typeload.load_type_opt ctx p t in
			let e = (match e with
				| None -> None
				| Some e ->
					let e = type_expr ctx e (WithType t) in
					unify ctx e.etype t p;
					Some e
			) in
			if v.[0] = '$' && not ctx.com.display then error "Variables names starting with a dollar are not allowed" p;
			add_local ctx v t, e
		with
			Error (e,p) ->
				display_error ctx (error_msg e) p;
				add_local ctx v t_dynamic, None
	) vl in
	save();
	mk (TVars vl) ctx.t.tvoid p

and with_type_error ctx with_type msg p =
	match with_type with
	| WithTypeResume _ -> raise (WithTypeError ([Unify_custom msg],p))
	| _ -> display_error ctx msg p

and type_expr ctx (e,p) (with_type:with_type) =
	match e with
	| EField ((EConst (String s),p),"code") ->
		if UTF8.length s <> 1 then error "String must be a single UTF8 char" p;
		mk (TConst (TInt (Int32.of_int (UChar.code (UTF8.get s 0))))) ctx.t.tint p
	| EField(_,n) when n.[0] = '$' ->
		error "Field names starting with $ are not allowed" p
	| EConst (Ident s) ->
		(try
			acc_get ctx (type_ident_raise ~imported_enums:false ctx s p MGet) p
		with Not_found -> try
			(match with_type with
			| WithType t | WithTypeResume t ->
				(match follow t with
				| TEnum (e,pl) ->
					(try
						let ef = PMap.find s e.e_constrs in
						mk (fast_enum_field e ef p) (apply_params e.e_types pl ef.ef_type) p
					with Not_found ->
						if ctx.untyped then raise Not_found;
						with_type_error ctx with_type (string_error s e.e_names ("Identifier '" ^ s ^ "' is not part of enum " ^ s_type_path e.e_path)) p;
						mk (TConst TNull) t p)
				| _ -> raise Not_found)
			| _ ->
				raise Not_found)
		with Not_found ->
			acc_get ctx (type_access ctx e p MGet) p)
	| EField _
	| EArray _ ->
		acc_get ctx (type_access ctx e p MGet) p
	| EConst (Regexp (r,opt)) ->
		let str = mk (TConst (TString r)) ctx.t.tstring p in
		let opt = mk (TConst (TString opt)) ctx.t.tstring p in
		let t = Typeload.load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[str;opt])) t p
	| EConst (String s) when Lexer.is_fmt_string p ->
		let e = ref None in
		let pmin = ref p.pmin in
		let min = ref (p.pmin + 1) in
		let add enext len =
			let p = { p with pmin = !min; pmax = !min + len } in
			min := !min + len;
			match !e with
			| None -> e := Some (enext,p)
			| Some prev ->
				e := Some (EBinop (OpAdd,prev,(enext,p)),punion (pos prev) p)
		in
		let add_sub start pos =
			let len = pos - start in
			if len > 0 || !e = None then add (EConst (String (String.sub s start len))) len
		in
		let warn_escape = Common.defined ctx.com Define.FormatWarning in
		let warn pos len =
			ctx.com.warning "This string is formated" { p with pmin = !pmin + 1 + pos; pmax = !pmin + 1 + pos + len }
		in
		let len = String.length s in
		let rec parse start pos =
			if pos = len then add_sub start pos else
			let c = String.unsafe_get s pos in
			let pos = pos + 1 in
			if c = '\'' then begin
				incr pmin;
				incr min;
			end;
			if c <> '$' || pos = len then parse start pos else
			match String.unsafe_get s pos with
			| '$' ->
				if warn_escape then warn pos 1;
				(* double $ *)
				add_sub start pos;
				parse (pos + 1) (pos + 1)
			| '{' ->
				parse_group start pos '{' '}' "brace"
			| 'a'..'z' | 'A'..'Z' | '_' ->
				add_sub start (pos - 1);
				incr min;
				let rec loop i =
					if i = len then i else
					let c = String.unsafe_get s i in
					match c with
					| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> loop (i+1)
					| _ -> i
				in
				let iend = loop (pos + 1) in
				let len = iend - pos in
				if warn_escape then warn pos len;
				add (EConst (Ident (String.sub s pos len))) len;
				parse (pos + len) (pos + len)
			| _ ->
				(* keep as-it *)
				parse start pos
		and parse_group start pos gopen gclose gname =
			add_sub start (pos - 1);
			let rec loop groups i =
				if i = len then
					match groups with
					| [] -> assert false
					| g :: _ -> error ("Unclosed " ^ gname) { p with pmin = !pmin + g + 1; pmax = !pmin + g + 2 }
				else
					let c = String.unsafe_get s i in
					if c = gopen then
						loop (i :: groups) (i + 1)
					else if c = gclose then begin
						let groups = List.tl groups in
						if groups = [] then i else loop groups (i + 1)
					end else
						loop groups (i + 1)
			in
			let send = loop [pos] (pos + 1) in
			let slen = send - pos - 1 in
			let scode = String.sub s (pos + 1) slen in
			if warn_escape then warn (pos + 1) slen;
			min := !min + 2;
			add (fst (parse_expr_string ctx scode { p with pmin = !pmin + pos + 2; pmax = !pmin + send + 1 } true)) slen;
			min := !min + 1;
			parse (send + 1) (send + 1)
		in
		parse 0 0;
		(match !e with
		| None -> assert false
		| Some e -> type_expr ctx e with_type);
	| EConst c ->
		Codegen.type_constant ctx.com c p
    | EBinop (op,e1,e2) ->
		type_binop ctx op e1 e2 false p
	| EBlock [] when with_type <> NoValue ->
		type_expr ctx (EObjectDecl [],p) with_type
	| EBlock l ->
		let locals = save_locals ctx in
		let rec loop = function
			| [] -> []
			| (EVars vl,p) :: l ->
				let e = type_vars ctx vl p true in
				e :: loop l
			| [e] ->
				(try
					[type_expr ctx e with_type]
				with
					Error (e,p) -> display_error ctx (error_msg e) p; [])
			| e :: l ->
				try
					let e = type_expr ctx e NoValue in
					e :: loop l
				with
					Error (e,p) -> display_error ctx (error_msg e) p; loop l
		in
		let l = loop l in
		locals();
		let rec loop = function
			| [] -> ctx.t.tvoid
			| [e] -> e.etype
			| _ :: l -> loop l
		in
		mk (TBlock l) (loop l) p
	| EParenthesis e ->
		let e = type_expr ctx e with_type in
		mk (TParenthesis e) e.etype p
	| EObjectDecl fl ->
		let a = (match with_type with
		| WithType t | WithTypeResume t ->
			(match follow t with
			| TAnon a when not (PMap.is_empty a.a_fields) -> Some a
			| _ -> None)
		| _ -> None
		) in
		(match a with
		| None ->
			let rec loop (l,acc) (f,e) =
				let f,add = object_field f in
				if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
				let e = type_expr ctx e Value in
				(match follow e.etype with TAbstract({a_path=[],"Void"},_) -> error "Fields of type Void are not allowed in structures" e.epos | _ -> ());
				let cf = mk_field f e.etype e.epos in
				((f,e) :: l, if add then PMap.add f cf acc else acc)
			in
			let fields , types = List.fold_left loop ([],PMap.empty) fl in
			let x = ref Const in
			ctx.opened <- x :: ctx.opened;
			mk (TObjectDecl (List.rev fields)) (TAnon { a_fields = types; a_status = x }) p
		| Some a ->
			let fields = ref PMap.empty in
			let extra_fields = ref [] in
			let fl = List.map (fun (n, e) ->
				let n,add = object_field n in
				if PMap.mem n !fields then error ("Duplicate field in object declaration : " ^ n) p;
				let e = try
					let t = (PMap.find n a.a_fields).cf_type in
					let e = type_expr ctx e (match with_type with WithTypeResume _ -> WithTypeResume t | _ -> WithType t) in
					unify ctx e.etype t e.epos;
					(try type_eq EqStrict e.etype t; e with Unify_error _ -> mk (TCast (e,None)) t e.epos)
				with Not_found ->
					extra_fields := n :: !extra_fields;
					type_expr ctx e Value
				in
				if add then begin
					let cf = mk_field n e.etype e.epos in
					fields := PMap.add n cf !fields;
				end;
				(n,e)
			) fl in
			let t = (TAnon { a_fields = !fields; a_status = ref Const }) in
			if not ctx.untyped then begin
				let unify_error l p =
					match with_type with
					| WithTypeResume _ -> raise (WithTypeError (l,p))
					| _ -> raise (Error (Unify l,p))
				in
				PMap.iter (fun n cf ->
					if not (Meta.has Meta.Optional cf.cf_meta) && not (PMap.mem n !fields) then unify_error [has_no_field t n] p;
				) a.a_fields;
				(match !extra_fields with
				| [] -> ()
				| _ -> unify_error (List.map (fun n -> has_extra_field t n) !extra_fields) p);
			end;
			a.a_status := Closed;
			mk (TObjectDecl fl) t p)
	| EArrayDecl [(EFor _,_) | (EWhile _,_) as e] ->
		let v = gen_local ctx (mk_mono()) in
		let et = ref (EConst(Ident "null"),p) in
		let rec map_compr (e,p) =
			match e with
			| EFor(it,e2) -> (EFor (it, map_compr e2),p)
			| EWhile(cond,e2,flag) -> (EWhile (cond,map_compr e2,flag),p)
			| EIf (cond,e2,None) -> (EIf (cond,map_compr e2,None),p)
			| EBlock [e] -> (EBlock [map_compr e],p)
			| EParenthesis e2 -> (EParenthesis (map_compr e2),p)
			| EBinop(OpArrow,a,b) ->
				et := (ENew({tpackage=[];tname="Map";tparams=[];tsub=None},[]),p);
				(ECall ((EField ((EConst (Ident v.v_name),p),"set"),p),[a;b]),p)
			| _ ->
				et := (EArrayDecl [],p);
				(ECall ((EField ((EConst (Ident v.v_name),p),"push"),p),[(e,p)]),p)
		in
		let e = map_compr e in
		let ea = type_expr ctx !et with_type in
		unify ctx v.v_type ea.etype p;
		let efor = type_expr ctx e NoValue in
		mk (TBlock [
			mk (TVars [v,Some ea]) ctx.t.tvoid p;
			efor;
			mk (TLocal v) v.v_type p;
		]) v.v_type p
	| EArrayDecl ((EBinop(OpArrow,_,_),_) as e1 :: el) ->
		let keys = Hashtbl.create 0 in
		let tkey,tval = mk_mono(),mk_mono() in
		let type_arrow e1 e2 =
			let e1 = type_expr ctx e1 (WithType tkey) in
			try
				let p = Hashtbl.find keys e1.eexpr in
				display_error ctx "Duplicate key" e1.epos;
				error "Previously defined here" p
			with Not_found ->
				Hashtbl.add keys e1.eexpr e1.epos;
				unify ctx e1.etype tkey e1.epos;
				let e2 = type_expr ctx e2 (WithType tval) in
				unify ctx e2.etype tval e2.epos;
				e1,e2
		in
		let m = Typeload.load_module ctx ([],"Map") null_pos in
		let a,c = match m.m_types with
			| (TAbstractDecl ({a_impl = Some c} as a)) :: _ -> a,c
			| _ -> assert false
		in
		let tmap = TAbstract(a,[tkey;tval]) in
		let cf = PMap.find "set" c.cl_statics in
		let el = e1 :: el in
		let v = gen_local ctx tmap in
		let ev = mk (TLocal v) tmap p in
		let ef = mk (TField(ev,FInstance(c,cf))) (tfun [tkey;tval] ctx.t.tvoid) p in
		let el = ev :: List.fold_left (fun acc e -> match fst e with
			| EBinop(OpArrow,e1,e2) ->
				let e1,e2 = type_arrow e1 e2 in
				(make_call ctx ef [e1;e2] ctx.com.basic.tvoid p) :: acc
			| _ ->
				error "Expected a => b" (snd e)
		) [] el in
		let enew = mk (TNew(c,[tkey;tval],[])) tmap p in
		let el = (mk (TVars [v,Some enew]) t_dynamic p) :: (List.rev el) in
		mk (TBlock el) tmap p
	| EArrayDecl el ->
		let tp = (match with_type with
		| WithType t | WithTypeResume t ->
			(match follow t with
			| TInst ({ cl_path = [],"Array" },[tp]) ->
				(match follow tp with
				| TMono _ -> None
				| _ -> Some tp)
			| _ ->
				if t == t_dynamic then Some t else None)
		| _ ->
			None
		) in
		(match tp with
		| None ->
			let el = List.map (fun e -> type_expr ctx e Value) el in
			let t = try
				unify_min_raise ctx el
			with Error (Unify l,p) ->
				display_error ctx "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>" p;
				raise (Error (Unify l, p))
			in
			mk (TArrayDecl el) (ctx.t.tarray t) p
		| Some t ->
			let el = List.map (fun e ->
				let e = type_expr ctx e (match with_type with WithTypeResume _ -> WithTypeResume t | _ -> WithType t) in
				(match with_type with
				| WithTypeResume _ -> (try unify_raise ctx e.etype t e.epos with Error (Unify l,p) -> raise (WithTypeError (l,p)))
				| _ -> unify ctx e.etype t e.epos);
				e
			) el in
			mk (TArrayDecl el) (ctx.t.tarray t) p)
	| EVars vl ->
		type_vars ctx vl p false
	| EFor (it,e2) ->
		let i, e1 = (match it with
			| (EIn ((EConst (Ident i),_),e),_) -> i, e
			| _ -> error "For expression should be 'v in expr'" (snd it)
		) in
		let e1 = type_expr ctx e1 Value in
		let old_loop = ctx.in_loop in
		let old_locals = save_locals ctx in
		ctx.in_loop <- true;
		let e = (match Optimizer.optimize_for_loop ctx i e1 e2 p with
			| Some e -> e
			| None ->
				let t, pt = Typeload.t_iterator ctx in
				let i = add_local ctx i pt in
				let e1 = (match follow e1.etype with
				| TMono _
				| TDynamic _ ->
					display_error ctx "You can't iterate on a Dynamic value, please specify Iterator or Iterable" e1.epos;
					e1
				| TLazy _ ->
					assert false
				| _ ->
					(try
						unify_raise ctx e1.etype t e1.epos;
						e1
					with Error (Unify _,_) ->
						let acc = acc_get ctx (type_field ctx e1 "iterator" e1.epos MCall) e1.epos in
						let acc = (match acc.eexpr with TField (e,FClosure (c,f)) -> { acc with eexpr = TField (e,match c with None -> FAnon f | Some c -> FInstance (c,f)) } | _ -> acc) in
						match follow acc.etype with
						| TFun ([],it) ->
							unify ctx it t e1.epos;
							make_call ctx acc [] it e1.epos
						| _ ->
							display_error ctx "The field iterator is not a method" e1.epos;
							mk (TConst TNull) t_dynamic p
					)
				) in
				let e2 = type_expr ctx e2 NoValue in
				(* can we inline hasNext() ? *)
				(try
					let c,pl = (match follow e1.etype with TInst (c,pl) -> c,pl | _ -> raise Exit) in
					let _, ft, fhasnext = (try class_field ctx c pl "hasNext" p with Not_found -> raise Exit) in
					if fhasnext.cf_kind <> Method MethInline then raise Exit;
					let tmp = gen_local ctx e1.etype in
					let eit = mk (TLocal tmp) e1.etype p in
					let ehasnext = make_call ctx (mk (TField (eit,FInstance (c, fhasnext))) (TFun([],ctx.t.tbool)) p) [] ctx.t.tbool p in
					let enext = mk (TVars [i,Some (make_call ctx (mk (TField (eit,FDynamic "next")) (TFun ([],pt)) p) [] pt p)]) ctx.t.tvoid p in
					let eblock = (match e2.eexpr with
						| TBlock el -> { e2 with eexpr = TBlock (enext :: el) }
						| _ -> mk (TBlock [enext;e2]) ctx.t.tvoid p
					) in
					mk (TBlock [
						mk (TVars [tmp,Some e1]) ctx.t.tvoid p;
						mk (TWhile (ehasnext,eblock,NormalWhile)) ctx.t.tvoid p
					]) ctx.t.tvoid p
				with Exit ->
					mk (TFor (i,e1,e2)) ctx.t.tvoid p)
		) in
		ctx.in_loop <- old_loop;
		old_locals();
		e
	| EIn _ ->
		error "This expression is not allowed outside a for loop" p
	| ETernary (e1,e2,e3) ->
		type_expr ctx (EIf (e1,e2,Some e3),p) with_type
	| EIf (e,e1,e2) ->
		let e = type_expr ctx e Value in
		unify ctx e.etype ctx.t.tbool e.epos;
		let e1 = type_expr ctx e1 with_type in
		(match e2 with
		| None ->
			if with_type <> NoValue then begin
				let t = ctx.t.tnull e1.etype in
				mk (TIf (e,e1,Some (null t p))) t p
			end else
				mk (TIf (e,e1,None)) ctx.t.tvoid p
		| Some e2 ->
			let e2 = type_expr ctx e2 with_type in
			let t = if with_type = NoValue then ctx.t.tvoid else unify_min ctx [e1; e2] in
			mk (TIf (e,e1,Some e2)) t p)
	| EWhile (cond,e,NormalWhile) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond Value in
		unify ctx cond.etype ctx.t.tbool cond.epos;
		ctx.in_loop <- true;
		let e = type_expr ctx e NoValue in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,NormalWhile)) ctx.t.tvoid p
	| EWhile (cond,e,DoWhile) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let e = type_expr ctx e NoValue in
		ctx.in_loop <- old_loop;
		let cond = type_expr ctx cond Value in
		unify ctx cond.etype ctx.t.tbool cond.epos;
		mk (TWhile (cond,e,DoWhile)) ctx.t.tvoid p
	| ESwitch (e,cases,def) ->
		type_switch ctx e cases def with_type p
	| EReturn e ->
		let e , t = (match e with
			| None ->
				let v = ctx.t.tvoid in
				unify ctx v ctx.ret p;
				None , v
			| Some e ->
				let e = type_expr ctx e (WithType ctx.ret) in
				unify ctx e.etype ctx.ret e.epos;
				Some e , e.etype
		) in
		mk (TReturn e) t_dynamic p
	| EBreak ->
		if not ctx.in_loop then display_error ctx "Break outside loop" p;
		mk TBreak t_dynamic p
	| EContinue ->
		if not ctx.in_loop then display_error ctx "Continue outside loop" p;
		mk TContinue t_dynamic p
	| ETry (e1,catches) ->
		let e1 = type_expr ctx e1 with_type in
		let catches = List.map (fun (v,t,e) ->
			let t = Typeload.load_complex_type ctx (pos e) t in
			let name = (match follow t with
				| TInst ({ cl_path = path },params) | TEnum ({ e_path = path },params) ->
					List.iter (fun pt ->
						if pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
					) params;
					add_feature ctx.com "typed_catch";
					(match path with
					| x :: _ , _ -> x
					| [] , name -> name)
				| TDynamic _ -> ""
				| _ -> error "Catch type must be a class" p
			) in
			let locals = save_locals ctx in
			let v = add_local ctx v t in
			let e = type_expr ctx e with_type in
			locals();
			if with_type <> NoValue then unify ctx e.etype e1.etype e.epos;
			if PMap.mem name ctx.locals then error ("Local variable " ^ name ^ " is preventing usage of this type here") e.epos;
			v , e
		) catches in
		mk (TTry (e1,catches)) (if with_type = NoValue then ctx.t.tvoid else e1.etype) p
	| EThrow e ->
		let e = type_expr ctx e Value in
		mk (TThrow e) (mk_mono()) p
	| ECall (((EConst (Ident s),_) as e),el) ->
		(try
			let t, e, pl = (match with_type with
				| WithType t | WithTypeResume t ->
					(match follow t with
					| TEnum (e,pl) -> t, e, pl
					| _ -> raise Exit)
				| _ -> raise Exit
			) in
			try
				ignore(type_ident_raise ~imported_enums:false ctx s p MCall);
				raise Exit
			with Not_found -> try
				let ef = PMap.find s e.e_constrs in
				let et = apply_params e.e_types pl (monomorphs ef.ef_params ef.ef_type) in
				let constr = mk (fast_enum_field e ef p) et p in
				build_call ctx (AKExpr constr) el (match with_type with WithTypeResume _ -> WithTypeResume t | _ -> WithType t) p
			with Not_found ->
				if ctx.untyped then raise Exit; (* __js__, etc. *)
				with_type_error ctx with_type (string_error s e.e_names "Identifier '" ^ s ^ "' is not part of enum " ^ s_type_path e.e_path) p;
				mk (TConst TNull) t p
		with Exit ->
			type_call ctx e el with_type p)
	| ECall (e,el) ->
		type_call ctx e el with_type p
	| ENew (t,el) ->
		let t = Typeload.load_instance ctx t p true in
		let ct = (match follow t with
			| TAbstract (a,pl) ->
				(match a.a_impl with
				| None -> t
				| Some c -> TInst (c,pl))
			| _ -> t
		) in
		(match follow ct with
		| TInst ({cl_kind = KTypeParameter tl} as c,params) ->
			if not (Codegen.is_generic_parameter ctx c) then error "Only generic type parameters can be constructed" p;
			let el = List.map (fun e -> type_expr ctx e Value) el in
			let ct = (tfun (List.map (fun e -> e.etype) el) ctx.t.tvoid) in
			List.iter (fun t -> match follow t with
				| TAnon a ->
					(try
						unify ctx (PMap.find "new" a.a_fields).cf_type ct p;
					with Not_found ->
						())
				| _ -> ()
			) tl;
			mk (TNew (c,params,el)) t p
		| TInst (c,params) ->
			let ct, f = get_constructor ctx c params p in
			if not (can_access ctx c f true || is_parent c ctx.curclass) && not ctx.untyped then display_error ctx "Cannot access private constructor" p;
			(match f.cf_kind with
			| Var { v_read = AccRequire (r,msg) } -> (match msg with Some msg -> error msg p | None -> error_require r p)
			| _ -> ());
			let el = (match follow ct with
			| TFun (args,r) ->
				(try
					fst (unify_call_params ctx (Some (TInst(c,params),f)) el args r p false)
				with Error (e,p) ->
					display_error ctx (error_msg e) p;
					[])
			| _ ->
				error "Constructor is not a function" p
			) in
			(match c.cl_kind with
			| KAbstractImpl a when not (Meta.has Meta.MultiType a.a_meta) ->
				let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
				let e = mk (TTypeExpr (TClassDecl c)) ta p in
				let e = mk (TField (e,(FStatic (c,f)))) ct p in
				make_call ctx e el t p
			| _ ->
				mk (TNew (c,params,el)) t p)
		| _ ->
			error (s_type (print_context()) t ^ " cannot be constructed") p)
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction (name,f) ->
		let params = Typeload.type_function_params ctx f (match name with None -> "localfun" | Some n -> n) p in
		if params <> [] then begin
			if name = None then display_error ctx "Type parameters not supported in unnamed local functions" p;
			if with_type <> NoValue then error "Type parameters are not supported for rvalue functions" p
		end else
			List.iter (fun tp -> if tp.tp_constraints <> [] then display_error ctx "Type parameters constraints are not supported for local functions" p) f.f_params;
		let old = ctx.type_params in
		ctx.type_params <- params @ ctx.type_params;
		let rt = Typeload.load_type_opt ctx p f.f_type in
		let args = List.map (fun (s,opt,t,c) ->
			let t = Typeload.load_type_opt ctx p t in
			let t, c = Typeload.type_function_param ctx t c opt p in
			s , c, t
		) f.f_args in
		(match with_type with
		| WithType t | WithTypeResume t ->
			(match follow t with
			| TFun (args2,_) when List.length args2 = List.length args ->
				List.iter2 (fun (_,_,t1) (_,_,t2) ->
					match follow t1 with
					| TMono _ -> unify ctx t2 t1 p
					| _ -> ()
				) args args2;
			| _ -> ())
		| _ ->
			());
		let ft = TFun (fun_args args,rt) in
		let inline, v = (match name with
			| None -> false, None
			| Some v when ExtString.String.starts_with v "inline_" -> true, Some (String.sub v 7 (String.length v - 7))
			| Some v -> false, Some v
		) in
		let v = (match v with
			| None -> None
			| Some v ->
				if v.[0] = '$' then display_error ctx "Variables names starting with a dollar are not allowed" p;
				Some (add_local ctx v ft)
		) in
		let e , fargs = Typeload.type_function ctx args rt (match ctx.curfun with FunStatic -> FunStatic | _ -> FunMemberLocal) f p in
		ctx.type_params <- old;
		let f = {
			tf_args = fargs;
			tf_type = rt;
			tf_expr = e;
		} in
		let e = mk (TFunction f) ft p in
		(match v with
		| None -> e
		| Some v ->
			if params <> [] || inline then v.v_extra <- Some (params,if inline then Some e else None);
			let rec loop = function
				| Codegen.Block f | Codegen.Loop f | Codegen.Function f -> f loop
				| Codegen.Use v2 when v == v2 -> raise Exit
				| Codegen.Use _ | Codegen.Declare _ -> ()
			in
			let is_rec = (try Codegen.local_usage loop e; false with Exit -> true) in
			let decl = (if is_rec then begin
				if inline then display_error ctx "Inline function cannot be recursive" e.epos;
				let vnew = add_local ctx v.v_name ft in
				mk (TVars [vnew,Some (mk (TBlock [
					mk (TVars [v,Some (mk (TConst TNull) ft p)]) ctx.t.tvoid p;
					mk (TBinop (OpAssign,mk (TLocal v) ft p,e)) ft p;
					mk (TLocal v) ft p
				]) ft p)]) ctx.t.tvoid p
			end else if inline then
				mk (TBlock []) ctx.t.tvoid p (* do not add variable since it will be inlined *)
			else
				mk (TVars [v,Some e]) ctx.t.tvoid p
			) in
			if with_type <> NoValue && not inline then mk (TBlock [decl;mk (TLocal v) v.v_type p]) v.v_type p else decl)
	| EUntyped e ->
		let old = ctx.untyped in
		ctx.untyped <- true;
		let e = type_expr ctx e with_type in
		ctx.untyped <- old;
		{
			eexpr = e.eexpr;
			etype = mk_mono();
			epos = e.epos;
		}
	| ECast (e,None) ->
		let e = type_expr ctx e Value in
		mk (TCast (e,None)) (mk_mono()) p
	| ECast (e, Some t) ->
		add_feature ctx.com "typed_cast";
		let t = Typeload.load_complex_type ctx (pos e) t in
		let texpr = (match follow t with
		| TInst (_,params) | TEnum (_,params) ->
			List.iter (fun pt ->
				if follow pt != t_dynamic then error "Cast type parameters must be Dynamic" p;
			) params;
			(match follow t with
			| TInst (c,_) ->
				(match c.cl_kind with KTypeParameter _ -> error "Can't cast to a type parameter" p | _ -> ());
				TClassDecl c
			| TEnum (e,_) -> TEnumDecl e
			| _ -> assert false);
		| TAbstract (a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
			List.iter (fun pt ->
				if follow pt != t_dynamic then error "Cast type parameters must be Dynamic" p;
			) params;
			TAbstractDecl a
		| _ ->
			error "Cast type must be a class or an enum" p
		) in
		mk (TCast (type_expr ctx e Value,Some texpr)) t p
	| EDisplay (e,iscall) when Common.defined_value_safe ctx.com Define.DisplayMode = "usage" ->
		let e = try type_expr ctx e Value with Error (Unknown_ident n,_) -> raise (Parser.TypePath ([n],None)) in
		(match e.eexpr with
		| TField(_,fa) -> (match extract_field fa with
			| None -> e
			| Some cf ->
				cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
				e)
		| _ -> e)
	| EDisplay (e,iscall) ->
		let old = ctx.in_display in
		let opt_args args ret = TFun(List.map(fun (n,o,t) -> n,true,t) args,ret) in
		ctx.in_display <- true;
		let e = (try type_expr ctx e Value with Error (Unknown_ident n,_) -> raise (Parser.TypePath ([n],None))) in
		let e = match e.eexpr with
			| TField (e1,fa) ->
				let mode = Common.defined_value_safe ctx.com Define.DisplayMode in
				if field_name fa = "bind" then (match follow e1.etype with
					| TFun(args,ret) -> {e1 with etype = opt_args args ret}
					| _ -> e)
				else if mode = "position" then (match extract_field fa with
					| None -> e
					| Some cf -> raise (Typecore.DisplayPosition [cf.cf_pos]))
				else if mode = "metadata" then (match fa with
					| FStatic (c,cf) | FInstance (c,cf) | FClosure(Some c,cf) -> raise (DisplayMetadata (c.cl_meta @ cf.cf_meta))
					| _ -> e)
				else
					e
			| TTypeExpr mt when Common.defined_value_safe ctx.com Define.DisplayMode = "position" ->
				raise (DisplayPosition [match mt with
					| TClassDecl c -> c.cl_pos
					| TEnumDecl en -> en.e_pos
					| TTypeDecl t -> t.t_pos
					| TAbstractDecl a -> a.a_pos])
			| TTypeExpr mt when Common.defined_value_safe ctx.com Define.DisplayMode = "metadata" ->
				raise (DisplayMetadata (match mt with
					| TClassDecl c -> c.cl_meta
					| TEnumDecl en -> en.e_meta
					| TTypeDecl t -> t.t_meta
					| TAbstractDecl a -> a.a_meta))
			| _ ->
				e
		in
		ctx.in_display <- old;
		let opt_type t =
			match t with
			| TLazy f ->
				Typeload.return_partial_type := true;
				let t = (!f)() in
				Typeload.return_partial_type := false;
				t
			| _ ->
				t
		in
		let rec get_fields t =
			match follow t with
			| TInst (c,params) ->
				let priv = is_parent c ctx.curclass in
				let merge ?(cond=(fun _ -> true)) a b =
					PMap.foldi (fun k f m -> if cond f then PMap.add k f m else m) a b
				in
				let rec loop c params =
					let m = List.fold_left (fun m (i,params) ->
						merge m (loop i params)
					) PMap.empty c.cl_implements in
					let m = (match c.cl_super with
						| None -> m
						| Some (csup,cparams) -> merge m (loop csup cparams)
					) in
					let m = merge ~cond:(fun f -> priv || can_access ctx c f false) c.cl_fields m in
					let m = (match c.cl_kind with
						| KTypeParameter pl -> List.fold_left (fun acc t -> merge acc (get_fields t)) m pl
						| _ -> m
					) in
					PMap.map (fun f -> { f with cf_type = apply_params c.cl_types params (opt_type f.cf_type); cf_public = true; }) m
				in
				loop c params
			| TAbstract({a_impl = Some c} as a,pl) ->
				ctx.m.module_using <- c :: ctx.m.module_using;
				PMap.fold (fun f acc ->
					if f.cf_name <> "_new" && can_access ctx c f true && Meta.has Meta.Impl f.cf_meta then begin
						let f = prepare_using_field f in
						let t = apply_params a.a_types pl (follow f.cf_type) in
						PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type t } acc
					end else
						acc
				) c.cl_statics PMap.empty
			| TAnon a ->
				(match !(a.a_status) with
				| Statics c ->
					PMap.fold (fun f acc -> if can_access ctx c f true then PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type f.cf_type } acc else acc) a.a_fields PMap.empty
				| _ ->
					a.a_fields)
			| TFun (args,ret) ->
				let t = opt_args args ret in
				let cf = mk_field "bind" (tfun [t] t) p in
				PMap.add "bind" cf PMap.empty
			| _ ->
				PMap.empty
		in
		let fields = get_fields e.etype in
		(*
			add 'using' methods compatible with this type
		*)
		let rec loop acc = function
			| [] -> acc
			| c :: l ->
				let acc = ref (loop acc l) in
				let rec dup t = Type.map dup t in
				List.iter (fun f ->
					if not (Meta.has Meta.NoUsing f.cf_meta) then
					let f = { f with cf_type = opt_type f.cf_type } in
					let monos = List.map (fun _ -> mk_mono()) f.cf_params in
					let map = apply_params f.cf_params monos in
					match follow (map f.cf_type) with
					| TFun((_,_,TType({t_path=["haxe";"macro"], "ExprOf"}, [t])) :: args, ret)
					| TFun((_,_,t) :: args, ret) ->
						(try
							unify_raise ctx (dup e.etype) t e.epos;
							List.iter2 (fun m (name,t) -> match follow t with
								| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
									List.iter (fun tc -> unify_raise ctx (dup e.etype) (map tc) e.epos) constr
								| _ -> ()
							) monos f.cf_params;
							if not (can_access ctx c f true) || follow e.etype == t_dynamic && follow t != t_dynamic then
								()
							else begin
								let f = prepare_using_field f in
								let f = { f with cf_params = []; cf_public = true; cf_type = TFun(args,ret) } in
								acc := PMap.add f.cf_name f (!acc)
							end
						with Error (Unify _,_) -> ())
					| _ -> ()
				) c.cl_ordered_statics;
				!acc
		in
		let use_methods = match follow e.etype with TMono _ -> PMap.empty | _ -> loop (loop PMap.empty ctx.g.global_using) ctx.m.module_using in
		let fields = PMap.fold (fun f acc -> PMap.add f.cf_name f acc) fields use_methods in
		let fields = PMap.fold (fun f acc -> if Meta.has Meta.NoCompletion f.cf_meta then acc else f :: acc) fields [] in
		let t = (if iscall then
			match follow e.etype with
			| TFun _ -> e.etype
			| _ -> t_dynamic
		else match fields with
			| [] -> e.etype
			| _ ->
				let get_field acc f =
					List.fold_left (fun acc f -> if f.cf_public then (f.cf_name,f.cf_type,f.cf_doc) :: acc else acc) acc (f :: f.cf_overloads)
				in
				raise (DisplayFields (List.fold_left get_field [] fields))
		) in
		(match follow t with
		| TMono _ | TDynamic _ when ctx.in_macro -> mk (TConst TNull) t p
		| _ -> raise (DisplayTypes [t]))
	| EDisplayNew t ->
		let t = Typeload.load_instance ctx t p true in
		(match follow t with
		| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
			let ct, f = get_constructor ctx c params p in
			raise (DisplayTypes (ct :: List.map (fun f -> f.cf_type) f.cf_overloads))
		| _ ->
			error "Not a class" p)
	| ECheckType (e,t) ->
		let t = Typeload.load_complex_type ctx p t in
		let e = type_expr ctx e (WithType t) in
		unify ctx e.etype t e.epos;
		if e.etype == t then e else mk (TCast (e,None)) t p
	| EMeta (m,e) ->
		let old = ctx.meta in
		ctx.meta <- m :: ctx.meta;
		let e = type_expr ctx e with_type in
		ctx.meta <- old;
		e

and type_call ctx e el (with_type:with_type) p =
	let def () = (match e with
		| EField ((EConst (Ident "super"),_),_) , _ -> ctx.in_super_call <- true
		| _ -> ());
		build_call ctx (type_access ctx (fst e) (snd e) MCall) el with_type p
	in
	match e, el with
	| (EConst (Ident "trace"),p) , e :: el ->
		if Common.defined ctx.com Define.NoTraces then
			null ctx.t.tvoid p
		else
		let params = (match el with [] -> [] | _ -> ["customParams",(EArrayDecl el , p)]) in
		let infos = mk_infos ctx p params in
		if platform ctx.com Js && el = [] && has_dce ctx.com then
			let e = type_expr ctx e Value in
			let infos = type_expr ctx infos Value in
			mk (TCall (mk (TLocal (alloc_var "`trace" t_dynamic)) t_dynamic p,[e;infos])) ctx.t.tvoid p
		else
			type_expr ctx (ECall ((EField ((EField ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[e;EUntyped infos,p]),p) NoValue
	| (EConst(Ident "callback"),p1),args ->
		let ecb = try Some (type_ident_raise ctx "callback" p1 MCall) with Not_found -> None in
		(match ecb with
		| Some ecb ->
			build_call ctx ecb args with_type p
		| None ->
			display_error ctx "callback syntax has changed to func.bind(args)" p;
			let e = type_expr ctx e Value in
			type_bind ctx e args p)
	| (EField (e,"bind"),p), args ->
		let e = type_expr ctx e Value in
		(match follow e.etype with
			| TFun _ -> type_bind ctx e args p
			| _ -> def ())
	| (EConst (Ident "$type"),_) , [e] ->
		let e = type_expr ctx e Value in
		ctx.com.warning (s_type (print_context()) e.etype) e.epos;
		e
	| (EConst (Ident "__unprotect__"),_) , [(EConst (String _),_) as e] ->
		let e = type_expr ctx e Value in
		if Common.platform ctx.com Flash then
			let t = tfun [e.etype] e.etype in
			mk (TCall (mk (TLocal (alloc_var "__unprotect__" t)) t p,[e])) e.etype e.epos
		else
			e
	| (EConst (Ident "super"),sp) , el ->
		if ctx.curfun <> FunConstructor then error "Cannot call super constructor outside class constructor" p;
		let el, t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let ct, f = get_constructor ctx c params p in
			let el, _ = (match follow ct with
			| TFun (args,r) ->
				unify_call_params ctx (Some (TInst(c,params),f)) el args r p false
			| _ ->
				error "Constructor is not a function" p
			) in
			el , TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) ctx.t.tvoid p
	| _ ->
		def ()

and build_call ctx acc el (with_type:with_type) p =
	let fopts t f = match follow t with
		| (TInst (c,pl) as t) -> Some (t,f)
		| (TAnon a) as t -> (match !(a.a_status) with Statics c -> Some (TInst(c,[]),f) | _ -> Some (t,f))
		| _ -> None
	in
	match acc with
	| AKInline (ethis,f,fmode,t) ->
		let params, tfunc = (match follow t with
			| TFun (args,r) -> unify_call_params ctx (fopts ethis.etype f) el args r p true
			| _ -> error (s_type (print_context()) t ^ " cannot be called") p
		) in
		make_call ctx (mk (TField (ethis,fmode)) t p) params (match tfunc with TFun(_,r) -> r | _ -> assert false) p
	| AKUsing (et,cl,ef,eparam) when Meta.has Meta.Generic ef.cf_meta ->
		(match et.eexpr with
		| TField(ec,_) ->
			let el,t,e = type_generic_function ctx (ec,ef) (Interp.make_ast eparam :: el) p in
			make_call ctx e el t p
		| _ -> assert false)
	| AKUsing (et,cl,ef,eparam) ->
		let ef = prepare_using_field ef in
		(match et.eexpr with
		| TField (ec,_) ->
			let acc = type_field ctx ec ef.cf_name p MCall in
			(match acc with
			| AKMacro _ ->
				build_call ctx acc (Interp.make_ast eparam :: el) with_type p
			| AKExpr _ | AKField _ | AKInline _ ->
				let params, tfunc = (match follow et.etype with
					| TFun ( _ :: args,r) -> unify_call_params ctx (Some (TInst(cl,[]),ef)) el args r p (ef.cf_kind = Method MethInline)
					| _ -> assert false
				) in
				let args,r = match tfunc with TFun(args,r) -> args,r | _ -> assert false in
				let et = {et with etype = TFun(("",false,eparam.etype) :: args,r)} in
				make_call ctx et (eparam::params) r p
			| _ -> assert false)
		| _ -> assert false)
	| AKMacro (ethis,f) ->
		if ctx.macro_depth > 300 then error "Stack overflow" p;
		ctx.macro_depth <- ctx.macro_depth + 1;
		let f = (match ethis.eexpr with
		| TTypeExpr (TClassDecl c) ->
			(match ctx.g.do_macro ctx MExpr c.cl_path f.cf_name el p with
			| None -> (fun() -> type_expr ctx (EConst (Ident "null"),p) Value)
			| Some (EVars vl,p) -> (fun() -> type_vars ctx vl p true)
			| Some e -> (fun() -> type_expr ctx (EMeta((Meta.PrivateAccess,[],snd e),e),snd e) with_type))
		| _ ->
			(* member-macro call : since we will make a static call, let's found the actual class and not its subclass *)
			(match follow ethis.etype with
			| TInst (c,_) ->
				let rec loop c =
					if PMap.mem f.cf_name c.cl_fields then
						match ctx.g.do_macro ctx MExpr c.cl_path f.cf_name (Interp.make_ast ethis :: el) p with
						| None -> (fun() -> type_expr ctx (EConst (Ident "null"),p) Value)
						| Some e -> (fun() -> type_expr ctx e Value)
					else
						match c.cl_super with
						| None -> assert false
						| Some (csup,_) -> loop csup
				in
				loop c
			| _ -> assert false)) in
		ctx.macro_depth <- ctx.macro_depth - 1;
		let old = ctx.on_error in
		ctx.on_error <- (fun ctx msg ep ->
			old ctx msg ep;
			(* display additional info in the case the error is not part of our original call *)
			if ep.pfile <> p.pfile || ep.pmax < p.pmin || ep.pmin > p.pmax then old ctx "Called from macro here" p
		);
		let e = try f() with Error (m,p) -> display_error ctx (error_msg m) p; ctx.on_error <- old; raise Fatal_error in
		ctx.on_error <- old;
		e
	| AKNo _ | AKSet _ | AKAccess _ ->
		ignore(acc_get ctx acc p);
		assert false
	| AKExpr e | AKField (e,_,_) ->
		let el , t, e = (match follow e.etype with
		| TFun (args,r) ->
			let fopts = (match acc with
				| AKField (e,f,_) ->
					(match e.eexpr with
					| TField (e,_) -> fopts e.etype f
					| _ -> None)
				| _ ->
					None
			) in
			(match fopts,acc with
				| Some (_,cf),AKField({eexpr = TField(e,_)},_,_) when Meta.has Meta.Generic cf.cf_meta ->
					type_generic_function ctx (e,cf) el p
				| _ ->
					let el, tfunc = unify_call_params ctx fopts el args r p false in
					el,(match tfunc with TFun(_,r) -> r | _ -> assert false), {e with etype = tfunc})
		| TMono _ ->
			let t = mk_mono() in
			let el = List.map (fun e -> type_expr ctx e Value) el in
			unify ctx (tfun (List.map (fun e -> e.etype) el) t) e.etype e.epos;
			el, t, e
		| t ->
			let el = List.map (fun e -> type_expr ctx e Value) el in
			el, (if t == t_dynamic then
				t_dynamic
			else if ctx.untyped then
				mk_mono()
			else
				error (s_type (print_context()) e.etype ^ " cannot be called") e.epos), e
		) in
		mk (TCall (e,el)) t p

and check_to_string ctx t =
	match follow t with
	| TInst (c,_) ->
		(try
			let _, _, f = Type.class_field c "toString" in
			ignore(follow f.cf_type);
		with Not_found ->
			())
	| _ -> ()

(* ---------------------------------------------------------------------- *)
(* FINALIZATION *)

let get_main ctx =
	match ctx.com.main_class with
	| None -> None
	| Some cl ->
		let t = Typeload.load_type_def ctx null_pos { tpackage = fst cl; tname = snd cl; tparams = []; tsub = None } in
		let fmode, ft, r = (match t with
		| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
			error ("Invalid -main : " ^ s_type_path cl ^ " is not a class") null_pos
		| TClassDecl c ->
			try
				let f = PMap.find "main" c.cl_statics in
				let t = Type.field_type f in
				(match follow t with
				| TFun ([],r) -> FStatic (c,f), t, r
				| _ -> error ("Invalid -main : " ^ s_type_path cl ^ " has invalid main function") c.cl_pos);
			with
				Not_found -> error ("Invalid -main : " ^ s_type_path cl ^ " does not have static function main") c.cl_pos
		) in
		let emain = type_type ctx cl null_pos in
		Some (mk (TCall (mk (TField (emain,fmode)) ft null_pos,[])) r null_pos)

let finalize ctx =
	flush_pass ctx PFinal "final"

type state =
	| Generating
	| Done
	| NotYet

let generate ctx =
	let types = ref [] in
	let states = Hashtbl.create 0 in
	let state p = try Hashtbl.find states p with Not_found -> NotYet in
	let statics = ref PMap.empty in

	let rec loop t =
		let p = t_path t in
		match state p with
		| Done -> ()
		| Generating ->
			ctx.com.warning ("Warning : maybe loop in static generation of " ^ s_type_path p) (t_infos t).mt_pos;
		| NotYet ->
			Hashtbl.add states p Generating;
			let t = (match t with
			| TClassDecl c ->
				walk_class p c;
				t
			| TEnumDecl _ | TTypeDecl _ | TAbstractDecl _ ->
				t
			) in
			Hashtbl.replace states p Done;
			types := t :: !types

    and loop_class p c =
		if c.cl_path <> p then loop (TClassDecl c)

	and loop_enum p e =
		if e.e_path <> p then loop (TEnumDecl e)

	and loop_abstract p a =
		if a.a_path <> p then loop (TAbstractDecl a)

	and walk_static_call p c name =
		try
			let f = PMap.find name c.cl_statics in
			match f.cf_expr with
			| None -> ()
			| Some e ->
				if PMap.mem (c.cl_path,name) (!statics) then
					()
				else begin
					statics := PMap.add (c.cl_path,name) () (!statics);
					walk_expr p e;
				end
		with
			Not_found -> ()

	and walk_expr p e =
		match e.eexpr with
		| TTypeExpr t ->
			(match t with
			| TClassDecl c -> loop_class p c
			| TEnumDecl e -> loop_enum p e
			| TAbstractDecl a -> loop_abstract p a
			| TTypeDecl _ -> assert false)
		| TNew (c,_,_) ->
			iter (walk_expr p) e;
			loop_class p c;
			let rec loop c =
				if PMap.mem (c.cl_path,"new") (!statics) then
					()
				else begin
					statics := PMap.add (c.cl_path,"new") () !statics;
					(match c.cl_constructor with
					| Some { cf_expr = Some e } -> walk_expr p e
					| _ -> ());
					match c.cl_super with
					| None -> ()
					| Some (csup,_) -> loop csup
				end
			in
			loop c
		| TMatch (_,(enum,_),_,_) ->
			loop_enum p enum;
			iter (walk_expr p) e
		| TCall (f,_) ->
			iter (walk_expr p) e;
			(* static call for initializing a variable *)
			let rec loop f =
				match f.eexpr with
				| TField ({ eexpr = TTypeExpr t },name) ->
					(match t with
					| TEnumDecl _ -> ()
					| TAbstractDecl _ -> assert false
					| TTypeDecl _ -> assert false
					| TClassDecl c -> walk_static_call p c (field_name name))
				| _ -> ()
			in
			loop f
		| _ ->
			iter (walk_expr p) e

    and walk_class p c =
		(match c.cl_super with None -> () | Some (c,_) -> loop_class p c);
		List.iter (fun (c,_) -> loop_class p c) c.cl_implements;
		(match c.cl_init with
		| None -> ()
		| Some e -> walk_expr p e);
		PMap.iter (fun _ f ->
			match f.cf_expr with
			| None -> ()
			| Some e ->
				match e.eexpr with
				| TFunction _ -> ()
				| _ -> walk_expr p e
		) c.cl_statics

	in
	let sorted_modules = List.sort (fun m1 m2 -> compare m1.m_path m2.m_path) (Hashtbl.fold (fun _ m acc -> m :: acc) ctx.g.modules []) in
	List.iter (fun m -> List.iter loop m.m_types) sorted_modules;
	get_main ctx, List.rev !types, sorted_modules

(* ---------------------------------------------------------------------- *)
(* MACROS *)

let macro_enable_cache = ref false
let macro_interp_cache = ref None
let delayed_macro_result = ref ((fun() -> assert false) : unit -> unit -> Interp.value)

let get_type_patch ctx t sub =
	let new_patch() =
		{ tp_type = None; tp_remove = false; tp_meta = [] }
	in
	let path = Ast.parse_path t in
	let h, tp = (try
		Hashtbl.find ctx.g.type_patches path
	with Not_found ->
		let h = Hashtbl.create 0 in
		let tp = new_patch() in
		Hashtbl.add ctx.g.type_patches path (h,tp);
		h, tp
	) in
	match sub with
	| None -> tp
	| Some k ->
		try
			Hashtbl.find h k
		with Not_found ->
			let tp = new_patch() in
			Hashtbl.add h k tp;
			tp

let macro_timer ctx path =
	Common.timer (if Common.defined ctx.com Define.MacroTimes then "macro " ^ path else "macro execution")

let typing_timer ctx f =
	let t = Common.timer "typing" in
	let old = ctx.com.error and oldp = ctx.pass in
	(*
		disable resumable errors... unless we are in display mode (we want to reach point of completion)
	*)
	if not ctx.com.display then ctx.com.error <- (fun e p -> raise (Error(Custom e,p)));
	if ctx.pass < PTypeField then ctx.pass <- PTypeField;
	let exit() =
		t();
		ctx.com.error <- old;
		ctx.pass <- oldp;
	in
	try
		let r = f() in
		exit();
		r
	with Error (ekind,p) ->
			exit();
			Interp.compiler_error (Typecore.error_msg ekind) p
		| WithTypeError (l,p) ->
			exit();
			Interp.compiler_error (Typecore.error_msg (Unify l)) p
		| e ->
			exit();
			raise e

let make_macro_api ctx p =
	let make_instance = function
		| TClassDecl c -> TInst (c,List.map snd c.cl_types)
		| TEnumDecl e -> TEnum (e,List.map snd e.e_types)
		| TTypeDecl t -> TType (t,List.map snd t.t_types)
		| TAbstractDecl a -> TAbstract (a,List.map snd a.a_types)
	in
	let parse_expr_string s p inl =
		typing_timer ctx (fun() -> parse_expr_string ctx s p inl)
	in
	{
		Interp.pos = p;
		Interp.get_com = (fun() -> ctx.com);
		Interp.get_type = (fun s ->
			typing_timer ctx (fun() ->
				let path = parse_path s in
				try
					let m = Some (Typeload.load_instance ctx { tpackage = fst path; tname = snd path; tparams = []; tsub = None } p true) in
					m
				with Error (Module_not_found _,p2) when p == p2 ->
					None
			)
		);
		Interp.get_module = (fun s ->
			typing_timer ctx (fun() ->
				let path = parse_path s in
				let m = List.map make_instance (Typeload.load_module ctx path p).m_types in
				m
			)
		);
		Interp.on_generate = (fun f ->
			Common.add_filter ctx.com (fun() ->
				let t = macro_timer ctx "onGenerate" in
				f (List.map make_instance ctx.com.types);
				t()
			)
		);
		Interp.on_type_not_found = (fun f ->
			ctx.com.load_extern_type <- (fun path p ->
				match f (s_type_path path) with
				| Interp.VNull -> None
				| td ->
					let (pack,name),tdef,p = Interp.decode_type_def td in
					Some (name,(pack,[tdef,p]))
			) :: ctx.com.load_extern_type;
		);
		Interp.parse_string = parse_expr_string;
		Interp.typeof = (fun e ->
			typing_timer ctx (fun() -> (type_expr ctx e Value).etype)
		);
		Interp.get_display = (fun s ->
			let is_displaying = ctx.com.display in
			let old_resume = !Parser.resume_display in
			let old_error = ctx.on_error in
			let restore () =
				if not is_displaying then begin
					ctx.com.defines <- PMap.remove (fst (Define.infos Define.Display)) ctx.com.defines;
					ctx.com.display <- false
				end;
				Parser.resume_display := old_resume;
				ctx.on_error <- old_error;
			in
			(* temporarily enter display mode with a fake position *)
			if not is_displaying then begin
				Common.define ctx.com Define.Display;
				ctx.com.display <- true;
			end;
			Parser.resume_display := {
				Ast.pfile = "macro";
				Ast.pmin = 0;
				Ast.pmax = 0;
			};
			ctx.on_error <- (fun ctx msg p -> raise (Error(Custom msg,p)));
			let str = try
				let e = parse_expr_string s Ast.null_pos true in
				let e = Optimizer.optimize_completion_expr e in
				ignore (type_expr ctx e Value);
				"NO COMPLETION"
			with DisplayFields fields ->
				let pctx = print_context() in
				String.concat "," (List.map (fun (f,t,_) -> f ^ ":" ^ s_type pctx t) fields)
			| DisplayTypes tl ->
				let pctx = print_context() in
				String.concat "," (List.map (s_type pctx) tl)
			| Parser.TypePath (p,sub) ->
				(match sub with
				| None ->
					"path(" ^ String.concat "." p ^ ")"
				| Some (c,_) ->
					"path(" ^ String.concat "." p ^ ":" ^ c ^ ")")
			| Typecore.Error (msg,p) ->
				"error(" ^ error_msg msg ^ ")"
			in
			restore();
			str
		);
		Interp.type_patch = (fun t f s v ->
			typing_timer ctx (fun() ->
				let v = (match v with None -> None | Some s ->
					match parse_string ctx ("typedef T = " ^ s) null_pos false with
					| ETypedef { d_data = ct } -> Some ct
					| _ -> assert false
				) in
				let tp = get_type_patch ctx t (Some (f,s)) in
				match v with
				| None -> tp.tp_remove <- true
				| Some _ -> tp.tp_type <- v
			);
		);
		Interp.meta_patch = (fun m t f s ->
			let m = (match parse_string ctx (m ^ " typedef T = T") null_pos false with
				| ETypedef t -> t.d_meta
				| _ -> assert false
			) in
			let tp = get_type_patch ctx t (match f with None -> None | Some f -> Some (f,s)) in
			tp.tp_meta <- tp.tp_meta @ m;
		);
		Interp.set_js_generator = (fun gen ->
			let js_ctx = Genjs.alloc_ctx ctx.com in
			ctx.com.js_gen <- Some (fun() ->
				let jsctx = Interp.enc_obj [
					"outputFile", Interp.enc_string ctx.com.file;
					"types", Interp.enc_array (List.map (fun t -> Interp.encode_type (make_instance t)) ctx.com.types);
					"main", (match ctx.com.main with None -> Interp.VNull | Some e -> Interp.encode_texpr e);
					"generateValue", Interp.VFunction (Interp.Fun1 (fun v ->
						match v with
						| Interp.VAbstract (Interp.ATExpr e) ->
							let str = Genjs.gen_single_expr js_ctx e false in
							Interp.enc_string str
						| _ -> failwith "Invalid expression";
					));
					"isKeyword", Interp.VFunction (Interp.Fun1 (fun v ->
						Interp.VBool (Hashtbl.mem Genjs.kwds (Interp.dec_string v))
					));
					"quoteString", Interp.VFunction (Interp.Fun1 (fun v ->
						Interp.enc_string ("\"" ^ Ast.s_escape (Interp.dec_string v) ^ "\"")
					));
					"buildMetaData", Interp.VFunction (Interp.Fun1 (fun t ->
						match Codegen.build_metadata ctx.com (Interp.decode_tdecl t) with
						| None -> Interp.VNull
						| Some e -> Interp.encode_texpr e
					));
					"generateStatement", Interp.VFunction (Interp.Fun1 (fun v ->
						match v with
						| Interp.VAbstract (Interp.ATExpr e) ->
							let str = Genjs.gen_single_expr js_ctx e true in
							Interp.enc_string str
						| _ -> failwith "Invalid expression";
					));
					"setTypeAccessor", Interp.VFunction (Interp.Fun1 (fun callb ->
						js_ctx.Genjs.type_accessor <- (fun t ->
							let v = Interp.encode_type (make_instance t) in
							let ret = Interp.call (Interp.get_ctx()) Interp.VNull callb [v] Nast.null_pos in
							Interp.dec_string ret
						);
						Interp.VNull
					));
					"setCurrentClass", Interp.VFunction (Interp.Fun1 (fun c ->
						Genjs.set_current_class js_ctx (match Interp.decode_tdecl c with TClassDecl c -> c | _ -> assert false);
						Interp.VNull
					));
				] in
				let t = macro_timer ctx "jsGenerator" in
				gen jsctx;
				t()
			);
		);
		Interp.get_local_type = (fun() ->
			match ctx.g.get_build_infos() with
			| Some (mt,_) ->
				Some (match mt with
					| TClassDecl c -> TInst (c,[])
					| TEnumDecl e -> TEnum (e,[])
					| TTypeDecl t -> TType (t,[])
					| TAbstractDecl a -> TAbstract(a,[]))
			| None ->
				if ctx.curclass == null_class then
					None
				else
					Some (TInst (ctx.curclass,[]))
		);
		Interp.get_local_method = (fun() ->
			ctx.curfield.cf_name;
		);
		Interp.get_local_using = (fun() ->
			ctx.m.module_using;
		);
		Interp.get_local_vars = (fun () ->
			ctx.locals;
		);
		Interp.get_build_fields = (fun() ->
			match ctx.g.get_build_infos() with
			| None -> Interp.VNull
			| Some (_,fields) -> Interp.enc_array (List.map Interp.encode_field fields)
		);
		Interp.get_pattern_locals = (fun e t ->
			!get_pattern_locals_ref ctx e t
		);
		Interp.define_type = (fun v ->
			let m, tdef, pos = (try Interp.decode_type_def v with Interp.Invalid_expr -> Interp.exc (Interp.VString "Invalid type definition")) in
			let mdep = Typeload.type_module ctx m ctx.m.curmod.m_extra.m_file [tdef,pos] pos in
			mdep.m_extra.m_kind <- MFake;
			mdep.m_extra.m_time <- -1.;
			add_dependency ctx.m.curmod mdep;
		);
		Interp.module_dependency = (fun mpath file ismacro ->
			let m = typing_timer ctx (fun() -> Typeload.load_module ctx (parse_path mpath) p) in
			if ismacro then
				m.m_extra.m_macro_calls <- file :: List.filter ((<>) file) m.m_extra.m_macro_calls
			else
				add_dependency m (create_fake_module ctx file);
		);
		Interp.current_module = (fun() ->
			ctx.m.curmod
		);
		Interp.delayed_macro = (fun i ->
			let mctx = (match ctx.g.macros with None -> assert false | Some (_,mctx) -> mctx) in
			let f = (try DynArray.get mctx.g.delayed_macros i with _ -> failwith "Delayed macro retrieve failure") in
			f();
			let ret = !delayed_macro_result in
			delayed_macro_result := (fun() -> assert false);
			ret
		);
	}

let rec init_macro_interp ctx mctx mint =
	let p = Ast.null_pos in
	ignore(Typeload.load_module mctx (["haxe";"macro"],"Expr") p);
	ignore(Typeload.load_module mctx (["haxe";"macro"],"Type") p);
	flush_macro_context mint ctx;
	Interp.init mint;
	if !macro_enable_cache && not (Common.defined mctx.com Define.NoMacroCache) then macro_interp_cache := Some mint

and flush_macro_context mint ctx =
	let mctx = (match ctx.g.macros with None -> assert false | Some (_,mctx) -> mctx) in
	finalize mctx;
	let _, types, modules = generate mctx in
	mctx.com.types <- types;
	mctx.com.Common.modules <- modules;
	(* if one of the type we are using has been modified, we need to create a new macro context from scratch *)
	let mint = if List.exists (Interp.has_old_version mint) types then begin
		let com2 = mctx.com in
		let mint = Interp.create com2 (make_macro_api ctx Ast.null_pos) in
		let macro = ((fun() -> Interp.select mint), mctx) in
		ctx.g.macros <- Some macro;
		mctx.g.macros <- Some macro;
		init_macro_interp ctx mctx mint;
		mint
	end else mint in
	(* we should maybe ensure that all filters in Main are applied. Not urgent atm *)
	(try Interp.add_types mint types (Codegen.post_process [Codegen.handle_abstract_casts mctx; Codegen.captured_vars mctx.com; Codegen.rename_local_vars mctx.com])
	with Error (e,p) -> display_error ctx (error_msg e) p; raise Fatal_error);
	Codegen.post_process_end()

let create_macro_interp ctx mctx =
	let com2 = mctx.com in
	let mint, init = (match !macro_interp_cache with
		| None ->
			let mint = Interp.create com2 (make_macro_api ctx Ast.null_pos) in
			mint, (fun() -> init_macro_interp ctx mctx mint)
		| Some mint ->
			mint, (fun() -> ())
	) in
	let on_error = com2.error in
	com2.error <- (fun e p ->
		Interp.set_error (Interp.get_ctx()) true;
		macro_interp_cache := None;
		on_error e p
	);
	let macro = ((fun() -> Interp.select mint), mctx) in
	ctx.g.macros <- Some macro;
	mctx.g.macros <- Some macro;
	(* mctx.g.core_api <- ctx.g.core_api; // causes some issues because of optional args and Null type in Flash9 *)
	init()

let get_macro_context ctx p =
	let api = make_macro_api ctx p in
	match ctx.g.macros with
	| Some (select,ctx) ->
		select();
		api, ctx
	| None ->
		let com2 = Common.clone ctx.com in
		ctx.com.get_macros <- (fun() -> Some com2);
		com2.package_rules <- PMap.empty;
		com2.main_class <- None;
		com2.display <- false;
		List.iter (fun p -> com2.defines <- PMap.remove (platform_name p) com2.defines) platforms;
		com2.defines_signature <- None;
		com2.class_path <- List.filter (fun s -> not (ExtString.String.exists s "/_std/")) com2.class_path;
		com2.class_path <- List.map (fun p -> p ^ "neko" ^ "/_std/") com2.std_path @ com2.class_path;
		let to_remove = List.map (fun d -> fst (Define.infos d)) [Define.NoTraces] in
		let to_remove = to_remove @ List.map (fun (_,d) -> "flash" ^ d) Common.flash_versions in
		com2.defines <- PMap.foldi (fun k v acc -> if List.mem k to_remove then acc else PMap.add k v acc) com2.defines PMap.empty;
		Common.define com2 Define.Macro;
		Common.init_platform com2 Neko;
		let mctx = ctx.g.do_create com2 in
		create_macro_interp ctx mctx;
		api, mctx

let load_macro ctx cpath f p =
	(*
		The time measured here takes into account both macro typing an init, but benchmarks
		shows that - unless you re doing heavy statics vars init - the time is mostly spent in
		typing the classes needed for macro execution.
	*)
	let t = macro_timer ctx "typing (+init)" in
	let api, mctx = get_macro_context ctx p in
	let mint = Interp.get_ctx() in
	let m = (try Hashtbl.find ctx.g.types_module cpath with Not_found -> cpath) in
	let mloaded = Typeload.load_module mctx m p in
	mctx.m <- {
		curmod = mloaded;
		module_types = [];
		module_using = [];
		module_globals = PMap.empty;
		wildcard_packages = [];
	};
	add_dependency ctx.m.curmod mloaded;
	let cl, meth = (match Typeload.load_instance mctx { tpackage = fst cpath; tname = snd cpath; tparams = []; tsub = None } p true with
		| TInst (c,_) ->
			finalize mctx;
			c, (try PMap.find f c.cl_statics with Not_found -> error ("Method " ^ f ^ " not found on class " ^ s_type_path cpath) p)
		| _ -> error "Macro should be called on a class" p
	) in
	let meth = (match follow meth.cf_type with TFun (args,ret) -> args,ret,cl,meth | _ -> error "Macro call should be a method" p) in
	if not ctx.in_macro then flush_macro_context mint ctx;
	t();
	let call args =
		let t = macro_timer ctx (s_type_path cpath ^ "." ^ f) in
		incr stats.s_macros_called;
		let r = Interp.call_path (Interp.get_ctx()) ((fst cpath) @ [snd cpath]) f args api in
		t();
		r
	in
	mctx, meth, call

let type_macro ctx mode cpath f (el:Ast.expr list) p =
	let mctx, (margs,mret,mclass,mfield), call_macro = load_macro ctx cpath f p in
	let mpos = mfield.cf_pos in
	let ctexpr = { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = None } in
	let expr = Typeload.load_instance mctx ctexpr p false in
	(match mode with
	| MExpr ->
		unify mctx mret expr mpos;
	| MBuild ->
		let ctfields = { tpackage = []; tname = "Array"; tparams = [TPType (CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = Some "Field" })]; tsub = None } in
		let tfields = Typeload.load_instance mctx ctfields p false in
		unify mctx mret tfields mpos
	| MMacroType ->
		let cttype = { tpackage = ["haxe";"macro"]; tname = "Type"; tparams = []; tsub = None } in
		let ttype = Typeload.load_instance mctx cttype p false in
		unify mctx mret ttype mpos
	);
	(*
		if the function's last argument is of Array<Expr>, split the argument list and use [] for unify_call_params
	*)
	let el,el2 = match List.rev margs with
		| (_,_,TInst({cl_path=([], "Array")},[e])) :: rest when (try Type.type_eq EqStrict e expr; true with Unify_error _ -> false) ->
			let rec loop (acc1,acc2) el1 el2 = match el1,el2 with
				| [],[] ->
					List.rev acc1, List.rev acc2
				| [], e2 :: [] ->
					(List.rev ((EArrayDecl [],p) :: acc1), [])
				| [], _ ->
					(* not enough arguments, will be handled by unify_call_params *)
					List.rev acc1, List.rev acc2
				| e1 :: l1, e2 :: [] ->
					loop (((EArrayDecl [],p) :: acc1), [e1]) l1 []
				| e1 :: l1, [] ->
					loop (acc1, e1 :: acc2) l1 []
				| e1 :: l1, e2 :: l2 ->
					loop (e1 :: acc1, acc2) l1 l2
			in
			loop ([],[]) el margs
		| _ ->
			el,[]
	in
	let todo = ref [] in
	let args =
		(*
			force default parameter types to haxe.macro.Expr, and if success allow to pass any value type since it will be encoded
		*)
		let eargs = List.map (fun (n,o,t) -> try unify_raise mctx t expr p; (n, o, t_dynamic), true with Error (Unify _,_) -> (n,o,t), false) margs in
		(*
			this is quite tricky here : we want to use unify_call_params which will type our AST expr
			but we want to be able to get it back after it's been padded with nulls
		*)
		let index = ref (-1) in
		let constants = List.map (fun e ->
			let p = snd e in
			let e = (try
				(match Codegen.type_constant_value ctx.com e with
				| { eexpr = TConst (TString _); epos = p } when Lexer.is_fmt_string p ->
					Lexer.remove_fmt_string p;
					todo := (fun() -> Lexer.add_fmt_string p) :: !todo;
				| _ -> ());
				e
			with Error (Custom _,_) ->
				(* if it's not a constant, let's make something that is typed as haxe.macro.Expr - for nice error reporting *)
				(EBlock [
					(EVars ["__tmp",Some (CTPath ctexpr),Some (EConst (Ident "null"),p)],p);
					(EConst (Ident "__tmp"),p);
				],p)
			) in
			(* let's track the index by doing [e][index] (we will keep the expression type this way) *)
			incr index;
			(EArray ((EArrayDecl [e],p),(EConst (Int (string_of_int (!index))),p)),p)
		) el in
		let elt, _ = unify_call_params mctx (Some (TInst(mclass,[]),mfield)) constants (List.map fst eargs) t_dynamic p false in
		List.iter (fun f -> f()) (!todo);
		List.map2 (fun (_,ise) e ->
			let e, et = (match e.eexpr with
				(* get back our index and real expression *)
				| TArray ({ eexpr = TArrayDecl [e] }, { eexpr = TConst (TInt index) }) -> List.nth el (Int32.to_int index), e
				(* added by unify_call_params *)
				| TConst TNull -> (EConst (Ident "null"),e.epos), e
				| _ -> assert false
			) in
			if ise then
				Interp.encode_expr e
			else match Interp.eval_expr (Interp.get_ctx()) et with
				| None -> assert false
				| Some v -> v
		) eargs elt
	in
	let args = match el2 with
		| [] -> args
		| _ -> (match List.rev args with _::args -> List.rev args | [] -> []) @ [Interp.enc_array (List.map Interp.encode_expr el2)]
	in
	let call() =
		match call_macro args with
		| None -> None
		| Some v ->
			try
				Some (match mode with
				| MExpr -> Interp.decode_expr v
				| MBuild ->
					let fields = (match v with
						| Interp.VNull ->
							(match ctx.g.get_build_infos() with
							| None -> assert false
							| Some (_,fields) -> fields)
						| _ ->
							List.map Interp.decode_field (Interp.dec_array v)
					) in
					(EVars ["fields",Some (CTAnonymous fields),None],p)
				| MMacroType ->
					ctx.ret <- Interp.decode_type v;
					(EBlock [],p)
				)
			with Interp.Invalid_expr ->
				error "The macro didn't return a valid result" p
	in
	let e = (if ctx.in_macro then begin
		(*
			this is super-tricky : we can't evaluate a macro inside a macro because we might trigger some cycles.
			So instead, we generate a haxe.macro.Context.delayedCalled(i) expression that will only evaluate the
			macro if/when it is called.

			The tricky part is that the whole delayed-evaluation process has to use the same contextual informations
			as if it was evaluated now.
		*)
		let ctx = {
			ctx with locals = ctx.locals;
		} in
		let pos = DynArray.length mctx.g.delayed_macros in
		DynArray.add mctx.g.delayed_macros (fun() ->
			delayed_macro_result := (fun() ->
				let mint = Interp.get_ctx() in
				match call() with
				| None -> (fun() -> raise Interp.Abort)
				| Some e -> Interp.eval mint (Genneko.gen_expr mint.Interp.gen (type_expr ctx e Value))
			);
		);
		ctx.m.curmod.m_extra.m_time <- -1.; (* disable caching for modules having macro-in-macro *)
		let e = (EConst (Ident "__dollar__delay_call"),p) in
		Some (EUntyped (ECall (e,[EConst (Int (string_of_int pos)),p]),p),p)
	end else
		call()
	) in
	e

let call_macro ctx path meth args p =
	let mctx, (margs,_,mclass,mfield), call = load_macro ctx path meth p in
	let el, _ = unify_call_params mctx (Some (TInst(mclass,[]),mfield)) args margs t_dynamic p false in
	call (List.map (fun e -> try Interp.make_const e with Exit -> error "Parameter should be a constant" e.epos) el)

let call_init_macro ctx e =
	let p = { pfile = "--macro"; pmin = 0; pmax = 0 } in
	let api = make_macro_api ctx p in
	let e = api.Interp.parse_string e p false in
	match fst e with
	| ECall (e,args) ->
		let rec loop e =
			match fst e with
			| EField (e,f) -> f :: loop e
			| EConst (Ident i) -> [i]
			| _ -> error "Invalid macro call" p
		in
		let path, meth = (match loop e with
		| [meth] -> (["haxe";"macro"],"Compiler"), meth
		| meth :: cl :: path -> (List.rev path,cl), meth
		| _ -> error "Invalid macro call" p) in
		ignore(call_macro ctx path meth args p);
	| _ ->
		error "Invalid macro call" p

(* ---------------------------------------------------------------------- *)
(* TYPER INITIALIZATION *)

let rec create com =
	let ctx = {
		com = com;
		t = com.basic;
		g = {
			core_api = None;
			macros = None;
			modules = Hashtbl.create 0;
			types_module = Hashtbl.create 0;
			type_patches = Hashtbl.create 0;
			delayed = [];
			debug_delayed = [];
			delayed_macros = DynArray.create();
			doinline = not (Common.defined com Define.NoInline || com.display);
			hook_generate = [];
			get_build_infos = (fun() -> None);
			std = null_module;
			global_using = [];
			do_inherit = Codegen.on_inherit;
			do_create = create;
			do_macro = type_macro;
			do_load_module = Typeload.load_module;
			do_optimize = Optimizer.reduce_expression;
			do_build_instance = Codegen.build_instance;
		};
		m = {
			curmod = null_module;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
		};
		meta = [];
		pass = PBuildModule;
		macro_depth = 0;
		untyped = false;
		curfun = FunStatic;
		in_loop = false;
		in_super_call = false;
		in_display = false;
		in_macro = Common.defined com Define.Macro;
		ret = mk_mono();
		locals = PMap.empty;
		type_params = [];
		curclass = null_class;
		curfield = null_field;
		tthis = mk_mono();
		opened = [];
		vthis = None;
		on_error = (fun ctx msg p -> ctx.com.error msg p);
	} in
	ctx.g.std <- (try
		Typeload.load_module ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) -> error "Standard library not found" null_pos
	);
	List.iter (fun t ->
		match t with
		| TAbstractDecl a ->
			(match snd a.a_path with
			| "Void" -> ctx.t.tvoid <- TAbstract (a,[]);
			| "Float" -> ctx.t.tfloat <- TAbstract (a,[]);
			| "Int" -> ctx.t.tint <- TAbstract (a,[])
			| "Bool" -> ctx.t.tbool <- TAbstract (a,[])
			| _ -> ());
		| TEnumDecl e ->
			(match snd e.e_path with
			| "Void" -> ctx.t.tvoid <- TEnum (e,[])
			| "Bool" -> ctx.t.tbool <- TEnum (e,[])
			| _ -> ())
		| TClassDecl c ->
			(match snd c.cl_path with
			| "Float" -> ctx.t.tfloat <- TInst (c,[])
			| "Int" -> ctx.t.tint <- TInst (c,[])
			| _ -> ())
		| TTypeDecl td ->
			(match snd td.t_path with
			| "Null" ->
				let mk_null t =
					try
						if not (is_nullable ~no_lazy:true t) then TType (td,[t]) else t
					with Exit ->
						(* don't force lazy evaluation *)
						let r = ref (fun() -> assert false) in
						r := (fun() ->
							let t = (if not (is_nullable t) then TType (td,[t]) else t) in
							r := (fun() -> t);
							t
						);
						TLazy r
				in
				ctx.t.tnull <- if not com.config.pf_static then (fun t -> t) else mk_null;
			| _ -> ());
	) ctx.g.std.m_types;
	let m = Typeload.load_module ctx ([],"String") null_pos in
	(match m.m_types with
	| [TClassDecl c] -> ctx.t.tstring <- TInst (c,[])
	| _ -> assert false);
	let m = Typeload.load_module ctx ([],"Array") null_pos in
	(match m.m_types with
	| [TClassDecl c] -> ctx.t.tarray <- (fun t -> TInst (c,[t]))
	| _ -> assert false);
	let m = Typeload.load_module ctx (["haxe"],"EnumTools") null_pos in
	(match m.m_types with
	| [TClassDecl c1;TClassDecl c2] -> ctx.g.global_using <- c1 :: c2 :: ctx.g.global_using
	| [TClassDecl c1] ->
		let m = Typeload.load_module ctx (["haxe"],"EnumValueTools") null_pos in
		(match m.m_types with
		| [TClassDecl c2 ] -> ctx.g.global_using <- c1 :: c2 :: ctx.g.global_using
		| _ -> assert false);
	| _ -> assert false);
	ctx

;;
unify_min_ref := unify_min;
make_call_ref := make_call;
