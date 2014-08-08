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

type identifier_type =
	| ITLocal of tvar
	| ITMember of tclass * tclass_field
	| ITStatic of tclass * tclass_field
	| ITEnum of tenum * tenum_field
	| ITGlobal of module_type * string * t
	| ITType of module_type
	| ITPackage of string

exception DisplayFields of (string * t * documentation) list
exception DisplayToplevel of identifier_type list

exception WithTypeError of unify_error list * pos

type access_kind =
	| AKNo of string
	| AKExpr of texpr
	| AKSet of texpr * t * tclass_field
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
	| KAbstract of tabstract * t list

let rec classify t =
	match follow t with
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TAbstract({a_impl = Some _} as a,tl) -> KAbstract (a,tl)
	| TAbstract ({ a_path = [],"Int" },[]) -> KInt
	| TAbstract ({ a_path = [],"Float" },[]) -> KFloat
	| TAbstract (a,[]) when List.exists (fun (t,_) -> match classify t with KInt | KFloat -> true | _ -> false) a.a_to -> KParam t
	| TInst ({ cl_kind = KTypeParameter ctl },_) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) ctl -> KParam t
	| TMono r when !r = None -> KUnk
	| TDynamic _ -> KDyn
	| _ -> KOther

let get_iterator_param t =
	match follow t with
	| TAnon a ->
		if !(a.a_status) <> Closed then raise Not_found;
		(match follow (PMap.find "hasNext" a.a_fields).cf_type, follow (PMap.find "next" a.a_fields).cf_type with
		| TFun ([],tb), TFun([],t) when (match follow tb with TAbstract ({ a_path = [],"Bool" },[]) -> true | _ -> false) ->
			if PMap.fold (fun _ acc -> acc + 1) a.a_fields 0 <> 2 then raise Not_found;
			t
		| _ ->
			raise Not_found)
	| _ ->
		raise Not_found

let get_iterable_param t =
	match follow t with
	| TAnon a ->
		if !(a.a_status) <> Closed then raise Not_found;
		(match follow (PMap.find "iterator" a.a_fields).cf_type with
		| TFun ([],it) ->
			let t = get_iterator_param it in
			if PMap.fold (fun _ acc -> acc + 1) a.a_fields 0 <> 1 then raise Not_found;
			t
		| _ ->
			raise Not_found)
	| _ -> raise Not_found

(*
	temporally remove the constant flag from structures to allow larger unification
*)
let remove_constant_flag t callb =
	let tmp = ref [] in
	let rec loop t =
		match follow t with
		| TAnon a ->
			if !(a.a_status) = Const then begin
				a.a_status := Closed;
				tmp := a :: !tmp;
			end;
			PMap.iter (fun _ f -> loop f.cf_type) a.a_fields;
		|  _ ->
			()
	in
	let restore() =
		List.iter (fun a -> a.a_status := Const) (!tmp)
	in
	try
		loop t;
		let ret = callb (!tmp <> []) in
		restore();
		ret
	with e ->
		restore();
		raise e

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

let check_constraints ctx tname tpl tl map delayed p =
	List.iter2 (fun m (name,t) ->
		match follow t with
		| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
			let f = (fun() ->
				List.iter (fun ct ->
					try
						Type.unify (map m) (map ct)
					with Unify_error l ->
						let l = Constraint_failure (tname ^ "." ^ name) :: l in
						raise (Unify_error l)
				) constr
			) in
			if delayed then
				delay ctx PCheckConstraint f
			else
				f()
		| _ ->
			()
	) tl tpl

let enum_field_type ctx en ef tl_en tl_ef p =
	let map t = apply_params en.e_types tl_en (apply_params ef.ef_params tl_ef t) in
	begin try
		check_constraints ctx (s_type_path en.e_path) en.e_types tl_en map true p;
		check_constraints ctx ef.ef_name ef.ef_params tl_ef map true p;
	with Unify_error l ->
		display_error ctx (error_msg (Unify l)) p
	end;
	map ef.ef_type

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
						(* if has_mono m then raise (Unify_error [Unify_custom "Could not resolve full type for constraint checks"; Unify_custom ("Type was " ^ (s_type (print_context()) m))]); *)
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
let rec can_access ctx ?(in_overload=false) c cf stat =
	let c = match c.cl_kind with
		| KGenericInstance(c,_) -> c
		| _ -> c
	in
	if cf.cf_public then
		true
	else if not in_overload && ctx.com.config.pf_overload && Meta.has Meta.Overload cf.cf_meta then
		true
	else
	(* TODO: should we add a c == ctx.curclass short check here? *)
	(* has metadata path *)
	let make_path c f = match c.cl_kind with
		| KAbstractImpl a -> fst a.a_path @ [snd a.a_path; f.cf_name]
		| _ when c.cl_private -> List.rev (f.cf_name :: snd c.cl_path :: (List.tl (List.rev (fst c.cl_path))))
		| _ -> fst c.cl_path @ [snd c.cl_path; f.cf_name]
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
	let cur_paths = ref [] in
	let rec loop c =
		cur_paths := make_path c ctx.curfield :: !cur_paths;
		begin match c.cl_super with
			| Some (csup,_) -> loop csup
			| None -> ()
		end;
		List.iter (fun (c,_) -> loop c) c.cl_implements;
	in
	loop ctx.curclass;
	let is_constr = cf.cf_name = "new" in
	let rec loop c =
		(try
			(* if our common ancestor declare/override the field, then we can access it *)
			let f = if is_constr then (match c.cl_constructor with None -> raise Not_found | Some c -> c) else PMap.find cf.cf_name (if stat then c.cl_statics else c.cl_fields) in
			is_parent c ctx.curclass || (List.exists (has Meta.Allow c f) !cur_paths)
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
	(* TODO: find out what this does and move it to genas3 *)
	if b && Common.defined ctx.com Common.Define.As3 && not (Meta.has Meta.Public cf.cf_meta) then cf.cf_meta <- (Meta.Public,[],cf.cf_pos) :: cf.cf_meta;
	b

(* removes the first argument of the class field's function type and all its overloads *)
let prepare_using_field cf = match cf.cf_type with
	| TFun((_,_,tf) :: args,ret) ->
		let rec loop acc overloads = match overloads with
			| ({cf_type = TFun((_,_,tfo) :: args,ret)} as cfo) :: l ->
				let tfo = apply_params cfo.cf_params (List.map snd cfo.cf_params) tfo in
				(* ignore overloads which have a different first argument *)
				if Type.type_iseq tf tfo then loop ({cfo with cf_type = TFun(args,ret)} :: acc) l else loop acc l
			| _ :: l ->
				loop acc l
			| [] ->
				acc
		in
		{cf with cf_overloads = loop [] cf.cf_overloads; cf_type = TFun(args,ret)}
	| _ -> cf

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
	Lexer.init p.pfile (ExtString.String.ends_with p.pfile ".hx");
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
	match parse_string ctx (head ^ s ^ ";}") p inl with
	| EClass { d_data = [{ cff_name = "main"; cff_kind = FFun { f_expr = Some e } }]} -> if inl then e else loop e
	| _ -> raise Interp.Invalid_expr

let collect_toplevel_identifiers ctx =
	let acc = DynArray.create () in

	(* locals *)
	PMap.iter (fun _ v ->
		DynArray.add acc (ITLocal v)
	) ctx.locals;

	(* member vars *)
	if ctx.curfun <> FunStatic then begin
		let rec loop c =
			List.iter (fun cf ->
				DynArray.add acc (ITMember(ctx.curclass,cf))
			) c.cl_ordered_fields;
			match c.cl_super with
				| None ->
					()
				| Some (csup,tl) ->
					loop csup; (* TODO: type parameters *)
		in
		loop ctx.curclass;
		(* TODO: local using? *)
	end;

	(* statics *)
	List.iter (fun cf ->
		DynArray.add acc (ITStatic(ctx.curclass,cf))
	) ctx.curclass.cl_ordered_statics;

	(* enum constructors *)
	let rec enum_ctors t =
		match t with
		| TClassDecl _ | TAbstractDecl _ ->
			()
		| TTypeDecl t ->
			begin match follow t.t_type with
				| TEnum (e,_) -> enum_ctors (TEnumDecl e)
				| _ -> ()
			end
		| TEnumDecl e ->
			PMap.iter (fun _ ef ->
				DynArray.add acc (ITEnum(e,ef))
			) e.e_constrs;
	in
	List.iter enum_ctors ctx.m.curmod.m_types;
	List.iter enum_ctors ctx.m.module_types;

	(* imported globals *)
	PMap.iter (fun _ (mt,s) ->
		try
			let t = match Typeload.resolve_typedef mt with
				| TClassDecl c -> (PMap.find s c.cl_statics).cf_type
				| TEnumDecl en -> (PMap.find s en.e_constrs).ef_type
				| TAbstractDecl {a_impl = Some c} -> (PMap.find s c.cl_statics).cf_type
				| _ -> raise Not_found
			in
			DynArray.add acc (ITGlobal(mt,s,t))
		with Not_found ->
			()
	) ctx.m.module_globals;

	let module_types = ref [] in

	let add_type mt =
		let path = (t_infos mt).mt_path in
		if not (List.exists (fun mt2 -> (t_infos mt2).mt_path = path) !module_types) then module_types := mt :: !module_types
	in

	(* module types *)
	List.iter add_type ctx.m.curmod.m_types;

	(* module imports *)
	List.iter add_type ctx.m.module_types;

	(* module using *)
	List.iter (fun c ->
		add_type (TClassDecl c)
	) ctx.m.module_using;

	(* TODO: wildcard packages. How? *)

	(* packages and toplevel types *)
	let class_paths = ctx.com.class_path in
	let class_paths = List.filter (fun s -> s <> "") class_paths in

	let packages = ref [] in
	let add_package pack =
		try
			begin match PMap.find pack ctx.com.package_rules with
				| Forbidden ->
					()
				| _ ->
					raise Not_found
			end
		with Not_found ->
			if not (List.mem pack !packages) then packages := pack :: !packages
	in

	List.iter (fun dir ->
		let entries = Sys.readdir dir in
		Array.iter (fun file ->
			match file with
				| "." | ".." ->
					()
				| _ when Sys.is_directory (dir ^ file) ->
					add_package file
				| _ ->
					let l = String.length file in
					if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
						try
							let name = String.sub file 0 (l - 3) in
							let md = Typeload.load_module ctx ([],name) Ast.null_pos in
							List.iter (fun mt ->
								if (t_infos mt).mt_path = md.m_path then add_type mt
							) md.m_types
						with _ ->
							()
					end
		) entries;
	) class_paths;

	List.iter (fun pack ->
		DynArray.add acc (ITPackage pack)
	) !packages;

	List.iter (fun mt ->
		DynArray.add acc (ITType mt)
	) !module_types;

	raise (DisplayToplevel (DynArray.to_list acc))

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
		| TEnum(en,(_ :: _ as tl2)) ->
			tl := (TEnum(en,List.map (fun _ -> t_dynamic) tl2)) :: !tl;
			tl := t :: !tl;
		| TType (td,pl) ->
			loop (apply_params td.t_types pl td.t_type);
			(* prioritize the most generic definition *)
			tl := t :: !tl;
		| TLazy f -> loop (!f())
		| TMono r -> (match !r with None -> () | Some t -> loop t)
		| _ -> tl := t :: !tl)
	in
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
			| TParenthesis e | TMeta(_,e) -> chk_null e
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

let is_forced_inline c cf =
	match c with
	| Some { cl_extern = true } -> true
	| Some { cl_kind = KAbstractImpl _ } -> true
	| _ when Meta.has Meta.Extern cf.cf_meta -> true
	| _ -> false

let rec unify_call_params ctx ?(overloads=None) cf el args r p inline =
  (* 'overloads' will carry a ( return_result ) list, called 'compatible' *)
  (* it's used to correctly support an overload selection algorithm *)
	let overloads, compatible, legacy = match cf, overloads with
		| Some(TInst(c,pl),f), None when ctx.com.config.pf_overload && Meta.has Meta.Overload f.cf_meta ->
				let overloads = List.filter (fun (_,f2) ->
					not (f == f2) && (f2.cf_public || can_access ctx ~in_overload:true c f2 false)
				) (Typeload.get_overloads c f.cf_name) in
				if overloads = [] then (* is static function *)
					let overloads = List.map (fun f -> f.cf_type, f) f.cf_overloads in
					let is_static = f.cf_name <> "new" in
					List.filter (fun (_,f) -> can_access ctx ~in_overload:true c f is_static) overloads, [], false
				else
					overloads, [], false
		| Some(_,f), None ->
				List.map (fun f -> f.cf_type, f) f.cf_overloads, [], true
		| _, Some s ->
				s
		| _ -> [], [], true
	in
	let next ?retval () =
		let compatible = Option.map_default (fun r -> r :: compatible) compatible retval in
		match cf, overloads with
		| Some (TInst(c,pl),_), (ft,o) :: l ->
			let o = { o with cf_type = ft } in
			let args, ret = (match follow (apply_params c.cl_types pl (field_type ctx c pl o p)) with (* I'm getting non-followed types here. Should it happen? *)
				| TFun (tl,t) -> tl, t
				| _ -> assert false
			) in
			Some (unify_call_params ctx ~overloads:(Some (l,compatible,legacy)) (Some (TInst(c,pl),o)) el args ret p inline)
		| Some (t,_), (ft,o) :: l ->
			let o = { o with cf_type = ft } in
			let args, ret = (match Type.field_type o with
				| TFun (tl,t) -> tl, t
				| _ -> assert false
			) in
			Some (unify_call_params ctx ~overloads:(Some (l,compatible,legacy)) (Some (t, o)) el args ret p inline)
		| _ ->
			match compatible with
			| [] -> None
			| [acc,t] -> Some (List.map fst acc, t)
			| comp ->
				match Codegen.Overloads.reduce_compatible compatible with
				| [acc,t] -> Some (List.map fst acc, t)
				| (acc,t) :: _ -> (* ambiguous overload *)
					let name = match cf with | Some(_,f) -> "'" ^ f.cf_name ^ "' " | _ -> "" in
					let format_amb = String.concat "\n" (List.map (fun (_,t) ->
						"Function " ^ name ^ "with type " ^ (s_type (print_context()) t)
					) compatible) in
					display_error ctx ("This call is ambiguous between the following methods:\n" ^ format_amb) p;
					Some (List.map fst acc,t)
				| [] -> None
	in
	let fun_details() =
		let format_arg = (fun (name,opt,_) -> (if opt then "?" else "") ^ name) in
		"Function " ^ (match cf with None -> "" | Some (_,f) -> "'" ^ f.cf_name ^ "' ") ^ "requires " ^ (if args = [] then "no arguments" else "arguments : " ^ String.concat ", " (List.map format_arg args))
	in
	let invalid_skips = ref [] in
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
		| l -> l
	in
	let rec default_value t po =
		if is_pos_infos t then
			let infos = mk_infos ctx p [] in
			let e = type_expr ctx infos (WithType t) in
			(e, true)
		else begin
			if not ctx.com.config.pf_can_skip_non_nullable_argument then begin match po with
				| Some (name,p) when not (is_nullable t) -> invalid_skips := (name,p) :: !invalid_skips;
				| _ -> ()
			end;
			(null (ctx.t.tnull t) p, true)
		end
	in
	let force_inline = match cf with Some(TInst(c,_),f) -> is_forced_inline (Some c) f | _ -> false in
	let rec loop acc l l2 skip =
		match l , l2 with
		| [] , [] ->
			begin match !invalid_skips with
				| [] -> ()
				| skips -> List.iter (fun (name,p) -> display_error ctx ("Cannot skip non-nullable argument " ^ name) p) skips
			end;
			let args,tf = if not (inline && (ctx.g.doinline || force_inline)) && not ctx.com.config.pf_pad_nulls then
				List.rev (no_opt acc), (TFun(args,r))
			else
				List.rev (acc), (TFun(args,r))
			in
			if not legacy && ctx.com.config.pf_overload then
				match next ~retval:(args,tf) () with
				| Some l -> l
				| None ->
					display_error ctx ("No overloaded function matches the arguments. Are the arguments correctly typed?") p;
					List.map fst args, tf
			else
				List.map fst args, tf
		| [] , (_,false,_) :: _ ->
			error (List.fold_left (fun acc (_,_,t) -> default_value t None :: acc) acc l2) "Not enough"
		| [] , (name,true,t) :: l ->
			loop (default_value t None :: acc) [] l skip
		| _ , [] ->
			(match List.rev skip with
			| [] -> error acc "Too many"
			| [name,ul] -> arg_error ul name true p
			| (name,ul) :: _ -> arg_error (Unify_custom ("Invalid arguments\n" ^ fun_details()) :: ul) name true p)
		| ee :: l, (name,opt,t) :: l2 ->
			try
				let e = type_expr ctx ee (WithTypeResume t) in
				(try unify_raise ctx e.etype t e.epos with Error (Unify l,p) -> raise (WithTypeError (l,p)));
				loop ((Codegen.Abstract.check_cast ctx t e p,false) :: acc) l l2 skip
			with
				WithTypeError (ul,p) ->
					if opt then
						loop (default_value t (Some (name,p)) :: acc) (ee :: l) l2 ((name,ul) :: skip)
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
			t_path = [],"Class<" ^ (s_type_path c.cl_path) ^ ">" ;
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
		mk (TTypeExpr (TEnumDecl e)) (TType (e.e_type,types)) p
	| TTypeDecl s ->
		let t = apply_params s.t_types (List.map (fun _ -> mk_mono()) s.t_types) s.t_type in
		Codegen.DeprecationCheck.check_typedef ctx.com s p;
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
			t_path = [],"Abstract<" ^ (s_type_path a.a_path) ^ ">";
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
		let config = match cl with
			| Some ({cl_kind = KAbstractImpl _}) when Meta.has Meta.Impl f.cf_meta ->
				let t = if fname = "_new" then
					t
				else if params = [] then
					error "Invalid abstract implementation function" f.cf_pos
				else
					follow (List.hd params).etype
				in
				begin match t with
					| TAbstract(a,pl) ->
						let has_params = a.a_types <> [] || f.cf_params <> [] in
						let monos = List.map (fun _ -> mk_mono()) f.cf_params in
						let map_type = fun t -> apply_params a.a_types pl (apply_params f.cf_params monos t) in
						Some (has_params,map_type)
					| _ ->
						None
				end
			| _ ->
				None
		in
		ignore(follow f.cf_type); (* force evaluation *)
		let params = List.map (ctx.g.do_optimize ctx) params in
		let force_inline = is_forced_inline cl f in
		(match f.cf_expr with
		| Some { eexpr = TFunction fd } ->
			(match Optimizer.type_inline ctx f fd ethis params t config p force_inline with
			| None ->
				if force_inline then error "Inline could not be done" p;
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
	| AKExpr e -> e
	| AKSet _ | AKAccess _ -> assert false
	| AKUsing (et,c,cf,e) when ctx.in_display ->
		(* Generate a TField node so we can easily match it for position/usage completion (issue #1968) *)
		let ec = type_module_type ctx (TClassDecl c) None p in
		mk (TField(ec,FStatic(c,cf))) et.etype et.epos
	| AKUsing (et,_,cf,e) ->
		(* build a closure with first parameter applied *)
		(match follow et.etype with
		| TFun (_ :: args,ret) ->
			begin match follow e.etype,cf.cf_kind with
				| TAbstract _,Method MethInline -> error "Cannot create closure on abstract inline method" e.epos
				| _ -> ()
			end;
			let tcallb = TFun (args,ret) in
			let twrap = TFun ([("_e",false,e.etype)],tcallb) in
			(* arguments might not have names in case of variable fields of function types, so we generate one (issue #2495) *)
			let args = List.map (fun (n,_,t) -> if n = "" then gen_local ctx t else alloc_var n t) args in
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
			if ctx.com.display <> DMNone then
				mk (TField (e,cmode)) t p
			else
				error "Recursive inline is not supported" p
		| Some { eexpr = TFunction _ } ->
			let chk_class c = (c.cl_extern || Meta.has Meta.Extern f.cf_meta) && not (Meta.has Meta.Runtime f.cf_meta) in
			let wrap_extern c =
				let c2 =
					let m = c.cl_module in
					let mpath = (fst m.m_path @ ["_" ^ snd m.m_path],(snd m.m_path) ^ "_Impl_") in
					try
						let rec loop mtl = match mtl with
							| (TClassDecl c) :: _ when c.cl_path = mpath -> c
							| _ :: mtl -> loop mtl
							| [] -> raise Not_found
						in
						loop c.cl_module.m_types
					with Not_found ->
						let c2 = mk_class c.cl_module mpath c.cl_pos in
						c.cl_module.m_types <- (TClassDecl c2) :: c.cl_module.m_types;
						c2
				in
				let cf = try
					PMap.find f.cf_name c2.cl_statics
				with Not_found ->
					let cf = {f with cf_kind = Method MethNormal} in
					c2.cl_statics <- PMap.add cf.cf_name cf c2.cl_statics;
					c2.cl_ordered_statics <- cf :: c2.cl_ordered_statics;
					cf
				in
				let e_t = type_module_type ctx (TClassDecl c2) None p in
				mk (TField(e_t,FStatic(c2,cf))) t p
			in
			let e_def = mk (TField (e,cmode)) t p in
			begin match follow e.etype with
				| TInst (c,_) when chk_class c ->
					display_error ctx "Can't create closure on an extern inline member method" p;
					e_def
				| TAnon a ->
					begin match !(a.a_status) with
						| Statics c when chk_class c -> wrap_extern c
						| _ -> e_def
					end
				| _ -> e_def
			end
		| Some e ->
			let rec loop e = Type.map_expr loop { e with epos = p } in
			loop e)
	| AKMacro _ ->
		assert false

let error_require r p =
	if r = "" then
		error "This field is not available with the current compilation flags" p
	else
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

let get_this ctx p =
	match ctx.curfun with
	| FunStatic ->
		error "Cannot access this from a static function" p
	| FunMemberClassLocal | FunMemberAbstractLocal ->
		let v = match ctx.vthis with
			| None ->
				let v = if ctx.curfun = FunMemberAbstractLocal then
					PMap.find "this" ctx.locals
				else
					gen_local ctx ctx.tthis
				in
				ctx.vthis <- Some v;
				v
			| Some v ->
				ctx.locals <- PMap.add v.v_name v ctx.locals;
				v
		in
		mk (TLocal v) ctx.tthis p
	| FunMemberAbstract ->
		let v = (try PMap.find "this" ctx.locals with Not_found -> assert false) in
		mk (TLocal v) v.v_type p
	| FunConstructor | FunMember ->
		mk (TConst TThis) ctx.tthis p

let field_access ctx mode f fmode t e p =
	let fnormal() = AKExpr (mk (TField (e,fmode)) t p) in
	let normal() =
		match follow e.etype with
		| TAnon a ->
			(match !(a.a_status) with
			| EnumStatics en ->
				let c = (try PMap.find f.cf_name en.e_constrs with Not_found -> assert false) in
				let fmode = FEnum (en,c) in
				AKExpr (mk (TField (e,fmode)) t p)
			| _ -> fnormal())
		| _ -> fnormal()
	in
	match f.cf_kind with
	| Method m ->
		if mode = MSet && m <> MethDynamic && not ctx.untyped then error "Cannot rebind this method : please use 'dynamic' before method declaration" p;
		begin match ctx.curfun,e.eexpr with
		| (FunMemberAbstract | FunMemberAbstractLocal),TTypeExpr(TClassDecl ({cl_kind = KAbstractImpl a} as c)) when c == ctx.curclass && Meta.has Meta.Impl f.cf_meta ->
			let e = mk (TField(e,fmode)) t p in
			let ethis = get_this ctx p in
			let ethis = {ethis with etype = TAbstract(a,List.map snd a.a_types)} in
			AKUsing(e,ctx.curclass,f,ethis)
		| _ ->
			(match m, mode with
			| MethInline, _ -> AKInline (e,f,fmode,t)
			| MethMacro, MGet -> display_error ctx "Macro functions must be called immediately" p; normal()
			| MethMacro, MCall -> AKMacro (e,f)
			| _ , MGet ->
				let cmode = (match fmode with
					| FInstance(_, cf) | FStatic(_, cf) when Meta.has Meta.Generic cf.cf_meta -> display_error ctx "Cannot create closure on generic function" p; fmode
					| FInstance (c,cf) -> FClosure (Some c,cf)
					| FStatic _ | FEnum _ -> fmode
					| FAnon f -> FClosure (None, f)
					| FDynamic _ | FClosure _ -> assert false
				) in
				AKExpr (mk (TField (e,cmode)) t p)
			| _ -> normal())
		end
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
		| AccCall ->
			let m = (match mode with MSet -> "set_" | _ -> "get_") ^ f.cf_name in
			if m = ctx.curfield.cf_name && (match e.eexpr with TConst TThis -> true | TTypeExpr (TClassDecl c) when c == ctx.curclass -> true | _ -> false) then
				let prefix = (match ctx.com.platform with Flash when Common.defined ctx.com Define.As3 -> "$" | _ -> "") in
				if is_extern_field f then begin
					display_error ctx "This field cannot be accessed because it is not a real variable" p;
					display_error ctx "Add @:isVar here to enable it" f.cf_pos;
				end;
				AKExpr (mk (TField (e,if prefix = "" then fmode else FDynamic (prefix ^ f.cf_name))) t p)
			else if (match e.eexpr with TTypeExpr (TClassDecl ({cl_kind = KAbstractImpl _} as c)) when c == ctx.curclass -> true | _ -> false) then begin
				let this = get_this ctx p in
				if mode = MSet then begin
					let c,a = match ctx.curclass with {cl_kind = KAbstractImpl a} as c -> c,a | _ -> assert false in
					let f = PMap.find m c.cl_statics in
					(* we don't have access to the type parameters here, right? *)
					(* let t = apply_params a.a_types pl (field_type ctx c [] f p) in *)
					let t = (field_type ctx c [] f p) in
					let ef = mk (TField (e,FStatic (c,f))) t p in
					AKUsing (ef,c,f,this)
				end else
					AKExpr (make_call ctx (mk (TField (e,quick_field_dynamic e.etype m)) (tfun [this.etype] t) p) [this] t p)
			end else if mode = MSet then
				AKSet (e,t,f)
			else
				AKExpr (make_call ctx (mk (TField (e,quick_field_dynamic e.etype m)) (tfun [] t) p) [] t p)
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

let rec using_field ctx mode e i p =
	if mode = MSet then raise Not_found;
	(* do not try to find using fields if the type is a monomorph, which could lead to side-effects *)
	let is_dynamic = match follow e.etype with
		| TMono _ -> raise Not_found
		| t -> t == t_dynamic
	in
	let check_constant_struct = ref false in
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
						| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] && not (has_mono m) ->
							List.iter (fun tc -> Type.unify m (map tc)) constr
						| _ -> ()
					) monos cf.cf_params;
					let et = type_module_type ctx (TClassDecl c) None p in
					AKUsing (mk (TField (et,FStatic (c,cf))) t p,c,cf,e)
				| _ ->
					raise Not_found
			end
		with Not_found ->
			loop l
		| Unify_error el ->
			if List.exists (function Has_extra_field _ -> true | _ -> false) el then check_constant_struct := true;
			loop l
	in
	try loop ctx.m.module_using with Not_found ->
	try
		let acc = loop ctx.g.global_using in
		(match acc with
		| AKUsing (_,c,_,_) -> add_dependency ctx.m.curmod c.cl_module
		| _ -> assert false);
		acc
	with Not_found ->
	if not !check_constant_struct then raise Not_found;
	remove_constant_flag e.etype (fun ok -> if ok then using_field ctx mode e i p else raise Not_found)

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
		| (MCall, KAbstractImpl _) | (MGet, _)-> AKExpr(get_this ctx p)
		| _ -> AKNo i)
	| "super" ->
		let t = (match ctx.curclass.cl_super with
			| None -> error "Current class does not have a superclass" p
			| Some (c,params) -> TInst(c,params)
		) in
		(match ctx.curfun with
		| FunMember | FunConstructor -> ()
		| FunMemberAbstract -> error "Cannot access super inside an abstract function" p
		| FunStatic -> error "Cannot access super inside a static function" p;
		| FunMemberClassLocal | FunMemberAbstractLocal -> error "Cannot access super inside a local function" p);
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
		let wrap e = if mode = MSet then
				AKNo i
			else
				AKExpr e
		in
		(* lookup imported enums *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| t :: l ->
				match t with
 				| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta ->
 					begin try
						let cf = PMap.find i c.cl_statics in
						if not (Meta.has Meta.Enum cf.cf_meta) then
							loop l
						else begin
							let et = type_module_type ctx (TClassDecl c) None p in
							AKInline(et,cf,FStatic(c,cf),monomorphs cf.cf_params cf.cf_type)
						end
					with Not_found ->
						loop l
					end
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
						let monos = List.map (fun _ -> mk_mono()) e.e_types in
						let monos2 = List.map (fun _ -> mk_mono()) ef.ef_params in
						wrap (mk (TField (et,FEnum (e,ef))) (enum_field_type ctx e ef monos monos2 p) p)
					with
						Not_found -> loop l
		in
		(try loop (List.rev ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_types)
	with Not_found ->
		(* lookup imported globals *)
		let t, name = PMap.find i ctx.m.module_globals in
		let e = type_module_type ctx t None p in
		type_field ctx e name p mode

and type_field ?(resume=false) ctx e i p mode =
	let no_field() =
		if resume then raise Not_found;
		let t = match follow e.etype with
			| TAnon a -> (match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a} -> TAbstract(a,[])
				| _ -> e.etype)
			| TInst({cl_kind = KAbstractImpl a},_) -> TAbstract(a,[])
			| _ -> e.etype
		in
		let has_special_field a =
			List.exists (fun (_,cf) -> cf.cf_name = i) a.a_ops
			|| List.exists (fun (_,_,cf) -> cf.cf_name = i) a.a_unops
			|| List.exists (fun cf -> cf.cf_name = i) a.a_array
		in
		if not ctx.untyped then begin
			match t with
			| TAbstract(a,_) when has_special_field a ->
				(* the abstract field is not part of the field list, which is only true when it has no expression (issue #2344) *)
				display_error ctx ("Field " ^ i ^ " cannot be called directly because it has no expression") p;
			| _ ->
				display_error ctx (string_error i (string_source t) (s_type (print_context()) t ^ " has no field " ^ i)) p;
		end;
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
					begin match f.cf_kind with
						| Method MethMacro -> display_error ctx "The macro accessor is not allowed for field resolve" f.cf_pos
						| _ -> ()
					end;
					let texpect = tfun [ctx.t.tstring] t in
					let tfield = apply_params c.cl_types params (monomorphs f.cf_params f.cf_type) in
					(try Type.unify tfield texpect
					with Unify_error l ->
						display_error ctx "Field resolve has an invalid type" f.cf_pos;
						display_error ctx (error_msg (Unify [Cannot_unify(tfield,texpect)])) f.cf_pos);
					AKExpr (make_call ctx (mk (TField (e,FInstance (c,f))) tfield p) [Codegen.type_constant ctx.com (String i) p] t p)
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
				| MGet,Var {v_read = AccCall }
				| MSet,Var {v_write = AccCall }
				| MCall,Var {v_read = AccCall } ->
					()
				| MCall, Var _ ->
					display_error ctx "Cannot access superclass variable for calling: needs to be a proper method" p
				| MCall, _ ->
					()
				| MGet,Var _
				| MSet,Var _ when (match c2 with Some { cl_extern = true; cl_path = ("flash" :: _,_) } -> true | _ -> false) ->
					()
				| _, Method _ ->
					display_error ctx "Cannot create closure on super method" p
				| _ ->
					display_error ctx "Normal variables cannot be accessed with 'super', use 'this' instead" p);
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
			if ctx.com.display <> DMNone && i = "iterator" && c.cl_path = (["flash"],"Vector") then begin
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
				| Closed | Extend _ -> () (* always allow anon private fields access *)
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
			if not (can_access ctx c f true) && not ctx.untyped then display_error ctx ("Cannot access private field " ^ i) p;
			let field_type f =
				let t = field_type ctx c [] f p in
				apply_params a.a_types pl t
			in
			let et = type_module_type ctx (TClassDecl c) None p in
			let field_expr f t = mk (TField (et,FStatic (c,f))) t p in
			(match mode, f.cf_kind with
			| (MGet | MCall), Var {v_read = AccCall } ->
				(* getter call *)
				let f = PMap.find ("get_" ^ f.cf_name) c.cl_statics in
				let t = field_type f in
				let r = match follow t with TFun(_,r) -> r | _ -> raise Not_found in
				let ef = field_expr f t in
				AKExpr(make_call ctx ef [e] r p)
			| MSet, Var {v_write = AccCall } ->
				let f = PMap.find ("set_" ^ f.cf_name) c.cl_statics in
				let t = field_type f in
				let ef = field_expr f t in
				AKUsing (ef,c,f,e)
			| (MGet | MCall), Var {v_read = AccNever} ->
				AKNo f.cf_name
			| (MGet | MCall), _ ->
				let t = field_type f in
				begin match follow t with
					| TFun((_,_,t1) :: _,_) -> ()
					| _ -> error ("Invalid call to static function " ^ i ^ " through abstract instance") p
				end;
				let ef = field_expr f t in
				AKUsing (ef,c,f,e)
			| MSet, _ ->
				error "This operation is unsupported" p)
		with Not_found -> try
			let _,el,_ = Meta.get Meta.Forward a.a_meta in
			if not (List.exists (fun e -> match fst e with
				| EConst(Ident s | String s) -> s = i
				| _ -> error "Identifier or string expected as argument to @:forward" (pos e)
			) el) && el <> [] then raise Not_found;
			type_field ctx {e with etype = apply_params a.a_types pl a.a_this} i p mode;
		with Not_found -> try
			using_field ctx mode e i p
		with Not_found -> try
			(match ctx.curfun, e.eexpr with
			| FunMemberAbstract, TConst (TThis) -> type_field ctx {e with etype = apply_params a.a_types pl a.a_this} i p mode;
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
		| (n,o,t) :: _ , (EConst(Ident "_"),p) :: _ when not ctx.com.config.pf_can_skip_non_nullable_argument && o && not (is_nullable t) ->
			error "Usage of _ is not supported for optional non-nullable arguments" p
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
	let e_ret = match follow ret with
		| TAbstract ({a_path = [],"Void"},_) ->
			call
		| TMono _ ->
			delay ctx PFinal (fun () ->
				match follow ret with
				| TAbstract ({a_path = [],"Void"},_) ->
					display_error ctx "Could not bind this function because its Void return type was inferred too late" p;
					error "Consider an explicit type hint" p
				| _ ->
					()
			);
			mk (TReturn (Some call)) t_dynamic p;
		| _ ->
			mk (TReturn (Some call)) t_dynamic p;
	in
	let func = mk (TFunction {
		tf_args = List.map (fun (v,o) -> v, if o then Some TNull else None) missing_args;
		tf_type = ret;
		tf_expr = e_ret;
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
		| TParenthesis e | TMeta(_,e) -> maybe_dynamic_mono e
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

let type_generic_function ctx (e,cf) el ?(using_param=None) with_type p =
	if cf.cf_params = [] then error "Function has no type parameters and cannot be generic" p;
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	let c,stat = match follow e.etype with
		| (TInst (c,_)) -> c,false
		| (TAnon a) -> (match !(a.a_status) with Statics c -> c,true | _ -> assert false)
		| _ -> assert false
	in
	let t = apply_params cf.cf_params monos cf.cf_type in
	add_constraint_checks ctx c.cl_types [] cf monos p;
	let args,ret = match t,using_param with
		| TFun((_,_,ta) :: args,ret),Some e ->
			let ta = if not (Meta.has Meta.Impl cf.cf_meta) then ta
			else match follow ta with TAbstract(a,tl) -> Codegen.Abstract.get_underlying_type a tl | _ -> assert false
			in
			(* manually unify first argument *)
			unify ctx e.etype ta p;
			args,ret
		| TFun(args,ret),None -> args,ret
		| _ ->  error "Invalid field type for generic call" p
	in
	begin match with_type with
		| WithType t -> unify ctx ret t p
		| WithTypeResume t -> (try unify_raise ctx ret t p with Error (Unify l,_) -> raise (WithTypeError(l,p)))
		| _ -> ()
	end;
	let el,_ = unify_call_params ctx None el args ret p false in
	let el = match using_param with None -> el | Some e -> e :: el in
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

let call_to_string ctx c e =
	let et = type_module_type ctx (TClassDecl c) None e.epos in
	let cf = PMap.find "toString" c.cl_statics in
	make_call ctx (mk (TField(et,FStatic(c,cf))) cf.cf_type e.epos) [e] ctx.t.tstring e.epos

let rec type_binop ctx op e1 e2 is_assign_op with_type p =
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) MSet in
		let tt = (match e1 with AKNo _ | AKInline _ | AKUsing _ | AKMacro _ | AKAccess _ -> Value | AKSet(_,t,_) -> WithType t | AKExpr e -> WithType e.etype) in
		let e2 = type_expr ctx e2 tt in
		let e2 = match tt with WithType t -> Codegen.Abstract.check_cast ctx t e2 p | _ -> e2 in
		(match e1 with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e1  ->
			unify ctx e2.etype e1.etype p;
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2 when i1 == i2 -> error "Assigning a value to itself" p
			| TField ({ eexpr = TConst TThis },FInstance (_,f1)) , TField ({ eexpr = TConst TThis },FInstance (_,f2)) when f1 == f2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AKSet (e,t,cf) ->
			unify ctx e2.etype t p;
			make_call ctx (mk (TField (e,quick_field_dynamic e.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [e2] t p
		| AKAccess(ebase,ekey) ->
			let a,pl,c = match follow ebase.etype with TAbstract({a_impl = Some c} as a,pl) -> a,pl,c | _ -> error "Invalid operation" p in
			let cf,tf,r =
				try find_array_access a pl ekey.etype e2.etype true
				with Not_found -> error ("No @:arrayAccess function accepts arguments of " ^ (s_type (print_context()) ekey.etype) ^ " and " ^ (s_type (print_context()) e2.etype)) p
			in
			begin match cf.cf_expr with
				| None ->
					let ea = mk (TArray(ebase,ekey)) r p in
					mk (TBinop(OpAssign,ea,e2)) r p
				| Some _ ->
					let et = type_module_type ctx (TClassDecl c) None p in
					let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
					make_call ctx ef [ebase;ekey;e2] r p
			end
		| AKUsing(ef,_,_,et) ->
			(* this must be an abstract setter *)
			let ret = match follow ef.etype with
				| TFun([_;(_,_,t)],ret) ->
					unify ctx e2.etype t p;
					ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			make_call ctx ef [et;e2] ret p
		| AKInline _ | AKMacro _ ->
			assert false)
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) MSet with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e ->
			let eop = type_binop ctx op e1 e2 true with_type p in
			(match eop.eexpr with
			| TBinop (_,_,e2) ->
				unify ctx eop.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssignOp op,e,e2)) e.etype p;
			| TMeta((Meta.RequiresAssign,_,_),e2) ->
				unify ctx e2.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssign,e,e2)) e.etype p;
			| _ ->
				(* this must be an abstract cast *)
				check_assign ctx e;
				eop)
		| AKSet (e,t,cf) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype in
			let ev = mk (TLocal v) e.etype p in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),cf.cf_name),p) e2 true with_type p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVar (v,Some e)) ctx.t.tvoid p;
				make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [get] t p
			]) t p
 		| AKUsing(ef,c,cf,et) ->
 			(* abstract setter + getter *)
 			let ta = match c.cl_kind with KAbstractImpl a -> TAbstract(a, List.map (fun _ -> mk_mono()) a.a_types) | _ -> assert false in
			let ret = match follow ef.etype with
				| TFun([_;_],ret) -> ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			let l = save_locals ctx in
			let v,is_temp = match et.eexpr with
				| TLocal v when not (v.v_name = "this") -> v,false
				| _ -> gen_local ctx ta,true
			in
			let ev = mk (TLocal v) ta p in
			(* this relies on the fact that cf_name is set_name *)
			let getter_name = String.sub cf.cf_name 4 (String.length cf.cf_name - 4) in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),getter_name),p) e2 true with_type p in
			unify ctx get.etype ret p;
			l();
			let e_call = make_call ctx ef [ev;get] ret p in
			if is_temp then
				mk (TBlock [
					mk (TVar (v,Some et)) ctx.t.tvoid p;
					e_call
				]) ret p
			else
				e_call
		| AKAccess(ebase,ekey) ->
			let a,pl,c = match follow ebase.etype with TAbstract({a_impl = Some c} as a,pl) -> a,pl,c | _ -> error "Invalid operation" p in
			let et = type_module_type ctx (TClassDecl c) None p in
			let cf_get,tf_get,r_get =
				try find_array_access a pl ekey.etype t_dynamic false
				with Not_found -> error ("No @:arrayAccess function accepts an argument of " ^ (s_type (print_context()) ekey.etype)) p
			in
			(* bind complex keys to a variable so they do not make it into the output twice *)
			let ekey,l = match Optimizer.make_constant_expression ctx ekey with
				| Some e -> e, fun () -> None
				| None ->
					let save = save_locals ctx in
					let v = gen_local ctx ekey.etype in
					let e = mk (TLocal v) ekey.etype p in
					e, fun () -> (save(); Some (mk (TVar (v,Some ekey)) ctx.t.tvoid p))
			in
			let ast_call = ECall((EField(Interp.make_ast ebase,cf_get.cf_name),p),[Interp.make_ast ekey]),p in
			let ast_call = (EMeta((Meta.PrivateAccess,[],pos ast_call),ast_call),pos ast_call) in
			let eget = type_binop ctx op ast_call e2 true with_type p in
			unify ctx eget.etype r_get p;
			let cf_set,tf_set,r_set =
				try find_array_access a pl ekey.etype eget.etype true
				with Not_found -> error ("No @:arrayAccess function accepts arguments of " ^ (s_type (print_context()) ekey.etype) ^ " and " ^ (s_type (print_context()) eget.etype)) p
			in
			begin match cf_set.cf_expr,cf_get.cf_expr with
				| None,None ->
					let ea = mk (TArray(ebase,ekey)) r_get p in
					mk (TBinop(OpAssignOp op,ea,type_expr ctx e2 (WithType r_get))) r_set p
				| Some _,Some _ ->
					let ef_set = mk (TField(et,(FStatic(c,cf_set)))) tf_set p in
					(match l() with
					| None -> make_call ctx ef_set [ebase;ekey;eget] r_set p
					| Some e ->
						mk (TBlock [
							e;
							make_call ctx ef_set [ebase;ekey;eget] r_set p
						]) r_set p)
				| _ ->
					error "Invalid array access getter/setter combination" p
			end;
		| AKInline _ | AKMacro _ ->
			assert false)
	| _ ->
	(* If the with_type is an abstract which has exactly one applicable @:op method, we can promote it
	   to the individual arguments (issue #2786). *)
	let wt = match with_type with
		| WithType t | WithTypeResume t ->
			begin match follow t with
				| TAbstract(a,_) ->
					begin match List.filter (fun (o,_) -> o = OpAssignOp(op) || o == op) a.a_ops with
						| [_] -> with_type
						| _ -> Value
					end
				| _ ->
					Value
			end
		| _ ->
			Value
	in
	let e1 = type_expr ctx e1 wt in
	let e2 = type_expr ctx e2 (if op == OpEq || op == OpNotEq then WithType e1.etype else wt) in
	let tint = ctx.t.tint in
	let tfloat = ctx.t.tfloat in
	let tstring = ctx.t.tstring in
	let to_string e =
		let rec loop t = match classify t with
			| KAbstract ({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
				call_to_string ctx c e
			| KInt | KFloat | KString -> e
			| KUnk | KDyn | KParam _ | KOther ->
				let std = type_type ctx ([],"Std") e.epos in
				let acc = acc_get ctx (type_field ctx std "string" e.epos MCall) e.epos in
				ignore(follow acc.etype);
				let acc = (match acc.eexpr with TField (e,FClosure (Some c,f)) -> { acc with eexpr = TField (e,FInstance (c,f)) } | _ -> acc) in
				make_call ctx acc [e] ctx.t.tstring e.epos
			| KAbstract (a,tl) ->
				loop (Codegen.Abstract.get_underlying_type a tl)
		in
		loop e.etype
	in
	let mk_op e1 e2 t =
		if op = OpAdd && (classify t) = KString then
			let e1 = to_string e1 in
			let e2 = to_string e2 in
			mk (TBinop (op,e1,e2)) t p
		else
			mk (TBinop (op,e1,e2)) t p
	in
	let make e1 e2 = match op with
	| OpAdd ->
		mk_op e1 e2 (match classify e1.etype, classify e2.etype with
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
		mk_op e1 e2 i
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
		mk_op e1 e2 !result
	| OpEq
	| OpNotEq ->
		let e1,e2 = try
			unify_raise ctx e1.etype e2.etype p;
			(* we only have to check one type here, because unification fails if one is Void and the other is not *)
			(match follow e2.etype with TAbstract({a_path=[],"Void"},_) -> error "Cannot compare Void" p | _ -> ());
			Codegen.Abstract.check_cast ctx e2.etype e1 p,e2
		with Error (Unify _,_) ->
			unify ctx e2.etype e1.etype p;
			e1,Codegen.Abstract.check_cast ctx e1.etype e2 p
		in
		mk_op e1 e2 ctx.t.tbool
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
		mk_op e1 e2 ctx.t.tbool
	| OpBoolAnd
	| OpBoolOr ->
		let b = ctx.t.tbool in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op e1 e2 b
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
	let find_overload a pl c t left =
		let rec loop ops = match ops with
			| [] -> raise Not_found
			| (o,cf) :: ops when is_assign_op && o = OpAssignOp(op) || o == op ->
				let impl = Meta.has Meta.Impl cf.cf_meta in
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let tcf = apply_params cf.cf_params monos cf.cf_type in
				let tcf = if impl then apply_params a.a_types pl tcf else tcf in
				(match follow tcf with
				| TFun([(_,_,t1);(_,_,t2)],r) ->
					(* implementation fields can only be used in left mode (issue #2130) *)
					if impl && not left then loop ops else begin
						let t1,t2 = if left || Meta.has Meta.Commutative cf.cf_meta then t1,t2 else t2,t1 in
						begin try
							begin
								if impl then
									type_eq EqStrict (Codegen.Abstract.get_underlying_type a pl) t1
								else
									type_eq EqStrict (TAbstract(a,pl)) t1;
							end;
							(* special case for == and !=: if the second type is a monomorph, assume that we want to unify
							   it with the first type to preserve comparison semantics. *)
							begin match op,follow t with
								| (OpEq | OpNotEq),TMono _ ->
									Type.unify (if left then e1.etype else e2.etype) t
								| _ ->
									()
							end;
							Type.unify t t2;
							check_constraints ctx "" cf.cf_params monos (apply_params a.a_types pl) false cf.cf_pos;
							cf,t2,r,o = OpAssignOp(op),Meta.has Meta.Commutative cf.cf_meta
						with Unify_error _ ->
							loop ops
						end
					end;
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
		if is_assign_op && not assign then mk (TMeta((Meta.RequiresAssign,[],ec.epos),ec)) ec.etype ec.epos else ec
	in
	let cast_rec e1t e2t r is_core_type =
		if is_core_type then
			(* we assume that someone declaring a @:coreType knows what he is doing with regards to operation return types (issue #2333) *)
			mk (TBinop(op,e1t,e2t)) r p
		else begin
			let e = make e1t e2t in
			begin try
				unify_raise ctx e.etype r p
			with Error (Unify _,_) ->
				match follow r with
					| TAbstract(a,tl) when type_iseq (Codegen.Abstract.get_underlying_type a tl) e.etype ->
						()
					| _ ->
						error ("The result of this operation (" ^ (s_type (print_context()) e.etype) ^ ") is not compatible with declared return type " ^ (s_type (print_context()) r)) p;
			end;
			{e with etype = r}
		end
	in
	try (match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) ->
			let f,t2,r,assign,_ = find_overload a pl c e2.etype true in
			let e2 = Codegen.Abstract.check_cast ctx t2 e2 e2.epos in
			begin match f.cf_expr with
				| None ->
					let e2 = match follow e2.etype with TAbstract(a,pl) -> {e2 with etype = apply_params a.a_types pl a.a_this} | _ -> e2 in
					cast_rec {e1 with etype = apply_params a.a_types pl a.a_this} e2 r (Meta.has Meta.CoreType a.a_meta)
				| Some _ ->
					mk_cast_op c f a pl e1 e2 r assign
			end
		| _ ->
			raise Not_found)
	with Not_found -> try (match follow e2.etype with
		| TAbstract ({a_impl = Some c} as a,pl) ->
			let f,t2,r,assign,commutative = find_overload a pl c e1.etype false in
			(* let e1,e2 = if commutative then  else e1,Codegen.Abstract.check_cast ctx t2 e2 e2.epos in *)
			let e1,e2,init = if not commutative then
				e1,Codegen.Abstract.check_cast ctx t2 e2 e2.epos,None
			else if not (Optimizer.has_side_effect e1) && not (Optimizer.has_side_effect e2) then
				e2,Codegen.Abstract.check_cast ctx t2 e1 e1.epos,None
			else begin
				let v1,v2 = gen_local ctx e1.etype, gen_local ctx e2.etype in
				let mk_var v e =
					mk (TVar(v,Some e)) ctx.t.tvoid e.epos,mk (TLocal v) e.etype e.epos
				in
				let v1 = mk_var v1 (Codegen.Abstract.check_cast ctx t2 e1 e1.epos) in
				let v2 = mk_var v2 e2 in
				snd v2,snd v1,Some(fst v1,fst v2)
			end in
			let e = match f.cf_expr with
				| None ->
					let e1 = match follow e1.etype with TAbstract(a,pl) -> {e1 with etype = apply_params a.a_types pl a.a_this} | _ -> e1 in
					cast_rec e1 {e2 with etype = apply_params a.a_types pl a.a_this} r (Meta.has Meta.CoreType a.a_meta)
				| Some _ ->
					mk_cast_op c f a pl e1 e2 r assign
			in
			begin match init with
				| None ->
					e
				| Some(e1,e2) ->
					mk (TBlock [
						e1;
						e2;
						e
					]) e.etype e.epos
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
				let rec loop opl = match opl with
					| [] -> raise Not_found
					| (op2,flag2,cf) :: opl when op == op2 && flag == flag2 ->
						let m = mk_mono() in
						let tcf = apply_params c.cl_types pl (monomorphs cf.cf_params cf.cf_type) in
						if Meta.has Meta.Impl cf.cf_meta then begin
							if type_iseq (tfun [apply_params a.a_types pl a.a_this] m) tcf then cf,tcf,m else loop opl
						end else
							if type_iseq (tfun [e.etype] m) tcf then cf,tcf,m else loop opl
					| _ :: opl -> loop opl
				in
				let cf,t,r = try loop a.a_unops with Not_found -> error "Invalid operation" p in
				(match cf.cf_expr with
				| None ->
					let e = {e with etype = apply_params a.a_types pl a.a_this} in
					let e = mk (TUnop(op,flag,e)) r p in
					(* unify ctx r e.etype p; *) (* TODO: I'm not sure why this was here (related to #2295) *)
					e
				| Some _ ->
					let et = type_module_type ctx (TClassDecl c) None p in
					let ef = mk (TField (et,FStatic (c,cf))) t p in
					make_call ctx ef [e] r p)
			| _ -> raise Not_found
		) with Not_found ->
			make e
	in
	match acc with
	| AKExpr e -> access e
	| AKInline _ | AKUsing _ when not set -> access (acc_get ctx acc p)
	| AKNo s ->
		error ("The field or identifier " ^ s ^ " is not accessible for " ^ (if set then "writing" else "reading")) p
	| AKInline _ | AKUsing _ | AKMacro _ | AKAccess _ ->
		error "This kind of operation is not supported" p
	| AKSet (e,t,cf) ->
		let l = save_locals ctx in
		let v = gen_local ctx e.etype in
		let ev = mk (TLocal v) e.etype p in
		let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in
		let one = (EConst (Int "1"),p) in
		let eget = (EField ((EConst (Ident v.v_name),p),cf.cf_name),p) in
		match flag with
		| Prefix ->
			let get = type_binop ctx op eget one false Value p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVar (v,Some e)) ctx.t.tvoid p;
				make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [get] t p
			]) t p
		| Postfix ->
			let v2 = gen_local ctx t in
			let ev2 = mk (TLocal v2) t p in
			let get = type_expr ctx eget Value in
			let plusone = type_binop ctx op (EConst (Ident v2.v_name),p) one false Value p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVar (v,Some e)) ctx.t.tvoid p;
				mk (TVar (v2,Some get)) ctx.t.tvoid p;
				make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [plusone.etype] t) p) [plusone] t p;
				ev2
			]) t p

and type_switch_old ctx e cases def with_type p =
	let eval = type_expr ctx e Value in
	let el = ref [] in
	let type_case_code e =
		let e = (match e with
			| Some e -> type_expr ctx e with_type
			| None -> mk (TBlock []) ctx.com.basic.tvoid Ast.null_pos
		) in
		el := e :: !el;
		e
	in
	let consts = Hashtbl.create 0 in
	let exprs (el,_,e) =
		let el = List.map (fun e ->
			match type_expr ctx e (WithType eval.etype) with
			| { eexpr = TConst c } as e ->
				if Hashtbl.mem consts c then error "Duplicate constant in switch" e.epos;
				Hashtbl.add consts c true;
				e
			| e ->
				e
		) el in
		let locals = save_locals ctx in
		let e = type_case_code e in
		locals();
		el, e
	in
	let cases = List.map exprs cases in
	let def() = (match def with
		| None -> None
		| Some e ->
			let locals = save_locals ctx in
			let e = type_case_code e in
			locals();
			Some e
	) in
	let def = def() in
	let t = if with_type = NoValue then (mk_mono()) else unify_min ctx (List.rev !el) in
	mk (TSwitch (eval,cases,def)) t p

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
				let v = alloc_var i t in
				v.v_meta <- [Meta.Unbound,[],p];
				AKExpr (mk (TLocal v) t p)
		end else begin
			if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			let err = Unknown_ident i in
			if ctx.in_display then raise (Error (err,p));
			if ctx.com.display <> DMNone then begin
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
	| EField (e1,"new") ->
		let e1 = type_expr ctx e1 Value in
		begin match e1.eexpr with
			| TTypeExpr (TClassDecl c) ->
				if mode = MSet then error "Cannot set constructor" p;
				if mode = MCall then error ("Cannot call constructor like this, use 'new " ^ (s_type_path c.cl_path) ^ "()' instead") p;
				let monos = List.map (fun _ -> mk_mono()) c.cl_types in
				let ct, _ = get_constructor ctx c monos p in
				let args = match follow ct with TFun(args,ret) -> args | _ -> assert false in
				let vl = List.map (fun (n,_,t) -> alloc_var n t) args in
				let vexpr v = mk (TLocal v) v.v_type p in
				let t = TInst(c,monos) in
				let ec = mk (TNew(c,monos,List.map vexpr vl)) t p in
				AKExpr(mk (TFunction {
					tf_args = List.map (fun v -> v,None) vl;
					tf_type = t;
					tf_expr = mk (TReturn (Some ec)) t p;
				}) (tfun (List.map (fun v -> v.v_type) vl) t) p)
			| _ -> error "Binding new is only allowed on class types" p
		end;
	| EField _ ->
		let fields ?(resume=false) path e =
			let resume = ref resume in
			let force = ref false in
			let e = List.fold_left (fun e (f,_,p) ->
				let e = acc_get ctx (e MGet) p in
				let f = type_field ~resume:(!resume) ctx e f p in
				force := !resume;
				resume := false;
				f
			) e path in
			if !force then ignore(e MCall); (* not necessarily a call, but prevent #2602 among others *)
			e
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
						let get_static resume t =
							fields ~resume ((sname,true,p) :: path) (fun _ -> AKExpr (type_module_type ctx t None p))
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
									Some (get_static false t)
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
								(* if the static is not found, look for a subtype instead - #1916 *)
								get_static true t
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
		let has_abstract_array_access = ref false in
		(try (match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) when a.a_array <> [] ->
			begin match mode with
			| MSet ->
				(* resolve later *)
				AKAccess (e1, e2)
			| _ ->
				has_abstract_array_access := true;
				let cf,tf,r = find_array_access a pl e2.etype t_dynamic false in
				let e = match cf.cf_expr with
					| None ->
						mk (TArray(e1,e2)) r p
					| Some _ ->
						let et = type_module_type ctx (TClassDecl c) None p in
						let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
						make_call ctx ef [e1;e2] r p
				in
				AKExpr e
			end
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
				with Error(Unify _,_) -> if not ctx.untyped then begin
					if !has_abstract_array_access then error ("No @:arrayAccess function accepts an argument of " ^ (s_type (print_context()) e2.etype)) e1.epos
					else error ("Array access is not allowed on " ^ (s_type (print_context()) e1.etype)) e1.epos
				end);
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
					Some (Codegen.Abstract.check_cast ctx t e p)
			) in
			if v.[0] = '$' && ctx.com.display = DMNone then error "Variables names starting with a dollar are not allowed" p;
			add_local ctx v t, e
		with
			Error (e,p) ->
				display_error ctx (error_msg e) p;
				add_local ctx v t_dynamic, None
	) vl in
	save();

	match vl with
	| [v,eo] ->
		mk (TVar (v,eo)) ctx.t.tvoid p
	| _ ->
		let e = mk (TBlock (List.map (fun (v,e) -> (mk (TVar (v,e)) ctx.t.tvoid p)) vl)) ctx.t.tvoid p in
		mk (TMeta((Meta.MergeBlock,[],p), e)) e.etype e.epos

and with_type_error ctx with_type msg p =
	match with_type with
	| WithTypeResume _ -> raise (WithTypeError ([Unify_custom msg],p))
	| _ -> display_error ctx msg p

and format_string ctx s p =
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
	match !e with
	| None -> assert false
	| Some e -> e

and type_block ctx el with_type p =
	let merge e = match e.eexpr with
		| TMeta((Meta.MergeBlock,_,_), {eexpr = TBlock el}) ->
			el
		| _ -> [e]
	in
	let rec loop = function
		| [] -> []
		| (EVars vl,p) :: l ->
			let e = type_vars ctx vl p true in
			merge e @ loop l
		| [e] ->
			(try
				merge (type_expr ctx e with_type)
			with
				Error (e,p) -> display_error ctx (error_msg e) p; [])
		| e :: l ->
			try
				let e = type_expr ctx e NoValue in
				merge e @ loop l
			with
				Error (e,p) -> display_error ctx (error_msg e) p; loop l
	in
	let l = loop el in
	let rec loop = function
		| [] -> ctx.t.tvoid
		| [e] -> e.etype
		| _ :: l -> loop l
	in
	mk (TBlock l) (loop l) p

and type_expr ctx (e,p) (with_type:with_type) =
	match e with
	| EField ((EConst (String s),p),"code") ->
		if UTF8.length s <> 1 then error "String must be a single UTF8 char" p;
		mk (TConst (TInt (Int32.of_int (UChar.code (UTF8.get s 0))))) ctx.t.tint p
	| EField(_,n) when n.[0] = '$' ->
		error "Field names starting with $ are not allowed" p
	| EConst (Ident s) ->
		if s = "super" && with_type <> NoValue then error "Cannot use super as value" p;
		(try
			acc_get ctx (type_ident_raise ~imported_enums:false ctx s p MGet) p
		with Not_found -> try
			(match with_type with
			| WithType t | WithTypeResume t ->
				(match follow t with
				| TEnum (e,pl) ->
					(try
						let ef = PMap.find s e.e_constrs in
						let monos = List.map (fun _ -> mk_mono()) ef.ef_params in
						mk (fast_enum_field e ef p) (enum_field_type ctx e ef pl monos p) p
					with Not_found ->
						if ctx.untyped then raise Not_found;
						with_type_error ctx with_type (string_error s e.e_names ("Identifier '" ^ s ^ "' is not part of enum " ^ s_type_path e.e_path)) p;
						mk (TConst TNull) t p)
				| TAbstract (a,pl) when has_meta Meta.Enum a.a_meta ->
					let cimpl = (match a.a_impl with None -> assert false | Some c -> c) in
					(try
						let cf = PMap.find s cimpl.cl_statics in
						acc_get ctx (type_field ctx (mk (TTypeExpr (TClassDecl cimpl)) (TAnon { a_fields = PMap.add cf.cf_name cf PMap.empty; a_status = ref (Statics cimpl) }) p) s p MGet) p
					with Not_found ->
						if ctx.untyped then raise Not_found;
						with_type_error ctx with_type (string_error s (List.map (fun f -> f.cf_name) cimpl.cl_ordered_statics) ("Identifier '" ^ s ^ "' is not part of enum " ^ s_type_path a.a_path)) p;
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
		type_expr ctx (format_string ctx s p) with_type
	| EConst c ->
		Codegen.type_constant ctx.com c p
    | EBinop (op,e1,e2) ->
		type_binop ctx op e1 e2 false with_type p
	| EBlock [] when with_type <> NoValue ->
		type_expr ctx (EObjectDecl [],p) with_type
	| EBlock l ->
		let locals = save_locals ctx in
		let e = type_block ctx l with_type p in
		locals();
		e
	| EParenthesis e ->
		let e = type_expr ctx e with_type in
		mk (TParenthesis e) e.etype p
	| EObjectDecl fl ->
		let a = (match with_type with
		| WithType t | WithTypeResume t ->
			(match follow t with
			| TAnon a when not (PMap.is_empty a.a_fields) -> Some a
			| TAbstract (a,tl) when not (Meta.has Meta.CoreType a.a_meta) && List.exists (fun (_,cfo) -> cfo = None) a.a_from ->
				begin match follow (Codegen.Abstract.get_underlying_type a tl) with
					| TAnon a when not (PMap.is_empty a.a_fields) -> Some a
					| _ -> None
				end
			| _ -> None)
		| _ -> None
		) in
		(match a with
		| None ->
			let rec loop (l,acc) (f,e) =
				let f,add = Parser.unquote_ident f in
				if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
				let e = type_expr ctx e Value in
				(match follow e.etype with TAbstract({a_path=[],"Void"},_) -> error "Fields of type Void are not allowed in structures" e.epos | _ -> ());
				let cf = mk_field f e.etype e.epos in
				((f,e) :: l, if add then begin
					if f.[0] = '$' then error "Field names starting with a dollar are not allowed" p;
					PMap.add f cf acc
				end else acc)
			in
			let fields , types = List.fold_left loop ([],PMap.empty) fl in
			let x = ref Const in
			ctx.opened <- x :: ctx.opened;
			mk (TObjectDecl (List.rev fields)) (TAnon { a_fields = types; a_status = x }) p
		| Some a ->
			let fields = ref PMap.empty in
			let extra_fields = ref [] in
			let fl = List.map (fun (n, e) ->
				let n,add = Parser.unquote_ident n in
				if PMap.mem n !fields then error ("Duplicate field in object declaration : " ^ n) p;
				let e = try
					let t = (PMap.find n a.a_fields).cf_type in
					let e = type_expr ctx e (match with_type with WithTypeResume _ -> WithTypeResume t | _ -> WithType t) in
					let e = Codegen.Abstract.check_cast ctx t e p in
					unify ctx e.etype t e.epos;
					(try type_eq EqStrict e.etype t; e with Unify_error _ -> mk (TCast (e,None)) t e.epos)
				with Not_found ->
					extra_fields := n :: !extra_fields;
					type_expr ctx e Value
				in
				if add then begin
					if n.[0] = '$' then error "Field names starting with a dollar are not allowed" p;
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
				(match PMap.foldi (fun n cf acc -> if not (Meta.has Meta.Optional cf.cf_meta) && not (PMap.mem n !fields) then n :: acc else acc) a.a_fields [] with
					| [] -> ()
					| [n] -> unify_error [Unify_custom ("Object requires field " ^ n)] p
					| nl -> unify_error [Unify_custom ("Object requires fields: " ^ (String.concat ", " nl))] p);
				(match !extra_fields with
				| [] -> ()
				| _ -> unify_error (List.map (fun n -> has_extra_field t n) !extra_fields) p);
			end;
			if !(a.a_status) <> Const then a.a_status := Closed;
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
			mk (TVar (v,Some ea)) ctx.t.tvoid p;
			efor;
			mk (TLocal v) v.v_type p;
		]) v.v_type p
	| EArrayDecl ((EBinop(OpArrow,_,_),_) as e1 :: el) ->
		let keys = Hashtbl.create 0 in
		let (tkey,tval),resume =
			let get_map_params t = match follow t with
				| TAbstract({a_path=[],"Map"},[tk;tv]) -> tk,tv
				| _ -> mk_mono(),mk_mono()
			in
			match with_type with
			| WithType t -> get_map_params t,false
			| WithTypeResume t -> get_map_params t,true
			| _ -> (mk_mono(),mk_mono()),false
		in
		let unify_with_resume ctx a b p =
			if resume then try unify_raise ctx a b p with Error (Unify l,p) -> raise (WithTypeError(l,p))
			else unify ctx a b p
		in
		let type_arrow e1 e2 =
			let e1 = type_expr ctx e1 (WithType tkey) in
			try
				let p = Hashtbl.find keys e1.eexpr in
				display_error ctx "Duplicate key" e1.epos;
				error "Previously defined here" p
			with Not_found ->
				Hashtbl.add keys e1.eexpr e1.epos;
				unify_with_resume ctx e1.etype tkey e1.epos;
				let e2 = type_expr ctx e2 (WithType tval) in
				unify_with_resume ctx e2.etype tval e2.epos;
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
		let el = (mk (TVar (v,Some enew)) t_dynamic p) :: (List.rev el) in
		mk (TBlock el) tmap p
	| EArrayDecl el ->
		let tp = (match with_type with
		| WithType t | WithTypeResume t ->
			(match follow t with
			| TInst ({ cl_path = [],"Array" },[tp]) ->
				(match follow tp with
				| TMono _ -> None
				| _ -> Some tp)
			| TAnon _ ->
				(try
					Some (get_iterable_param t)
				with Not_found ->
					None)
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
				if ctx.untyped then t_dynamic else begin
					display_error ctx "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>" p;
					raise (Error (Unify l, p))
				end
			in
			mk (TArrayDecl el) (ctx.t.tarray t) p
		| Some t ->
			let el = List.map (fun e ->
				let e = type_expr ctx e (match with_type with WithTypeResume _ -> WithTypeResume t | _ -> WithType t) in
				(match with_type with
				| WithTypeResume _ -> (try unify_raise ctx e.etype t e.epos with Error (Unify l,p) -> raise (WithTypeError (l,p)))
				| _ -> unify ctx e.etype t e.epos);
				Codegen.Abstract.check_cast ctx t e p
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
						Codegen.Abstract.check_cast ctx t e1 p
					with Error (Unify _,_) ->
						let acc = build_call ctx (type_field ctx e1 "iterator" e1.epos MCall) [] Value e1.epos in
						try
							unify_raise ctx acc.etype t acc.epos;
							acc
						with Error (Unify(l),p) ->
							display_error ctx "Field iterator has an invalid type" acc.epos;
							display_error ctx (error_msg (Unify l)) p;
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
					let enext = mk (TVar (i,Some (make_call ctx (mk (TField (eit,FDynamic "next")) (TFun ([],pt)) p) [] pt p))) ctx.t.tvoid p in
					let eblock = (match e2.eexpr with
						| TBlock el -> { e2 with eexpr = TBlock (enext :: el) }
						| _ -> mk (TBlock [enext;e2]) ctx.t.tvoid p
					) in
					mk (TBlock [
						mk (TVar (tmp,Some e1)) ctx.t.tvoid p;
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
		let e = Codegen.Abstract.check_cast ctx ctx.t.tbool e p in
		let e1 = type_expr ctx e1 with_type in
		(match e2 with
		| None ->
			mk (TIf (e,e1,None)) ctx.t.tvoid p
		| Some e2 ->
			let e2 = type_expr ctx e2 with_type in
			let e1,e2,t = match with_type with
				| NoValue -> e1,e2,ctx.t.tvoid
				| Value -> e1,e2,unify_min ctx [e1; e2]
				| WithType t | WithTypeResume t when (match follow t with TMono _ -> true | _ -> false) -> e1,e2,unify_min ctx [e1; e2]
				| WithType t | WithTypeResume t ->
					begin try
						unify_raise ctx e1.etype t e1.epos;
						unify_raise ctx e2.etype t e2.epos;
					with Error (Unify l,p) -> match with_type with
						| WithTypeResume _ -> raise (WithTypeError (l,p))
						| _ -> display_error ctx (error_msg (Unify l)) p
					end;
					let e1 = Codegen.Abstract.check_cast ctx t e1 e1.epos in
					let e2 = Codegen.Abstract.check_cast ctx t e2 e2.epos in
					e1,e2,t
			in
			mk (TIf (e,e1,Some e2)) t p)
	| EWhile (cond,e,NormalWhile) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond Value in
		unify ctx cond.etype ctx.t.tbool cond.epos;
		let cond = Codegen.Abstract.check_cast ctx ctx.t.tbool cond p in
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
		let cond = Codegen.Abstract.check_cast ctx ctx.t.tbool cond p in
		mk (TWhile (cond,e,DoWhile)) ctx.t.tvoid p
	| ESwitch (e1,cases,def) ->
		begin try
			let dt = match_expr ctx e1 cases def with_type p in
			let wrap e1 = if not dt.dt_is_complex then e1 else mk (TMeta((Meta.Ast,[e,p],p),e1)) e1.etype e1.epos in
			if not ctx.in_macro && not (Common.defined ctx.com Define.Interp) && ctx.com.config.pf_pattern_matching && dt.dt_is_complex then
				wrap (mk (TPatMatch dt) dt.dt_type p)
			else
				wrap (Codegen.PatternMatchConversion.to_typed_ast ctx dt p)
		with Exit ->
			type_switch_old ctx e1 cases def with_type p
		end
	| EReturn e ->
		let e , t = (match e with
			| None ->
				let v = ctx.t.tvoid in
				unify ctx v ctx.ret p;
				None , v
			| Some e ->
				let e = type_expr ctx e (WithType ctx.ret) in
				unify ctx e.etype ctx.ret e.epos;
				let e = Codegen.Abstract.check_cast ctx ctx.ret e p in
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
		let rec check_unreachable cases t p = match cases with
			| (v,e) :: cases ->
				let unreachable () =
					display_error ctx "This block is unreachable" p;
					let st = s_type (print_context()) in
					display_error ctx (Printf.sprintf "%s can be assigned to %s, which is handled here" (st t) (st v.v_type)) e.epos
				in
				begin try
					begin match follow t,follow v.v_type with
						| TDynamic _, TDynamic _ ->
							unreachable()
						| TDynamic _,_ ->
							()
						| _ ->
							Type.unify t v.v_type;
							unreachable()
					end
				with Unify_error _ ->
					check_unreachable cases t p
				end
			| [] ->
				()
		in
		let catches = List.fold_left (fun acc (v,t,e) ->
			let t = Typeload.load_complex_type ctx (pos e) t in
			let rec loop t = match follow t with
				| TInst ({ cl_kind = KTypeParameter _} as c,_) when not (Typeload.is_generic_parameter ctx c) ->
					error "Cannot catch non-generic type parameter" p
				| TInst ({ cl_path = path },params)
				| TEnum ({ e_path = path },params) ->
					List.iter (fun pt ->
						if pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
					) params;
					(match path with
					| x :: _ , _ -> x
					| [] , name -> name),t
				| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
					loop (Codegen.Abstract.get_underlying_type a tl)
				| TDynamic _ -> "",t
				| _ -> error "Catch type must be a class, an enum or Dynamic" (pos e)
			in
			let name,t2 = loop t in
			if v.[0] = '$' then display_error ctx "Catch variable names starting with a dollar are not allowed" p;
			check_unreachable acc t2 (pos e);
			let locals = save_locals ctx in
			let v = add_local ctx v t in
			let e = type_expr ctx e with_type in
			v.v_type <- t2;
			locals();
			if with_type <> NoValue then unify ctx e.etype e1.etype e.epos;
			if PMap.mem name ctx.locals then error ("Local variable " ^ name ^ " is preventing usage of this type here") e.epos;
			(v , e) :: acc
		) [] catches in
		mk (TTry (e1,List.rev catches)) (if with_type = NoValue then ctx.t.tvoid else e1.etype) p
	| EThrow e ->
		let e = type_expr ctx e Value in
		mk (TThrow e) (mk_mono()) p
	| ECall (((EConst (Ident s),pc) as e),el) ->
		(try
			let en,t = (match with_type with
				| WithType t | WithTypeResume t ->
					(match follow t with
					| TEnum (e,pl) -> e,t
					| _ -> raise Exit)
				| _ -> raise Exit
			) in
			let old = ctx.on_error,ctx.m.curmod.m_types in
			ctx.m.curmod.m_types <- ctx.m.curmod.m_types @ [(TEnumDecl en)];
			let restore = fun () ->
				ctx.m.curmod.m_types <- snd old;
				ctx.on_error <- fst old;
			in
			ctx.on_error <- (fun ctx msg ep ->
				(* raise Not_found only if the error is actually about the outside identifier (issue #2148) *)
				if ep = pc then
					raise Not_found
				else begin
					restore();
					ctx.on_error ctx msg ep;
				end
			);
			begin try
				let e = type_call ctx e el with_type p in
				restore();
				e
			with Not_found ->
				restore();
				if ctx.untyped then raise Exit; (* __js__, etc. *)
				with_type_error ctx with_type (string_error s en.e_names ("Identifier '" ^ s ^ "' is not part of enum " ^ s_type_path en.e_path)) p;
				mk (TConst TNull) t p
			| err ->
				restore();
				raise err
			end
		with Exit ->
			type_call ctx e el with_type p)
	| ECall (e,el) ->
		type_call ctx e el with_type p
	| ENew (t,el) ->
		let unify_constructor_call c params f ct = match follow ct with
			| TFun (args,r) ->
				(try
					fst (unify_call_params ctx (Some (TInst(c,params),f)) el args r p false)
				with Error (e,p) ->
					display_error ctx (error_msg e) p;
					[])
			| _ ->
				error "Constructor is not a function" p
		in
		let t = try
			ctx.constructor_argument_stack <- el :: ctx.constructor_argument_stack;
			let t = follow (Typeload.load_instance ctx t p true) in
			ctx.constructor_argument_stack <- List.tl ctx.constructor_argument_stack;
			t
		with Codegen.Generic_Exception _ ->
			(* Try to infer generic parameters from the argument list (issue #2044) *)
			match Typeload.load_type_def ctx p t with
			| TClassDecl ({cl_constructor = Some cf} as c) ->
				let monos = List.map (fun _ -> mk_mono()) c.cl_types in
				let ct, f = get_constructor ctx c monos p in
				ignore (unify_constructor_call c monos f ct);
				Codegen.build_generic ctx c p monos
			| mt ->
				error ((s_type_path (t_infos mt).mt_path) ^ " cannot be constructed") p
		in
		let ct = (match t with
			| TAbstract (a,pl) ->
				(match a.a_impl with
				| None -> t
				| Some c -> TInst (c,pl))
			| _ -> t
		) in
		(match ct with
		| TInst ({cl_kind = KTypeParameter tl} as c,params) ->
			if not (Typeload.is_generic_parameter ctx c) then error "Only generic type parameters can be constructed" p;
			let el = List.map (fun e -> type_expr ctx e Value) el in
			let ct = (tfun (List.map (fun e -> e.etype) el) ctx.t.tvoid) in
			if not (List.exists (fun t -> match follow t with
				| TAnon a ->
					(try
						unify ctx (PMap.find "new" a.a_fields).cf_type ct p;
						true
					with Not_found ->
						 false)
				| _ -> false
			) tl) then error (s_type_path c.cl_path ^ " does not have a constructor") p;
			mk (TNew (c,params,el)) t p
		| TInst (c,params) ->
			let ct, f = get_constructor ctx c params p in
			if not (can_access ctx c f true || is_parent c ctx.curclass) && not ctx.untyped then display_error ctx "Cannot access private constructor" p;
			(match f.cf_kind with
			| Var { v_read = AccRequire (r,msg) } -> (match msg with Some msg -> error msg p | None -> error_require r p)
			| _ -> ());
			let el = unify_constructor_call c params f ct in
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
		end;
		List.iter (fun tp -> if tp.tp_constraints <> [] then display_error ctx "Type parameter constraints are not supported for local functions" p) f.f_params;
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
			let rec loop t =
				(match follow t with
				| TFun (args2,tr) when List.length args2 = List.length args ->
					List.iter2 (fun (_,_,t1) (_,_,t2) ->
						match follow t1 with
						| TMono _ -> unify ctx t2 t1 p
						| _ -> ()
					) args args2;
					(* unify for top-down inference unless we are expecting Void *)
					begin match follow tr with
						| TAbstract({a_path = [],"Void"},_) -> ()
						| _ -> unify ctx rt tr p
					end
				| TAbstract(a,tl) ->
					loop (Codegen.Abstract.get_underlying_type a tl)
				| _ -> ())
			in
			loop t
		| NoValue ->
			if name = None then display_error ctx "Unnamed lvalue functions are not supported" p
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
				if v.[0] = '$' then display_error ctx "Variable names starting with a dollar are not allowed" p;
				Some (add_local ctx v ft)
		) in
		let curfun = match ctx.curfun with
			| FunStatic -> FunStatic
			| FunMemberAbstract -> FunMemberAbstractLocal
			| _ -> FunMemberClassLocal
		in
		let e , fargs = Typeload.type_function ctx args rt curfun f false p in
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
				| Filters.Block f | Filters.Loop f | Filters.Function f -> f loop
				| Filters.Use v2 when v == v2 -> raise Exit
				| Filters.Use _ | Filters.Declare _ -> ()
			in
			let is_rec = (try Filters.local_usage loop e; false with Exit -> true) in
			let decl = (if is_rec then begin
				if inline then display_error ctx "Inline function cannot be recursive" e.epos;
				let vnew = add_local ctx v.v_name ft in
				mk (TVar (vnew,Some (mk (TBlock [
					mk (TVar (v,Some (mk (TConst TNull) ft p))) ctx.t.tvoid p;
					mk (TBinop (OpAssign,mk (TLocal v) ft p,e)) ft p;
					mk (TLocal v) ft p
				]) ft p))) ctx.t.tvoid p
			end else if inline then
				mk (TBlock []) ctx.t.tvoid p (* do not add variable since it will be inlined *)
			else
				mk (TVar (v,Some e)) ctx.t.tvoid p
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
		let t = Typeload.load_complex_type ctx (pos e) t in
		let rec loop t = match follow t with
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
			| TAbstract (a,params) ->
				loop (Codegen.Abstract.get_underlying_type a params)
			| _ ->
				error "Cast type must be a class or an enum" p
		in
		let texpr = loop t in
		mk (TCast (type_expr ctx e Value,Some texpr)) t p
	| EDisplay (e,iscall) ->
		handle_display ctx e iscall p
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
		let e = Codegen.Abstract.check_cast ctx t e p in
		unify ctx e.etype t e.epos;
		if e.etype == t then e else mk (TCast (e,None)) t p
	| EMeta (m,e1) ->
		let old = ctx.meta in
		ctx.meta <- m :: ctx.meta;
		let e () = type_expr ctx e1 with_type in
		let e = match m with
			| (Meta.ToString,_,_) ->
				let e = e() in
				(match follow e.etype with
					| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics -> call_to_string ctx c e
					| _ -> e)
			| (Meta.This,_,_) ->
				let e = List.hd ctx.this_stack in
				let rec loop e = match e.eexpr with
					| TConst TThis -> get_this ctx e.epos
					| _ -> Type.map_expr loop e
				in
				loop e
			| _ -> e()
		in
		ctx.meta <- old;
		e

and handle_display ctx e iscall p =
	let old = ctx.in_display in
	ctx.in_display <- true;
	let e = try
		type_expr ctx e Value
	with Error (Unknown_ident n,_) when not iscall ->
		raise (Parser.TypePath ([n],None))
	| Error (Unknown_ident "trace",_) ->
		raise (DisplayTypes [tfun [t_dynamic] ctx.com.basic.tvoid])
	in
	ctx.in_display <- old;
	let handle_field cf =
		if ctx.com.display = DMPosition then
			raise (DisplayPosition [cf.cf_pos]);
		cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
	in
	match ctx.com.display with
	| DMUsage | DMPosition ->
		(* print_endline (s_expr (s_type (print_context())) e); *)
		begin match e.eexpr with
		| TField(_,FEnum(_,ef)) ->
			if ctx.com.display = DMPosition then
				raise (DisplayPosition [ef.ef_pos]);
			ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
		| TField(_,(FAnon cf | FInstance (_,cf) | FStatic (_,cf) | FClosure (_,cf))) ->
			handle_field cf;
		| TLocal v ->
			v.v_meta <- (Meta.Usage,[],p) :: v.v_meta;
		| TTypeExpr mt ->
			let ti = t_infos mt in
			if ctx.com.display = DMPosition then
				raise (DisplayPosition [ti.mt_pos]);
			ti.mt_meta <- (Meta.Usage,[],p) :: ti.mt_meta;
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				handle_field cf;
			with Not_found ->
				()
			end
		| _ ->
			()
		end;
		e
	| DMToplevel ->
		collect_toplevel_identifiers ctx;
	| DMDefault | DMNone ->
		let opt_args args ret = TFun(List.map(fun (n,o,t) -> n,true,t) args,ret) in
		let e = match e.eexpr with
			| TField (e1,fa) ->
				if field_name fa = "bind" then (match follow e1.etype with
					| TFun(args,ret) -> {e1 with etype = opt_args args ret}
					| _ -> e)
				else if field_name fa = "match" then (match follow e1.etype with
					| TEnum _ as t -> {e1 with etype = tfun [t] ctx.t.tbool }
					| _ -> e)
				else
					e
			| _ ->
				e
		in
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
		let merge_core_doc c =
			let c_core = Typeload.load_core_class ctx c in
			if c.cl_doc = None then c.cl_doc <- c_core.cl_doc;
			let maybe_merge cf_map cf =
				if cf.cf_doc = None then try cf.cf_doc <- (PMap.find cf.cf_name cf_map).cf_doc with Not_found -> ()
			in
			List.iter (maybe_merge c_core.cl_fields) c.cl_ordered_fields;
			List.iter (maybe_merge c_core.cl_statics) c.cl_ordered_statics;
			match c.cl_constructor,c_core.cl_constructor with
				| Some ({cf_doc = None} as cf),Some cf2 -> cf.cf_doc <- cf2.cf_doc
				| _ -> ()
		in
		let rec get_fields t =
			match follow t with
			| TInst (c,params) ->
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc c;
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
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc c;
				ctx.m.module_using <- c :: ctx.m.module_using;
				let fields = try
					let _,el,_ = Meta.get Meta.Forward a.a_meta in
					let sl = ExtList.List.filter_map (fun e -> match fst e with
						| EConst(Ident s) -> Some s
						| _ -> None
					) el in
					let fields = get_fields (apply_params a.a_types pl a.a_this) in
					if sl = [] then fields else PMap.fold (fun cf acc ->
						if List.mem cf.cf_name sl then
							PMap.add cf.cf_name cf acc
						else
							acc
					) fields PMap.empty
				with Not_found ->
					PMap.empty
				in
				PMap.fold (fun f acc ->
					if f.cf_name <> "_new" && can_access ctx c f true && Meta.has Meta.Impl f.cf_meta && not (Meta.has Meta.Enum f.cf_meta) then begin
						let f = prepare_using_field f in
						let t = apply_params a.a_types pl (follow f.cf_type) in
						PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type t } acc
					end else
						acc
				) c.cl_statics fields
			| TAnon a ->
				(match !(a.a_status) with
				| Statics c ->
					if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc c;
					let is_abstract_impl = match c.cl_kind with KAbstractImpl _ -> true | _ -> false in
					let pm = match c.cl_constructor with None -> PMap.empty | Some cf -> PMap.add "new" cf PMap.empty in
					PMap.fold (fun f acc ->
						if can_access ctx c f true && (not is_abstract_impl || not (Meta.has Meta.Impl f.cf_meta) || Meta.has Meta.Enum f.cf_meta) then
							PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type f.cf_type } acc else acc
					) a.a_fields pm
				| _ ->
					a.a_fields)
			| TFun (args,ret) ->
				let t = opt_args args ret in
				let cf = mk_field "bind" (tfun [t] t) p in
				PMap.add "bind" cf PMap.empty
			| TEnum(_) as t ->
				let cf = mk_field "match" (tfun [t] ctx.t.tbool) p in
				PMap.add "match" cf PMap.empty
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
									List.iter (fun tc -> unify_raise ctx m (map tc) e.epos) constr
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
			let e = match follow e.etype with
				| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
					call_to_string ctx c e
				| _ ->
					e
			in
			mk (TCall (mk (TLocal (alloc_var "`trace" t_dynamic)) t_dynamic p,[e;infos])) ctx.t.tvoid p
		else
			let me = Meta.ToString,[],pos e in
			type_expr ctx (ECall ((EField ((EField ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[(EMeta (me,e),pos e);infos]),p) NoValue
	| (EConst(Ident "callback"),p1),args ->
		let ecb = try Some (type_ident_raise ctx "callback" p1 MCall) with Not_found -> None in
		(match ecb with
		| Some ecb ->
			build_call ctx ecb args with_type p
		| None ->
			display_error ctx "callback syntax has changed to func.bind(args)" p;
			let e = type_expr ctx e Value in
			type_bind ctx e args p)
	| (EField ((EConst (Ident "super"),_),_),_), _ ->
		def()
	| (EField (e,"bind"),p), args ->
		let e = type_expr ctx e Value in
		(match follow e.etype with
			| TFun _ -> type_bind ctx e args p
			| _ -> def ())
	| (EConst (Ident "$type"),_) , [e] ->
		let e = type_expr ctx e Value in
		ctx.com.warning (s_type (print_context()) e.etype) e.epos;
		e
	| (EField(e,"match"),p), [epat] ->
		let et = type_expr ctx e Value in
		(match follow et.etype with
			| TEnum _ as t ->
				let e = match_expr ctx e [[epat],None,Some (EConst(Ident "true"),p)] (Some (Some (EConst(Ident "false"),p))) (WithType ctx.t.tbool) p in
				let locals = !get_pattern_locals_ref ctx epat t in
				PMap.iter (fun _ (_,p) -> display_error ctx "Capture variables are not allowed" p) locals;
				Codegen.PatternMatchConversion.to_typed_ast ctx e p
			| _ -> def ())
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
		| (TAnon a) as t -> (match !(a.a_status) with Statics c -> Some (TInst(c,List.map snd c.cl_types),f) | _ -> Some (t,f))
		| _ -> None
	in
	let push_this e =
		match e.eexpr with
			| TConst (TInt _ | TFloat _ | TString _ | TBool _) ->
				(Interp.make_ast e),fun () -> ()
			| _ ->
				ctx.this_stack <- e :: ctx.this_stack;
				let er = EMeta((Meta.This,[],e.epos), (EConst(Ident "this"),e.epos)),e.epos in
				er,fun () -> ctx.this_stack <- List.tl ctx.this_stack
	in
	match acc with
	| AKInline (ethis,f,fmode,t) when Meta.has Meta.Generic f.cf_meta ->
		let el,t,e = type_generic_function ctx (ethis,f) el with_type p in
		make_call ctx e el t p
	| AKInline (ethis,f,fmode,t) ->
		let params, tfunc = (match follow t with
			| TFun (args,r) -> unify_call_params ctx (fopts ethis.etype f) el args r p true
			| _ -> error (s_type (print_context()) t ^ " cannot be called") p
		) in
		make_call ctx (mk (TField (ethis,fmode)) t p) params (match tfunc with TFun(_,r) -> r | _ -> assert false) p
	| AKUsing (et,cl,ef,eparam) when Meta.has Meta.Generic ef.cf_meta ->
		(match et.eexpr with
		| TField(ec,_) ->
			let el,t,e = type_generic_function ctx (ec,ef) el ~using_param:(Some eparam) with_type p in
			make_call ctx e el t p
		| _ -> assert false)
	| AKUsing (et,cl,ef,eparam) ->
		begin match ef.cf_kind with
		| Method MethMacro ->
			let ethis = type_module_type ctx (TClassDecl cl) None p in
			let eparam,f = push_this eparam in
			let e = build_call ctx (AKMacro (ethis,ef)) (eparam :: el) with_type p in
			f();
			e
		| _ ->
			let t = follow (field_type ctx cl [] ef p) in
			(* for abstracts we have to apply their parameters to the static function *)
			let t,tthis,is_abstract_impl_call = match follow eparam.etype with
				| TAbstract(a,tl) when Meta.has Meta.Impl ef.cf_meta -> apply_params a.a_types tl t,apply_params a.a_types tl a.a_this,true
				| te -> t,te,false
			in
			let params,args,r,eparam = match t with
				| TFun ((_,_,t1) :: args,r) ->
					unify ctx tthis t1 eparam.epos;
					let ef = prepare_using_field ef in
					begin match unify_call_params ctx (Some (TInst(cl,[]),ef)) el args r p (ef.cf_kind = Method MethInline) with
					| el,TFun(args,r) -> el,args,r,(if is_abstract_impl_call then eparam else Codegen.Abstract.check_cast ctx t1 eparam eparam.epos)
					| _ -> assert false
					end
				| _ -> assert false
			in
			make_call ctx et (eparam :: params) r p
		end
	| AKMacro (ethis,cf) ->
		if ctx.macro_depth > 300 then error "Stack overflow" p;
		ctx.macro_depth <- ctx.macro_depth + 1;
		ctx.with_type_stack <- with_type :: ctx.with_type_stack;
		let ethis_f = ref (fun () -> ()) in
		let f = (match ethis.eexpr with
		| TTypeExpr (TClassDecl c) ->
			(match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name el p with
			| None -> (fun() -> type_expr ctx (EConst (Ident "null"),p) Value)
			| Some (EMeta((Meta.MergeBlock,_,_),(EBlock el,_)),_) -> (fun () -> let e = type_block ctx el with_type p in mk (TMeta((Meta.MergeBlock,[],p), e)) e.etype e.epos)
			| Some (EVars vl,p) -> (fun() -> type_vars ctx vl p true)
			| Some e -> (fun() -> type_expr ctx (EMeta((Meta.PrivateAccess,[],snd e),e),snd e) with_type))
		| _ ->
			(* member-macro call : since we will make a static call, let's found the actual class and not its subclass *)
			(match follow ethis.etype with
			| TInst (c,_) ->
				let rec loop c =
					if PMap.mem cf.cf_name c.cl_fields then
						let eparam,f = push_this ethis in
						ethis_f := f;
						let e = match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name (eparam :: el) p with
							| None -> (fun() -> type_expr ctx (EConst (Ident "null"),p) Value)
							| Some e -> (fun() -> type_expr ctx e Value)
						in
						e
					else
						match c.cl_super with
						| None -> assert false
						| Some (csup,_) -> loop csup
				in
				loop c
			| _ -> assert false))
		in
		ctx.macro_depth <- ctx.macro_depth - 1;
		ctx.with_type_stack <- List.tl ctx.with_type_stack;
		let old = ctx.on_error in
		ctx.on_error <- (fun ctx msg ep ->
			old ctx msg ep;
			(* display additional info in the case the error is not part of our original call *)
			if ep.pfile <> p.pfile || ep.pmax < p.pmin || ep.pmin > p.pmax then old ctx "Called from macro here" p
		);
		let e = try
			f()
		with Error (m,p) ->
			ctx.on_error <- old;
			!ethis_f();
			raise (Fatal_error ((error_msg m),p))
		in
		ctx.on_error <- old;
		!ethis_f();
		e
	| AKNo _ | AKSet _ | AKAccess _ ->
		ignore(acc_get ctx acc p);
		assert false
	| AKExpr e ->
		let el , t, e = (match follow e.etype with
		| TFun (args,r) ->
			let fopts = (match acc with
				| AKExpr {eexpr = TField(e, (FStatic (_,f) | FInstance(_,f) | FAnon(f)))} ->
					fopts e.etype f
				| _ ->
					None
			) in
			(match fopts,acc with
				| Some (_,cf),AKExpr({eexpr = TField(e,_)}) when Meta.has Meta.Generic cf.cf_meta ->
					type_generic_function ctx (e,cf) el with_type p
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

	and walk_static_field p c cf =
		match cf.cf_expr with
		| None -> ()
		| Some e ->
			if PMap.mem (c.cl_path,cf.cf_name) (!statics) then
				()
			else begin
				statics := PMap.add (c.cl_path,cf.cf_name) () (!statics);
				walk_expr p e;
			end

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
		| TField(e1,FStatic(c,cf)) ->
			walk_expr p e1;
			walk_static_field p c cf;
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
	if ctx.com.display = DMNone then ctx.com.error <- (fun e p -> raise (Error(Custom e,p)));
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
				let tp = match List.rev (fst path) with
					| s :: sl when String.length s > 0 && (match s.[0] with 'A'..'Z' -> true | _ -> false) ->
						{ tpackage = List.rev sl; tname = s; tparams = []; tsub = Some (snd path) }
					| _ ->
						{ tpackage = fst path; tname = snd path; tparams = []; tsub = None }
				in
				try
					let m = Some (Typeload.load_instance ctx tp p true) in
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
		Interp.after_generate = (fun f ->
			Common.add_final_filter ctx.com (fun() ->
				let t = macro_timer ctx "afterGenerate" in
				f();
				t()
			)
		);
		Interp.on_type_not_found = (fun f ->
			ctx.com.load_extern_type <- ctx.com.load_extern_type @ [fun path p ->
				match f (s_type_path path) with
				| Interp.VNull -> None
				| td ->
					let (pack,name),tdef,p = Interp.decode_type_def td in
					Some (name,(pack,[tdef,p]))
			];
		);
		Interp.parse_string = parse_expr_string;
		Interp.type_expr = (fun e ->
			typing_timer ctx (fun() -> (type_expr ctx e Value))
		);
		Interp.get_display = (fun s ->
			let is_displaying = ctx.com.display <> DMNone in
			let old_resume = !Parser.resume_display in
			let old_error = ctx.on_error in
			let restore () =
				if not is_displaying then begin
					ctx.com.defines <- PMap.remove (fst (Define.infos Define.Display)) ctx.com.defines;
					ctx.com.display <- DMNone
				end;
				Parser.resume_display := old_resume;
				ctx.on_error <- old_error;
			in
			(* temporarily enter display mode with a fake position *)
			if not is_displaying then begin
				Common.define ctx.com Define.Display;
				ctx.com.display <- DMDefault;
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
		Interp.allow_package = (fun v -> Common.allow_package ctx.com v);
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
						let e = Interp.decode_texpr v in
						let str = Genjs.gen_single_expr js_ctx e false in
						Interp.enc_string str
					));
					"isKeyword", Interp.VFunction (Interp.Fun1 (fun v ->
						Interp.VBool (Hashtbl.mem Genjs.kwds (Interp.dec_string v))
					));
					"hasFeature", Interp.VFunction (Interp.Fun1 (fun v ->
						Interp.VBool (Common.has_feature ctx.com (Interp.dec_string v))
					));
					"addFeature", Interp.VFunction (Interp.Fun1 (fun v ->
						Common.add_feature ctx.com (Interp.dec_string v);
						Interp.VNull
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
						let e = Interp.decode_texpr v in
						let str = Genjs.gen_single_expr js_ctx e true in
						Interp.enc_string str
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
			| Some (mt,tl,_) ->
				Some (match mt with
					| TClassDecl c -> TInst (c,tl)
					| TEnumDecl e -> TEnum (e,tl)
					| TTypeDecl t -> TType (t,tl)
					| TAbstractDecl a -> TAbstract(a,tl))
			| None ->
				if ctx.curclass == null_class then
					None
				else
					Some (TInst (ctx.curclass,[]))
		);
		Interp.get_expected_type = (fun() ->
			match ctx.with_type_stack with
				| (WithType t | WithTypeResume t) :: _ -> Some t
				| _ -> None
		);
		Interp.get_constructor_arguments = (fun() ->
			match ctx.constructor_argument_stack with
				| [] -> None
				| el :: _ -> Some el
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
			| Some (_,_,fields) -> Interp.enc_array (List.map Interp.encode_field fields)
		);
		Interp.get_pattern_locals = (fun e t ->
			!get_pattern_locals_ref ctx e t
		);
		Interp.define_type = (fun v ->
			let m, tdef, pos = (try Interp.decode_type_def v with Interp.Invalid_expr -> Interp.exc (Interp.VString "Invalid type definition")) in
			let add ctx =
				let prev = (try Some (Hashtbl.find ctx.g.modules m) with Not_found -> None) in
				let mnew = Typeload.type_module ctx m ctx.m.curmod.m_extra.m_file [tdef,pos] [] [] pos in
				add_dependency mnew ctx.m.curmod;
				(* if we defined a type in an existing module, let's move the types here *)
				(match prev with
				| None ->
					mnew.m_extra.m_kind <- MFake;
				| Some mold ->
					Hashtbl.replace ctx.g.modules mnew.m_path mold;
					mold.m_types <- mold.m_types @ mnew.m_types;
					mnew.m_extra.m_kind <- MSub;
					add_dependency mold mnew;
				);
			in
			add ctx;
			(* if we are adding a class which has a macro field, we also have to add it to the macro context (issue #1497) *)
			if not ctx.in_macro then match tdef,ctx.g.macros with
			| EClass c,Some (_,mctx) when List.exists (fun cff -> (Meta.has Meta.Macro cff.cff_meta || List.mem AMacro cff.cff_access)) c.d_data ->
				add mctx
			| _ ->
				()
		);
		Interp.define_module = (fun m types imports usings ->
			let types = List.map (fun v ->
				let _, tdef, pos = (try Interp.decode_type_def v with Interp.Invalid_expr -> Interp.exc (Interp.VString "Invalid type definition")) in
				tdef, pos
			) types in
			let m = Ast.parse_path m in
			let pos = (match types with [] -> Ast.null_pos | (_,p) :: _ -> p) in
			let prev = (try Some (Hashtbl.find ctx.g.modules m) with Not_found -> None) in
			let mnew = Typeload.type_module ctx m ctx.m.curmod.m_extra.m_file types imports usings pos in
			add_dependency mnew ctx.m.curmod;
			(* if we defined a type in an existing module, let's move the types here *)
			(match prev with
			| None ->
				mnew.m_extra.m_kind <- MFake;
			| Some mold ->
				Hashtbl.replace ctx.g.modules mnew.m_path mold;
				mold.m_types <- mold.m_types @ mnew.m_types;
				mnew.m_extra.m_kind <- MSub;
				add_dependency mold mnew;
			);
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
		Interp.use_cache = (fun() ->
			!macro_enable_cache
		);
		Interp.format_string = (fun s p ->
			format_string ctx s p
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
	let mint = if not (Interp.can_reuse mint types) then begin
		let com2 = mctx.com in
		let mint = Interp.create com2 (make_macro_api ctx Ast.null_pos) in
		let macro = ((fun() -> Interp.select mint), mctx) in
		ctx.g.macros <- Some macro;
		mctx.g.macros <- Some macro;
		init_macro_interp ctx mctx mint;
		mint
	end else mint in
	(* we should maybe ensure that all filters in Main are applied. Not urgent atm *)
	let expr_filters = [Codegen.Abstract.handle_abstract_casts mctx; Filters.captured_vars mctx.com; Filters.rename_local_vars mctx.com] in
	let type_filters = [Filters.add_field_inits mctx] in
	let ready = fun t ->
		Filters.post_process mctx expr_filters t;
		List.iter (fun f -> f t) type_filters
	in
	(try Interp.add_types mint types ready
	with Error (e,p) -> raise (Fatal_error(error_msg e,p)));
	Filters.post_process_end()

let create_macro_interp ctx mctx =
	let com2 = mctx.com in
	let mint, init = (match !macro_interp_cache with
		| None ->
			let mint = Interp.create com2 (make_macro_api ctx Ast.null_pos) in
			mint, (fun() -> init_macro_interp ctx mctx mint)
		| Some mint ->
			Interp.do_reuse mint;
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
		com2.display <- DMNone;
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
	let cpath, sub = (match List.rev (fst cpath) with
		| name :: pack when name.[0] >= 'A' && name.[0] <= 'Z' -> (List.rev pack,name), Some (snd cpath)
		| _ -> cpath, None
	) in
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
	let cl, meth = (match Typeload.load_instance mctx { tpackage = fst cpath; tname = snd cpath; tparams = []; tsub = sub } p true with
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
		let r = Interp.call_path (Interp.get_ctx()) ((fst cpath) @ [(match sub with None -> snd cpath | Some s -> s)]) f args api in
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
		try
			unify_raise mctx mret ttype mpos;
			(* TODO: enable this again in the future *)
			(* ctx.com.warning "Returning Type from @:genericBuild macros is deprecated, consider returning ComplexType instead" p; *)
		with Error (Unify _,_) ->
			let cttype = { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = Some ("ComplexType") } in
			let ttype = Typeload.load_instance mctx cttype p false in
			unify_raise mctx mret ttype mpos;
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
							| Some (_,_,fields) -> fields)
						| _ ->
							List.map Interp.decode_field (Interp.dec_array v)
					) in
					(EVars ["fields",Some (CTAnonymous fields),None],p)
				| MMacroType ->
					let t = if v = Interp.VNull then
						mk_mono()
					else try
						let ct = Interp.decode_ctype v in
						Typeload.load_complex_type ctx p ct;
					with Interp.Invalid_expr ->
						Interp.decode_type v
					in
					ctx.ret <- t;
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
	let e = try
		parse_expr_string ctx e p false
	with err ->
		display_error ctx ("Could not parse `" ^ e ^ "`") p;
		raise err
	in
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
			doinline = not (Common.defined com Define.NoInline || com.display <> DMNone);
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
		this_stack = [];
		with_type_stack = [];
		constructor_argument_stack = [];
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
	(* We always want core types to be available so we add them as default imports (issue #1904 and #3131). *)
	ctx.m.module_types <- ctx.g.std.m_types;
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
			()
		| TClassDecl c ->
			()
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
	(try
		List.iter (fun t -> (
			match t with
			| TClassDecl ({cl_path = ([],"Array")} as c) ->
				ctx.t.tarray <- (fun t -> TInst (c,[t]));
				raise Exit
			| _ -> ()
		)) m.m_types;
		assert false
	with Exit -> ());
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
get_constructor_ref := get_constructor;
check_abstract_cast_ref := Codegen.Abstract.check_cast;
type_module_type_ref := type_module_type;
