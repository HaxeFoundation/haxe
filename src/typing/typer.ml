(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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
open Common.DisplayMode
open Common
open Type
open Typecore
open Error

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

type switch_mode =
	| CMatch of (tenum_field * (string * t) option list option * pos)
	| CExpr of texpr

type access_mode =
	| MGet
	| MSet
	| MCall

type access_kind =
	| AKNo of string
	| AKExpr of texpr
	| AKSet of texpr * t * tclass_field
	| AKInline of texpr * tclass_field * tfield_access * t
	| AKMacro of texpr * tclass_field
	| AKUsing of texpr * tclass * tclass_field * texpr
	| AKAccess of tabstract * tparams * tclass * texpr * texpr

type object_decl_kind =
	| ODKWithStructure of tanon
	| ODKWithClass of tclass * tparams
	| ODKPlain

let build_call_ref : (typer -> access_kind -> expr list -> with_type -> pos -> texpr) ref = ref (fun _ _ _ _ _ -> assert false)

let mk_infos ctx p params =
	let file = if ctx.in_macro then p.pfile else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path p.pfile else Filename.basename p.pfile in
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
	| TLocal {v_extra = None} | TArray _ | TField _ ->
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
	| TAbstract (a,[]) when List.exists (fun t -> match classify t with KInt | KFloat -> true | _ -> false) a.a_to -> KParam t
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

let get_abstract_froms a pl =
	let l = List.map (apply_params a.a_params pl) a.a_from in
	List.fold_left (fun acc (t,f) ->
		match follow (field_type f) with
		| TFun ([_,_,v],t) ->
			(try
				ignore(type_eq EqStrict t (TAbstract(a,List.map dup pl))); (* unify fields monomorphs *)
				v :: acc
			with Unify_error _ ->
				acc)
		| _ ->
			acc
	) l a.a_from_field

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
		is_pos_infos (apply_params t.t_params tl t.t_type)
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
				delay ctx PCheckConstraint (fun () -> try f() with Unify_error l -> display_error ctx (error_msg (Unify l)) p)
			else
				f()
		| _ ->
			()
	) tl tpl

let enum_field_type ctx en ef tl_en tl_ef p =
	let map t = apply_params en.e_params tl_en (apply_params ef.ef_params tl_ef t) in
	begin try
		check_constraints ctx (s_type_path en.e_path) en.e_params tl_en map true p;
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
		if not (Meta.has Meta.Generic f.cf_meta) then add_constraint_checks ctx c.cl_params pl f monos p;
		apply_params l monos f.cf_type

let class_field ctx c tl name p =
	raw_class_field (fun f -> field_type ctx c tl f p) c tl name

(* checks if we can access to a given class field using current context *)
let rec can_access ctx ?(in_overload=false) c cf stat =
	if cf.cf_public then
		true
	else if not in_overload && ctx.com.config.pf_overload && Meta.has Meta.Overload cf.cf_meta then
		true
	else
	(* TODO: should we add a c == ctx.curclass short check here? *)
	(* has metadata path *)
	let rec make_path c f = match c.cl_kind with
		| KAbstractImpl a -> fst a.a_path @ [snd a.a_path; f.cf_name]
		| KGenericInstance(c,_) -> make_path c f
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
			| (m2,el,_) :: l when m = m2 ->
				List.exists (fun e ->
					let p = expr_path [] e in
					(p <> [] && chk_path p path)
				) el
				|| loop l
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
let prepare_using_field cf = match follow cf.cf_type with
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

let parse_string com s p inlined =
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
	Lexer.init p.pfile true;
	Parser.display_error := (fun e p -> raise (Parser.Error (e,p)));
	if not inlined then Parser.resume_display := null_pos;
	let pack, decls = try
		Parser.parse com (Lexing.from_string s)
	with Parser.Error (e,pe) ->
		restore();
		error (Parser.error_msg e) (if inlined then pe else p)
	| Lexer.Error (e,pe) ->
		restore();
		error (Lexer.error_msg e) (if inlined then pe else p)
	in
	restore();
	pack,decls

let eval ctx s =
	let p = { pfile = "--eval"; pmin = 0; pmax = String.length s; } in
	let pack,decls = parse_string ctx.com s p false in
	let rec find_main current decls = match decls with
		| (EClass c,_) :: decls ->
			let path = pack,fst c.d_name in
			begin try
				let cff = List.find (fun cff -> fst cff.cff_name = "main") c.d_data in
				if ctx.com.main_class <> None then error "Multiple main" cff.cff_pos;
				ctx.com.main_class <- Some path;
				Some path
			with Not_found ->
				find_main (if current = None then Some path else current) decls
			end
		| ((EEnum {d_name = (s,_)} | ETypedef {d_name = (s,_)} | EAbstract {d_name = (s,_)}),_) :: decls when current = None ->
			find_main (Some (pack,s)) decls
		| _ :: decls ->
			find_main current decls
		| [] ->
			current
	in
	let path_module = match find_main None decls with
		| None -> error "Evaluated string did not define any types" p
		| Some path -> path
	in
	ignore(Typeload.type_module ctx path_module "eval" decls p);
	flush_pass ctx PBuildClass "eval"

let parse_expr_string ctx s p inl =
	let head = "class X{static function main() " in
	let head = (if p.pmin > String.length head then head ^ String.make (p.pmin - String.length head) ' ' else head) in
	let rec loop e = let e = Ast.map_expr loop e in (fst e,p) in
	match parse_string ctx.com (head ^ s ^ ";}") p inl with
	| _,[EClass { d_data = [{ cff_name = "main",null_pos; cff_kind = FFun { f_expr = Some e } }]},_] -> if inl then e else loop e
	| _ -> raise Interp.Invalid_expr

let merge_core_doc ctx c =
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

let check_error ctx err p = match err with
	| Module_not_found ([],name) when Display.Diagnostics.is_diagnostics_run ctx ->
		Display.ToplevelCollector.handle_unresolved_identifier ctx name p true
	| _ ->
		display_error ctx (error_msg err) p

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let rec unify_min_raise ctx (el:texpr list) : t =
	let rec base_types t =
		let tl = ref [] in
		let rec loop t = (match t with
			| TInst(cl, params) ->
				(match cl.cl_kind with
				| KTypeParameter tl -> List.iter loop tl
				| _ -> ());
				List.iter (fun (ic, ip) ->
					let t = apply_params cl.cl_params params (TInst (ic,ip)) in
					loop t
				) cl.cl_implements;
				(match cl.cl_super with None -> () | Some (csup, pl) ->
					let t = apply_params cl.cl_params params (TInst (csup,pl)) in
					loop t);
				tl := t :: !tl;
			| TEnum(en,(_ :: _ as tl2)) ->
				tl := (TEnum(en,List.map (fun _ -> t_dynamic) tl2)) :: !tl;
				tl := t :: !tl;
			| TType (td,pl) ->
				loop (apply_params td.t_params pl td.t_type);
				(* prioritize the most generic definition *)
				tl := t :: !tl;
			| TLazy f -> loop (!f())
			| TMono r -> (match !r with None -> () | Some t -> loop t)
			| _ -> tl := t :: !tl)
		in
		loop t;
		!tl
	in
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
				PMap.add n (mk_field n t (List.hd el).epos null_pos) acc
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

let rec unify_call_args' ctx el args r callp inline force_inline =
	let in_call_args = ctx.in_call_args in
	ctx.in_call_args <- true;
	let call_error err p =
		raise (Error (Call_error err,p))
	in
	let arg_error ul name opt p =
		let err = Stack (ul,Custom ("For " ^ (if opt then "optional " else "") ^ "function argument '" ^ name ^ "'")) in
		call_error (Could_not_unify err) p
	in
	let mk_pos_infos t =
		let infos = mk_infos ctx callp [] in
		type_expr ctx infos (WithType t)
	in
	let rec default_value name t =
		if is_pos_infos t then
			mk_pos_infos t
		else
			null (ctx.t.tnull t) callp
	in
	let skipped = ref [] in
	let invalid_skips = ref [] in
	let skip name ul t p =
		if not ctx.com.config.pf_can_skip_non_nullable_argument && not (is_nullable t) then
			invalid_skips := name :: !invalid_skips;
		skipped := (name,ul,p) :: !skipped;
		default_value name t
	in
	(* let force_inline, is_extern = match cf with Some(TInst(c,_),f) -> is_forced_inline (Some c) f, c.cl_extern | _ -> false, false in *)
	let type_against t e =
		try
			let e = type_expr ctx e (WithType t) in
			AbstractCast.cast_or_unify_raise ctx t e e.epos
		with Error(l,p) when (match l with Call_error _ -> false | _ -> true) ->
			raise (WithTypeError (l,p))
	in
	let rec loop el args = match el,args with
		| [],[] ->
			begin match List.rev !invalid_skips with
				| [] -> ()
				| name :: _ -> call_error (Cannot_skip_non_nullable name) callp;
			end;
			[]
		| _,[name,false,t] when (match follow t with TAbstract({a_path = ["haxe";"extern"],"Rest"},_) -> true | _ -> false) ->
			begin match follow t with
				| TAbstract({a_path=(["haxe";"extern"],"Rest")},[t]) ->
					(try List.map (fun e -> type_against t e,false) el with WithTypeError(ul,p) -> arg_error ul name false p)
				| _ ->
					assert false
			end
		| [],(_,false,_) :: _ ->
			call_error (Not_enough_arguments args) callp
		| [],(name,true,t) :: args ->
			begin match loop [] args with
				| [] when not (inline && (ctx.g.doinline || force_inline)) && not ctx.com.config.pf_pad_nulls ->
					if is_pos_infos t then [mk_pos_infos t,true]
					else []
				| args ->
					let e_def = default_value name t in
					(e_def,true) :: args
			end
		| (_,p) :: _, [] ->
			begin match List.rev !skipped with
				| [] -> call_error Too_many_arguments p
				| (s,ul,p) :: _ -> arg_error ul s true p
			end
		| e :: el,(name,opt,t) :: args ->
			begin try
				let e = type_against t e in
				(e,opt) :: loop el args
			with
				WithTypeError (ul,p)->
					if opt then
						let e_def = skip name ul t p in
						(e_def,true) :: loop (e :: el) args
					else
						arg_error ul name false p
			end
	in
	let el = try loop el args with exc -> ctx.in_call_args <- in_call_args; raise exc; in
	ctx.in_call_args <- in_call_args;
	el,TFun(args,r)

let unify_call_args ctx el args r p inline force_inline =
	let el,tf = unify_call_args' ctx el args r p inline force_inline in
	List.map fst el,tf

let unify_field_call ctx fa el args ret p inline =
	let map_cf cf0 map cf =
		let t = map (monomorphs cf.cf_params cf.cf_type) in
		begin match cf.cf_expr,cf.cf_kind with
		| None,Method MethInline when not ctx.com.config.pf_overload ->
			(* This is really awkward and shouldn't be here. We'll keep it for
			   3.2 in order to not break code that relied on the quirky behavior
			   in 3.1.3, but it should really be reviewed afterwards.
			   Related issue: https://github.com/HaxeFoundation/haxe/issues/3846
			*)
			cf.cf_expr <- cf0.cf_expr;
			cf.cf_kind <- cf0.cf_kind;
		| _ ->
			()
		end;
		t,cf
	in
	let expand_overloads map cf =
		(TFun(args,ret),cf) :: (List.map (map_cf cf map) cf.cf_overloads)
	in
	let candidates,co,cf,mk_fa = match fa with
		| FStatic(c,cf) ->
			expand_overloads (fun t -> t) cf,Some c,cf,(fun cf -> FStatic(c,cf))
		| FAnon cf ->
			expand_overloads (fun t -> t) cf,None,cf,(fun cf -> FAnon cf)
		| FInstance(c,tl,cf) ->
			let map = apply_params c.cl_params tl in
			let cfl = if cf.cf_name = "new" || not (Meta.has Meta.Overload cf.cf_meta && ctx.com.config.pf_overload) then
				List.map (map_cf cf map) cf.cf_overloads
			else
				List.map (fun (t,cf) -> map (monomorphs cf.cf_params t),cf) (Overloads.get_overloads c cf.cf_name)
			in
			(TFun(args,ret),cf) :: cfl,Some c,cf,(fun cf -> FInstance(c,tl,cf))
		| FClosure(co,cf) ->
			let c = match co with None -> None | Some (c,_) -> Some c in
			expand_overloads (fun t -> t) cf,c,cf,(fun cf -> match co with None -> FAnon cf | Some (c,tl) -> FInstance(c,tl,cf))
		| _ ->
			error "Invalid field call" p
	in
	let is_forced_inline = is_forced_inline co cf in
	let is_overload = Meta.has Meta.Overload cf.cf_meta in
	let attempt_call t cf = match follow t with
		| TFun(args,ret) ->
			let el,tf = unify_call_args' ctx el args ret p inline is_forced_inline in
			let mk_call ethis p_field =
				let ef = mk (TField(ethis,mk_fa cf)) tf p_field in
				make_call ctx ef (List.map fst el) ret p
			in
			el,tf,mk_call
		| _ ->
			assert false
	in
	let rec loop candidates = match candidates with
		| [] -> [],[]
		| (t,cf) :: candidates ->
			begin try
				let candidate = attempt_call t cf in
				if ctx.com.config.pf_overload && is_overload then begin
					let candidates,failures = loop candidates in
					candidate :: candidates,failures
				end else
					[candidate],[]
			with Error ((Call_error _ as err),p) ->
				let candidates,failures = loop candidates in
				candidates,(cf,err,p) :: failures
			end
	in
	let fail_fun () =
		let tf = TFun(args,ret) in
		[],tf,(fun ethis p_field ->
			let e1 = mk (TField(ethis,mk_fa cf)) tf p_field in
			mk (TCall(e1,[])) ret p)
	in
	match candidates with
	| [t,cf] ->
		begin try
			let el,tf,mk_call = attempt_call t cf in
			List.map fst el,tf,mk_call
		with Error _ when ctx.com.display.dms_error_policy = EPIgnore ->
			fail_fun();
		end
	| _ ->
		let candidates,failures = loop candidates in
		let fail () =
			let failures = List.map (fun (cf,err,p) -> cf,error_msg err,p) failures in
			let failures = remove_duplicates (fun (_,msg1,_) (_,msg2,_) -> msg1 <> msg2) failures in
			begin match failures with
			| [_,msg,p] ->
				error msg p
			| _ ->
				display_error ctx "Could not find a suitable overload, reasons follow" p;
				List.iter (fun (cf,msg,p2) ->
					display_error ctx ("Overload resolution failed for " ^ (s_type (print_context()) cf.cf_type)) p;
					display_error ctx msg p2;
				) failures;
				error "End of overload failure reasons" p
			end
		in
		if is_overload && ctx.com.config.pf_overload then begin match Overloads.Resolution.reduce_compatible candidates with
			| [] -> fail()
			| [el,tf,mk_call] -> List.map fst el,tf,mk_call
			| _ -> error "Ambiguous overload" p
		end else begin match List.rev candidates with
			| [] -> fail()
			| (el,tf,mk_call) :: _ -> List.map fst el,tf,mk_call
		end

let fast_enum_field e ef p =
	let et = mk (TTypeExpr (TEnumDecl e)) (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics e) }) p in
	TField (et,FEnum (e,ef))

let abstract_module_type a tl = {
	t_path = [],Printf.sprintf "Abstract<%s%s>" (s_type_path a.a_path) (s_type_params (ref []) tl);
	t_module = a.a_module;
	t_doc = None;
	t_pos = a.a_pos;
	t_name_pos = null_pos;
	t_type = TAnon {
		a_fields = PMap.empty;
		a_status = ref (AbstractStatics a);
	};
	t_private = true;
	t_params = [];
	t_meta = no_meta;
}

let rec type_module_type ctx t tparams p =
	match t with
	| TClassDecl {cl_kind = KGenericBuild _} ->
		let _,_,f = Typeload.build_instance ctx t p in
		let t = f (match tparams with None -> [] | Some tl -> tl) in
		let mt = try
			module_type_of_type t
		with Exit ->
			if follow t == t_dynamic then Typeload.load_type_def ctx p { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None }
			else error "Invalid module type" p
		in
		type_module_type ctx mt None p
	| TClassDecl c ->
		let t_tmp = {
			t_path = [],"Class<" ^ (s_type_path c.cl_path) ^ ">" ;
			t_module = c.cl_module;
			t_doc = None;
			t_pos = c.cl_pos;
			t_name_pos = null_pos;
			t_type = TAnon {
				a_fields = c.cl_statics;
				a_status = ref (Statics c);
			};
			t_private = true;
			t_params = [];
			t_meta = no_meta;
		} in
		mk (TTypeExpr (TClassDecl c)) (TType (t_tmp,[])) p
	| TEnumDecl e ->
		let types = (match tparams with None -> List.map (fun _ -> mk_mono()) e.e_params | Some l -> l) in
		mk (TTypeExpr (TEnumDecl e)) (TType (e.e_type,types)) p
	| TTypeDecl s ->
		let t = apply_params s.t_params (List.map (fun _ -> mk_mono()) s.t_params) s.t_type in
		if not (Common.defined ctx.com Define.NoDeprecationWarnings) then
			Display.DeprecationCheck.check_typedef ctx.com s p;
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
		let t_tmp = abstract_module_type a [] in
		mk (TTypeExpr (TAbstractDecl a)) (TType (t_tmp,[])) p

let type_type ctx tpath p =
	type_module_type ctx (Typeload.load_type_def ctx p { tpackage = fst tpath; tname = snd tpath; tparams = []; tsub = None }) None p

let get_constructor ctx c params p =
	match c.cl_kind with
	| KAbstractImpl a ->
		let f = (try PMap.find "_new" c.cl_statics with Not_found -> raise_error (No_constructor (TAbstractDecl a)) p) in
		let ct = field_type ctx c params f p in
		apply_params a.a_params params ct, f
	| _ ->
		let ct, f = (try Type.get_constructor (fun f -> field_type ctx c params f p) c with Not_found -> raise_error (No_constructor (TClassDecl c)) p) in
		apply_params c.cl_params params ct, f

let make_call ctx e params t p =
	try
		let ethis,cl,f = match e.eexpr with
			| TField (ethis,fa) ->
				let co,cf = match fa with
					| FInstance(c,_,cf) | FStatic(c,cf) -> Some c,cf
					| FAnon cf -> None,cf
					| _ -> raise Exit
				in
				ethis,co,cf
			| _ ->
				raise Exit
		in
		if f.cf_kind <> Method MethInline then raise Exit;
		let config = match cl with
			| Some ({cl_kind = KAbstractImpl _}) when Meta.has Meta.Impl f.cf_meta ->
				let t = if f.cf_name = "_new" then
					t
				else if params = [] then
					error "Invalid abstract implementation function" f.cf_pos
				else
					follow (List.hd params).etype
				in
				begin match t with
					| TAbstract(a,pl) ->
						let has_params = a.a_params <> [] || f.cf_params <> [] in
						let monos = List.map (fun _ -> mk_mono()) f.cf_params in
						let map_type = fun t -> apply_params a.a_params pl (apply_params f.cf_params monos t) in
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
		(match f.cf_expr_unoptimized,f.cf_expr with
		| Some fd,_
		| None,Some { eexpr = TFunction fd } ->
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

let mk_array_get_call ctx (cf,tf,r,e1,e2o) c ebase p = match cf.cf_expr with
	| None ->
		if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx "Recursive array get method" p;
		mk (TArray(ebase,e1)) r p
	| Some _ ->
		let et = type_module_type ctx (TClassDecl c) None p in
		let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
		make_call ctx ef [ebase;e1] r p

let mk_array_set_call ctx (cf,tf,r,e1,e2o) c ebase p =
	let evalue = match e2o with None -> assert false | Some e -> e in
	match cf.cf_expr with
		| None ->
			if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx "Recursive array set method" p;
			let ea = mk (TArray(ebase,e1)) r p in
			mk (TBinop(OpAssign,ea,evalue)) r p
		| Some _ ->
			let et = type_module_type ctx (TClassDecl c) None p in
			let ef = mk (TField(et,(FStatic(c,cf)))) tf p in
			make_call ctx ef [ebase;e1;evalue] r p

let rec acc_get ctx g p =
	match g with
	| AKNo f -> error ("Field " ^ f ^ " cannot be accessed for reading") p
	| AKExpr e -> e
	| AKSet _ | AKAccess _ -> assert false
	| AKUsing (et,c,cf,e) when ctx.in_display ->
		(* Generate a TField node so we can easily match it for position/usage completion (issue #1968) *)
		let ec = type_module_type ctx (TClassDecl c) None p in
		let t = match follow et.etype with
			| TFun (_ :: args,ret) -> TFun(args,ret)
			| _ -> et.etype
		in
		mk (TField(ec,FStatic(c,cf))) t et.epos
	| AKUsing (et,_,cf,e) ->
		(* build a closure with first parameter applied *)
		(match follow et.etype with
		| TFun (_ :: args,ret) ->
			let tcallb = TFun (args,ret) in
			let twrap = TFun ([("_e",false,e.etype)],tcallb) in
			(* arguments might not have names in case of variable fields of function types, so we generate one (issue #2495) *)
			let args = List.map (fun (n,o,t) ->
				let t = if o then ctx.t.tnull t else t in
				o,if n = "" then gen_local ctx t e.epos else alloc_var n t e.epos (* TODO: var pos *)
			) args in
			let ve = alloc_var "_e" e.etype e.epos in
			let ecall = make_call ctx et (List.map (fun v -> mk (TLocal v) v.v_type p) (ve :: List.map snd args)) ret p in
			let ecallb = mk (TFunction {
				tf_args = List.map (fun (o,v) -> v,if o then Some TNull else None) args;
				tf_type = ret;
				tf_expr = (match follow ret with | TAbstract ({a_path = [],"Void"},_) -> ecall | _ -> mk (TReturn (Some ecall)) t_dynamic p);
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
		let cmode = (match fmode with FStatic _ -> fmode | FInstance (c,tl,f) -> FClosure (Some (c,tl),f) | _ -> assert false) in
		ignore(follow f.cf_type); (* force computing *)
		(match f.cf_expr with
		| None when ctx.com.display.dms_display ->
			mk (TField (e,cmode)) t p
		| None ->
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
						let c2 = mk_class c.cl_module mpath c.cl_pos null_pos in
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
						| Statics {cl_extern = false} when Meta.has Meta.Extern f.cf_meta ->
							display_error ctx "Cannot create closure on @:extern inline method" p;
							e_def
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
					add_local ctx "`this" ctx.tthis p
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
			let ethis = {ethis with etype = TAbstract(a,List.map snd a.a_params)} in
			AKUsing(e,ctx.curclass,f,ethis)
		| _ ->
			(match m, mode with
			| MethInline, _ -> AKInline (e,f,fmode,t)
			| MethMacro, MGet -> display_error ctx "Macro functions must be called immediately" p; normal()
			| MethMacro, MCall -> AKMacro (e,f)
			| _ , MGet ->
				let cmode = (match fmode with
					| FInstance(_, _, cf) | FStatic(_, cf) when Meta.has Meta.Generic cf.cf_meta -> display_error ctx "Cannot create closure on generic function" p; fmode
					| FInstance (c,tl,cf) -> FClosure (Some (c,tl),cf)
					| FStatic _ | FEnum _ -> fmode
					| FAnon f -> FClosure (None, f)
					| FDynamic _ | FClosure _ -> assert false
				) in
				AKExpr (mk (TField (e,cmode)) t p)
			| _ -> normal())
		end
	| Var v ->
		match (match mode with MGet | MCall -> v.v_read | MSet -> v.v_write) with
		| AccNo when not (Meta.has Meta.PrivateAccess ctx.meta) ->
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
		| AccNormal | AccNo ->
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
			let is_abstract_this_access () = match e.eexpr,ctx.curfun with
				| TTypeExpr (TClassDecl ({cl_kind = KAbstractImpl _} as c)),(FunMemberAbstract | FunMemberAbstractLocal) ->
					c == ctx.curclass
				| _ ->
					false
			in
			if m = ctx.curfield.cf_name && (match e.eexpr with TConst TThis -> true | TTypeExpr (TClassDecl c) when c == ctx.curclass -> true | _ -> false) then
				let prefix = (match ctx.com.platform with Flash when Common.defined ctx.com Define.As3 -> "$" | _ -> "") in
				if is_extern_field f then begin
					display_error ctx "This field cannot be accessed because it is not a real variable" p;
					display_error ctx "Add @:isVar here to enable it" f.cf_pos;
				end;
				AKExpr (mk (TField (e,if prefix = "" then fmode else FDynamic (prefix ^ f.cf_name))) t p)
			else if is_abstract_this_access() then begin
				let this = get_this ctx p in
				if mode = MSet then begin
					let c,a = match ctx.curclass with {cl_kind = KAbstractImpl a} as c -> c,a | _ -> assert false in
					let f = PMap.find m c.cl_statics in
					(* we don't have access to the type parameters here, right? *)
					(* let t = apply_params a.a_params pl (field_type ctx c [] f p) in *)
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
	| (c,pc) :: l ->
		try
			let cf = PMap.find i c.cl_statics in
			if Meta.has Meta.NoUsing cf.cf_meta || not (can_access ctx c cf true) then raise Not_found;
			let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
			let map = apply_params cf.cf_params monos in
			let t = map cf.cf_type in
			begin match follow t with
				| TFun((_,_,(TType({t_path = ["haxe";"macro"],"ExprOf"},[t0]) | t0)) :: args,r) ->
					if is_dynamic && follow t0 != t_dynamic then raise Not_found;
					let e = AbstractCast.cast_or_unify_raise ctx t0 e p in
					(* early constraints check is possible because e.etype has no monomorphs *)
					List.iter2 (fun m (name,t) -> match follow t with
						| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] && not (has_mono m) ->
							List.iter (fun tc -> Type.unify m (map tc)) constr
						| _ -> ()
					) monos cf.cf_params;
					let et = type_module_type ctx (TClassDecl c) None p in
					Display.ImportHandling.maybe_mark_import_position ctx pc;
					AKUsing (mk (TField (et,FStatic (c,cf))) t p,c,cf,e)
				| _ ->
					raise Not_found
			end
		with Not_found ->
			loop l
		| Unify_error el | Error (Unify el,_) ->
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

let rec type_ident_raise ctx i p mode =
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
			| Some ({ eexpr = TFunction f } as e) when ctx.com.display.dms_full_typing ->
				begin match mode with
					| MSet -> error "Cannot set inline closure" p
					| MGet -> error "Cannot create closure on inline closure" p
					| MCall ->
						(* create a fake class with a fake field to emulate inlining *)
						let c = mk_class ctx.m.curmod (["local"],v.v_name) e.epos null_pos in
						let cf = { (mk_field v.v_name v.v_type e.epos null_pos) with cf_params = params; cf_expr = Some e; cf_kind = Method MethInline } in
						c.cl_extern <- true;
						c.cl_fields <- PMap.add cf.cf_name cf PMap.empty;
						AKInline (mk (TConst TNull) (TInst (c,[])) p, cf, FInstance(c,[],cf), t)
				end
			| _ ->
				AKExpr (mk (TLocal v) t p))
		| _ ->
			AKExpr (mk (TLocal v) v.v_type p))
	with Not_found -> try
		(* member variable lookup *)
		if ctx.curfun = FunStatic then raise Not_found;
		let c , t , f = class_field ctx ctx.curclass (List.map snd ctx.curclass.cl_params) i p in
		field_access ctx mode f (match c with None -> FAnon f | Some (c,tl) -> FInstance (c,tl,f)) t (get_this ctx p) p
	with Not_found -> try
		(* lookup using on 'this' *)
		if ctx.curfun = FunStatic then raise Not_found;
		(match using_field ctx mode (mk (TConst TThis) ctx.tthis p) i p with
		| AKUsing (et,c,f,_) -> AKUsing (et,c,f,get_this ctx p)
		| _ -> assert false)
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		if Meta.has Meta.Impl f.cf_meta && not (Meta.has Meta.Impl ctx.curfield.cf_meta) && not (Meta.has Meta.Enum f.cf_meta) then
			error (Printf.sprintf "Cannot access non-static field %s from static method" f.cf_name) p;
		let e = type_type ctx ctx.curclass.cl_path p in
		(* check_locals_masking already done in type_type *)
		field_access ctx mode f (FStatic (ctx.curclass,f)) (field_type ctx ctx.curclass [] f p) e p
	with Not_found -> try
		let wrap e = if mode = MSet then
				AKNo i
			else
				AKExpr e
		in
		(* lookup imported enums *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| (t,pt) :: l ->
				match t with
				| TAbstractDecl ({a_impl = Some c} as a) when Meta.has Meta.Enum a.a_meta ->
					begin try
						let cf = PMap.find i c.cl_statics in
						if not (Meta.has Meta.Enum cf.cf_meta) then
							loop l
						else begin
							let et = type_module_type ctx (TClassDecl c) None p in
							let fa = FStatic(c,cf) in
							let t = monomorphs cf.cf_params cf.cf_type in
							Display.ImportHandling.maybe_mark_import_position ctx pt;
							begin match cf.cf_kind with
								| Var {v_read = AccInline} -> AKInline(et,cf,fa,t)
								| _ -> AKExpr (mk (TField(et,fa)) t p)
							end
						end
					with Not_found ->
						loop l
					end
				| TClassDecl _ | TAbstractDecl _ ->
					loop l
				| TTypeDecl t ->
					(match follow t.t_type with
					| TEnum (e,_) -> loop ((TEnumDecl e,pt) :: l)
					| _ -> loop l)
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						let et = type_module_type ctx t None p in
						let monos = List.map (fun _ -> mk_mono()) e.e_params in
						let monos2 = List.map (fun _ -> mk_mono()) ef.ef_params in
						Display.ImportHandling.maybe_mark_import_position ctx pt;
						wrap (mk (TField (et,FEnum (e,ef))) (enum_field_type ctx e ef monos monos2 p) p)
					with
						Not_found -> loop l
		in
		(try loop (List.rev_map (fun t -> t,null_pos) ctx.m.curmod.m_types) with Not_found -> loop ctx.m.module_types)
	with Not_found ->
		(* lookup imported globals *)
		let t, name, pi = PMap.find i ctx.m.module_globals in
		Display.ImportHandling.maybe_mark_import_position ctx pi;
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
				display_error ctx (StringError.string_error i (string_source t) (s_type (print_context()) t ^ " has no field " ^ i)) p;
		end;
		AKExpr (mk (TField (e,FDynamic i)) (mk_mono()) p)
	in
	let does_forward a stat =
		try
			let _,el,_ = Meta.get (if stat then Meta.ForwardStatics else Meta.Forward) a.a_meta in
			match el with
				| [] ->
					true
				| _ ->
					List.exists (fun e -> match fst e with
						| EConst(Ident s | String s) -> s = i
						| _ -> error "Identifier or string expected as argument to @:forward" (pos e)
					) el
		with Not_found ->
			false
	in
	match follow e.etype with
	| TInst (c,params) ->
		let rec loop_dyn c params =
			match c.cl_dynamic with
			| Some t ->
				let t = apply_params c.cl_params params t in
				if (mode = MGet || mode = MCall) && PMap.mem "resolve" c.cl_fields then begin
					let f = PMap.find "resolve" c.cl_fields in
					begin match f.cf_kind with
						| Method MethMacro -> display_error ctx "The macro accessor is not allowed for field resolve" f.cf_pos
						| _ -> ()
					end;
					let texpect = tfun [ctx.t.tstring] t in
					let tfield = apply_params c.cl_params params (monomorphs f.cf_params f.cf_type) in
					(try Type.unify tfield texpect
					with Unify_error l ->
						display_error ctx "Field resolve has an invalid type" f.cf_pos;
						display_error ctx (error_msg (Unify [Cannot_unify(tfield,texpect)])) f.cf_pos);
					AKExpr (make_call ctx (mk (TField (e,FInstance (c,params,f))) tfield p) [Codegen.type_constant ctx.com (String i) p] t p)
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
				| MSet,Var _ when (match c2 with Some ({ cl_extern = true; cl_path = ("flash" :: _,_) }, _) -> true | _ -> false) ->
					()
				| _, Method _ ->
					display_error ctx "Cannot create closure on super method" p
				| _ ->
					display_error ctx "Normal variables cannot be accessed with 'super', use 'this' instead" p);
			if not (can_access ctx c f false) && not ctx.untyped then display_error ctx ("Cannot access private field " ^ i) p;
			field_access ctx mode f (match c2 with None -> FAnon f | Some (c,tl) -> FInstance (c,tl,f)) (apply_params c.cl_params params t) e p
		with Not_found -> try
			using_field ctx mode e i p
		with Not_found -> try
			loop_dyn c params
		with Not_found -> try
			(* if we have an abstract constraint we have to check its static fields and recurse (issue #2343) *)
			begin match c.cl_kind with
				| KTypeParameter tl ->
					let rec loop tl = match tl with
						| t :: tl ->
							begin match follow t with
								| TAbstract({a_impl = Some c},tl) when PMap.mem i c.cl_statics ->
									let e = mk_cast e t p in
									type_field ctx e i p mode;
								| _ ->
									loop tl
							end
						| [] ->
							raise Not_found
					in
					loop tl
				| _ ->
					raise Not_found
			end
		with Not_found ->
			if PMap.mem i c.cl_statics then error ("Cannot access static field " ^ i ^ " from a class instance") p;
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
		with Not_found -> try
				match !(a.a_status) with
				| Statics {cl_kind = KAbstractImpl a} when does_forward a true ->
					let mt = try module_type_of_type a.a_this with Exit -> raise Not_found in
					let et = type_module_type ctx mt None p in
					type_field ctx et i p mode;
				| _ ->
					raise Not_found
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
					cf_name_pos = null_pos;
					cf_kind = Var { v_read = AccNormal; v_write = (match mode with MSet -> AccNormal | MGet | MCall -> AccNo) };
					cf_expr = None;
					cf_expr_unoptimized = None;
					cf_params = [];
					cf_overloads = [];
				} in
				a.a_fields <- PMap.add i f a.a_fields;
				field_access ctx mode f (FAnon f) (Type.field_type f) e p
		)
	| TMono r ->
		let f = {
			cf_name = i;
			cf_type = mk_mono();
			cf_doc = None;
			cf_meta = no_meta;
			cf_public = true;
			cf_pos = p;
			cf_name_pos = null_pos;
			cf_kind = Var { v_read = AccNormal; v_write = (match mode with MSet -> AccNormal | MGet | MCall -> AccNo) };
			cf_expr = None;
			cf_expr_unoptimized = None;
			cf_params = [];
			cf_overloads = [];
		} in
		let x = ref Opened in
		let t = TAnon { a_fields = PMap.add i f PMap.empty; a_status = x } in
		ctx.opened <- x :: ctx.opened;
		r := Some t;
		field_access ctx mode f (FAnon f) (Type.field_type f) e p
	| TAbstract (a,pl) ->
		let static_abstract_access_through_instance = ref false in
		(try
			let c = (match a.a_impl with None -> raise Not_found | Some c -> c) in
			let f = PMap.find i c.cl_statics in
			if not (can_access ctx c f true) && not ctx.untyped then display_error ctx ("Cannot access private field " ^ i) p;
			let field_type f =
				if not (Meta.has Meta.Impl f.cf_meta) then begin
					static_abstract_access_through_instance := true;
					raise Not_found;
				end;
				let t = field_type ctx c [] f p in
				apply_params a.a_params pl t
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
				let rec loop cfl = match cfl with
					| [] -> error (Printf.sprintf "Field %s cannot be called on %s" f.cf_name (s_type (print_context()) e.etype)) p
					| cf :: cfl ->
						match follow (apply_params a.a_params pl (monomorphs cf.cf_params cf.cf_type)) with
							| TFun((_,_,t1) :: _,_) when type_iseq t1 (Abstract.get_underlying_type a pl) ->
								cf
							| _ ->
								loop cfl
				in
				let f = match f.cf_overloads with
					| [] -> f
					| cfl -> loop (f :: cfl)
				in
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
			if does_forward a false then
				type_field ~resume:true ctx {e with etype = apply_params a.a_params pl a.a_this} i p mode
			else
				raise Not_found
		with Not_found -> try
			using_field ctx mode e i p
		with Not_found -> try
			(match ctx.curfun, e.eexpr with
			| FunMemberAbstract, TConst (TThis) -> type_field ctx {e with etype = apply_params a.a_params pl a.a_this} i p mode;
			| _ -> raise Not_found)
		with Not_found -> try
			let c,cf = match a.a_impl,a.a_resolve with
				| Some c,Some cf -> c,cf
				| _ -> raise Not_found
			in
			let et = type_module_type ctx (TClassDecl c) None p in
			let t = apply_params a.a_params pl (field_type ctx c [] cf p) in
			let ef = mk (TField (et,FStatic (c,cf))) t p in
			AKExpr ((!build_call_ref) ctx (AKUsing(ef,c,cf,e)) [EConst (String i),p] NoValue p)
		with Not_found ->
			if !static_abstract_access_through_instance then error ("Invalid call to static function " ^ i ^ " through abstract instance") p
			else no_field())
	| _ ->
		try using_field ctx mode e i p with Not_found -> no_field()

let type_bind ctx (e : texpr) params p =
	let args,ret = match follow e.etype with TFun(args, ret) -> args, ret | _ -> error "First parameter of callback is not a function" p in
	let vexpr v = mk (TLocal v) v.v_type p in
	let acount = ref 0 in
	let alloc_name n =
		if n = "" || String.length n > 2 && not ctx.is_display_file then begin
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
			let v = alloc_var (alloc_name n) (if o then ctx.t.tnull t else t) p in
			loop args params given_args (missing_args @ [v,o]) (ordered_args @ [vexpr v])
		| (n,o,t) :: args , param :: params ->
			let e = type_expr ctx param (WithType t) in
			unify ctx e.etype t p;
			let v = alloc_var (alloc_name n) t (pos param) in
			loop args params (given_args @ [v,o,Some e]) missing_args (ordered_args @ [vexpr v])
	in
	let given_args,missing_args,ordered_args = loop args params [] [] [] in
	let rec gen_loc_name n =
		let name = if n = 0 then "f" else "f" ^ (string_of_int n) in
		if List.exists (fun (n,_,_) -> name = n) args then gen_loc_name (n + 1) else name
	in
	let loc = alloc_var (gen_loc_name 0) e.etype e.epos in
	let given_args = (loc,false,Some e) :: given_args in
	let inner_fun_args l = List.map (fun (v,o) -> v.v_name, o, v.v_type) l in
	let t_inner = TFun(inner_fun_args missing_args, ret) in
	let call = make_call ctx (vexpr loc) ordered_args ret p in
	let e_ret = match follow ret with
		| TAbstract ({a_path = [],"Void"},_) ->
			call
		| TMono _ ->
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
		| TInst (c,tl) ->
			(try is_dynamic (apply_params c.cl_params tl ((let _,t,_ = Type.class_field c tl f in t))) with Not_found -> false)
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

 let type_generic_function ctx (e,fa) el ?(using_param=None) with_type p =
	let c,tl,cf,stat = match fa with
		| FInstance(c,tl,cf) -> c,tl,cf,false
		| FStatic(c,cf) -> c,[],cf,true
		| _ -> assert false
	in
	if cf.cf_params = [] then error "Function has no type parameters and cannot be generic" p;
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	let map_monos t = apply_params cf.cf_params monos t in
	let map t = if stat then map_monos t else apply_params c.cl_params tl (map_monos t) in
	let t = map cf.cf_type in
	let args,ret = match t,using_param with
		| TFun((_,_,ta) :: args,ret),Some e ->
			let ta = if not (Meta.has Meta.Impl cf.cf_meta) then ta
			else match follow ta with TAbstract(a,tl) -> Abstract.get_underlying_type a tl | _ -> assert false
			in
			(* manually unify first argument *)
			unify ctx e.etype ta p;
			args,ret
		| TFun(args,ret),None -> args,ret
		| _ ->  error "Invalid field type for generic call" p
	in
	begin match with_type with
		| WithType t -> unify ctx ret t p
		| _ -> ()
	end;
	let el,_ = unify_call_args ctx el args ret p false false in
	begin try
		check_constraints ctx cf.cf_name cf.cf_params monos map false p
	with Unify_error l ->
		display_error ctx (error_msg (Unify l)) p
	end;
	let el = match using_param with None -> el | Some e -> e :: el in
	(try
		let gctx = Typeload.make_generic ctx cf.cf_params monos p in
		let name = cf.cf_name ^ "_" ^ gctx.Typeload.name in
		let unify_existing_field tcf pcf = try
			unify_raise ctx tcf t p
		with Error(Unify _,_) as err ->
			display_error ctx ("Cannot create field " ^ name ^ " due to type mismatch") p;
			display_error ctx "Conflicting field was defined here" pcf;
			raise err
		in
		let cf2 = try
			let cf2 = if stat then
				let cf2 = PMap.find name c.cl_statics in
				unify_existing_field cf2.cf_type cf2.cf_pos;
				cf2
			else
				let cf2 = PMap.find name c.cl_fields in
				unify_existing_field cf2.cf_type cf2.cf_pos;
				cf2
			in
			cf2
		with Not_found ->
			let cf2 = mk_field name (map_monos cf.cf_type) cf.cf_pos cf.cf_name_pos in
			if stat then begin
				c.cl_statics <- PMap.add name cf2 c.cl_statics;
				c.cl_ordered_statics <- cf2 :: c.cl_ordered_statics
			end else begin
				if List.memq cf c.cl_overrides then c.cl_overrides <- cf2 :: c.cl_overrides;
				c.cl_fields <- PMap.add name cf2 c.cl_fields;
				c.cl_ordered_fields <- cf2 :: c.cl_ordered_fields
			end;
			ignore(follow cf.cf_type);
			let rec check e = match e.eexpr with
				| TNew({cl_kind = KTypeParameter _} as c,_,_) when not (Typeload.is_generic_parameter ctx c) ->
					display_error ctx "Only generic type parameters can be constructed" e.epos;
					display_error ctx "While specializing this call" p;
				| _ ->
					Type.iter check e
			in
			cf2.cf_expr <- (match cf.cf_expr with
				| None ->
					display_error ctx "Recursive @:generic function" p; None;
				| Some e ->
					let e = Typeload.generic_substitute_expr gctx e in
					check e;
					Some e
			);
			cf2.cf_kind <- cf.cf_kind;
			cf2.cf_public <- cf.cf_public;
			let metadata = List.filter (fun (m,_,_) -> match m with
				| Meta.Generic -> false
				| _ -> true
			) cf.cf_meta in
			cf2.cf_meta <- (Meta.NoCompletion,[],p) :: (Meta.NoUsing,[],p) :: (Meta.GenericInstance,[],p) :: metadata;
			cf2
		in
		let path = match c.cl_kind with
			| KAbstractImpl(a) ->
				a.a_path
			| _ -> c.cl_path
		in
		let e = if stat then type_type ctx path p else e in
		let fa = if stat then FStatic (c,cf2) else FInstance (c,tl,cf2) in
		let e = mk (TField(e,fa)) cf2.cf_type p in
		make_call ctx e el ret p
	with Typeload.Generic_Exception (msg,p) ->
		error msg p)

let call_to_string ctx e =
	(* Ignore visibility of the toString field. *)
	ctx.meta <- (Meta.PrivateAccess,[],e.epos) :: ctx.meta;
	let acc = type_field ctx e "toString" e.epos MCall in
	ctx.meta <- List.tl ctx.meta;
	!build_call_ref ctx acc [] (WithType ctx.t.tstring) e.epos

let get_next_stored_typed_expr_id =
	let uid = ref 0 in
	(fun() -> incr uid; !uid)

let get_stored_typed_expr com id =
	let e = PMap.find id com.stored_typed_exprs in
	Texpr.duplicate_tvars e

let rec type_binop ctx op e1 e2 is_assign_op with_type p =
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) MSet in
		let tt = (match e1 with AKNo _ | AKInline _ | AKUsing _ | AKMacro _ | AKAccess _ -> Value | AKSet(_,t,_) -> WithType t | AKExpr e -> WithType e.etype) in
		let e2 = type_expr ctx e2 tt in
		(match e1 with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e1  ->
			let e2 = AbstractCast.cast_or_unify ctx e1.etype e2 p in
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2 when i1 == i2 -> error "Assigning a value to itself" p
			| TField ({ eexpr = TConst TThis },FInstance (_,_,f1)) , TField ({ eexpr = TConst TThis },FInstance (_,_,f2)) when f1 == f2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AKSet (e,t,cf) ->
			let e2 = AbstractCast.cast_or_unify ctx t e2 p in
			make_call ctx (mk (TField (e,quick_field_dynamic e.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [e2] t p
		| AKAccess(a,tl,c,ebase,ekey) ->
			mk_array_set_call ctx (AbstractCast.find_array_access ctx a tl ekey (Some e2) p) c ebase p
		| AKUsing(ef,_,_,et) ->
			(* this must be an abstract setter *)
			let e2,ret = match follow ef.etype with
				| TFun([_;(_,_,t)],ret) ->
					AbstractCast.cast_or_unify ctx t e2 p,ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			make_call ctx ef [et;e2] ret p
		| AKInline _ | AKMacro _ ->
			assert false)
	| OpAssignOp (OpBoolAnd | OpBoolOr) ->
		error "The operators ||= and &&= are not supported" p
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) MSet with
		| AKNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AKExpr e ->
			let save = save_locals ctx in
			let v = gen_local ctx e.etype e.epos in
			let has_side_effect = OptimizerTexpr.has_side_effect e in
			let e1 = if has_side_effect then (EConst(Ident v.v_name),e.epos) else e1 in
			let eop = type_binop ctx op e1 e2 true with_type p in
			save();
			(match eop.eexpr with
			| TBinop (_,_,e2) ->
				unify ctx eop.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssignOp op,e,e2)) e.etype p;
			| TMeta((Meta.RequiresAssign,_,_),e2) ->
				unify ctx e2.etype e.etype p;
				check_assign ctx e;
				begin match e.eexpr with
					| TArray(ea1,ea2) when has_side_effect ->
						let v1 = gen_local ctx ea1.etype ea1.epos in
						let ev1 = mk (TLocal v1) v1.v_type p in
						let v2 = gen_local ctx ea2.etype ea2.epos in
						let ev2 = mk (TLocal v2) v2.v_type p in
						let e = {e with eexpr = TArray(ev1,ev2)} in
						mk (TBlock [
							mk (TVar(v1,Some ea1)) ctx.t.tvoid p;
							mk (TVar(v2,Some ea2)) ctx.t.tvoid p;
							mk (TVar(v,Some e)) ctx.t.tvoid p;
							mk (TBinop (OpAssign,e,e2)) e.etype p;
						]) e.etype p
					| TField(ea1,fa) when has_side_effect ->
						let v1 = gen_local ctx ea1.etype ea1.epos in
						let ev1 = mk (TLocal v1) v1.v_type p in
						let e = {e with eexpr = TField(ev1,fa)} in
						mk (TBlock [
							mk (TVar(v1,Some ea1)) ctx.t.tvoid p;
							mk (TVar(v,Some e)) ctx.t.tvoid p;
							mk (TBinop (OpAssign,e,e2)) e.etype p;
						]) e.etype p
					| _ ->
						mk (TBinop (OpAssign,e,e2)) e.etype p;
				end
			| _ ->
				(* this must be an abstract cast *)
				check_assign ctx e;
				if has_side_effect then
					mk (TBlock [
						mk (TVar(v,Some e)) ctx.t.tvoid eop.epos;
						eop
					]) eop.etype eop.epos
				else
					eop)
		| AKSet (e,t,cf) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype e.epos in
			let ev = mk (TLocal v) e.etype p in
			let get = type_binop ctx op (EField ((EConst (Ident v.v_name),p),cf.cf_name),p) e2 true with_type p in
			let e' = match get.eexpr with
				| TBinop _ | TMeta((Meta.RequiresAssign,_,_),_) ->
					unify ctx get.etype t p;
					make_call ctx (mk (TField (ev,quick_field_dynamic ev.etype ("set_" ^ cf.cf_name))) (tfun [t] t) p) [get] t p
				| _ ->
					(* abstract setter *)
					get
			in
			l();
			mk (TBlock [
				mk (TVar (v,Some e)) ctx.t.tvoid p;
				e'
			]) t p
		| AKUsing(ef,c,cf,et) ->
			(* abstract setter + getter *)
			let ta = match c.cl_kind with KAbstractImpl a -> TAbstract(a, List.map (fun _ -> mk_mono()) a.a_params) | _ -> assert false in
			let ret = match follow ef.etype with
				| TFun([_;_],ret) -> ret
				| _ ->  error "Invalid field type for abstract setter" p
			in
			let l = save_locals ctx in
			let v,is_temp = match et.eexpr with
				| TLocal v when not (v.v_name = "this") -> v,false
				| _ -> gen_local ctx ta ef.epos,true
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
		| AKAccess(a,tl,c,ebase,ekey) ->
			let cf_get,tf_get,r_get,ekey,_ = AbstractCast.find_array_access ctx a tl ekey None p in
			(* bind complex keys to a variable so they do not make it into the output twice *)
			let ekey,l = match Optimizer.make_constant_expression ctx ekey with
				| Some e -> e, fun () -> None
				| None ->
					let save = save_locals ctx in
					let v = gen_local ctx ekey.etype p in
					let e = mk (TLocal v) ekey.etype p in
					e, fun () -> (save(); Some (mk (TVar (v,Some ekey)) ctx.t.tvoid p))
			in
			let eget = mk_array_get_call ctx (cf_get,tf_get,r_get,ekey,None) c ebase p in
			let eget = type_binop2 ctx op eget e2 true (WithType eget.etype) p in
			unify ctx eget.etype r_get p;
			let cf_set,tf_set,r_set,ekey,eget = AbstractCast.find_array_access ctx a tl ekey (Some eget) p in
			let eget = match eget with None -> assert false | Some e -> e in
			let et = type_module_type ctx (TClassDecl c) None p in
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
			| WithType t ->
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
		type_binop2 ctx op e1 e2 is_assign_op wt p

and type_binop2 ctx op (e1 : texpr) (e2 : Ast.expr) is_assign_op wt p =
	let e2 = type_expr ctx e2 (if op == OpEq || op == OpNotEq then WithType e1.etype else wt) in
	let tint = ctx.t.tint in
	let tfloat = ctx.t.tfloat in
	let tstring = ctx.t.tstring in
	let to_string e =
		let rec loop t = match classify t with
			| KAbstract ({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics ->
				call_to_string ctx e
			| KInt | KFloat | KString -> e
			| KUnk | KDyn | KParam _ | KOther ->
				let std = type_type ctx ([],"Std") e.epos in
				let acc = acc_get ctx (type_field ctx std "string" e.epos MCall) e.epos in
				ignore(follow acc.etype);
				let acc = (match acc.eexpr with TField (e,FClosure (Some (c,tl),f)) -> { acc with eexpr = TField (e,FInstance (c,tl,f)) } | _ -> acc) in
				make_call ctx acc [e] ctx.t.tstring e.epos
			| KAbstract (a,tl) ->
				try
					AbstractCast.cast_or_unify_raise ctx tstring e p
				with Error (Unify _,_) ->
					loop (Abstract.get_underlying_type a tl)
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
		| KAbstract _,KFloat ->
			unify ctx e1.etype tfloat e1.epos;
			tfloat
		| KFloat, KAbstract _ ->
			unify ctx e2.etype tfloat e2.epos;
			tfloat
		| KAbstract _,KInt ->
			unify ctx e1.etype ctx.t.tint e1.epos;
			ctx.t.tint
		| KInt, KAbstract _ ->
			unify ctx e2.etype ctx.t.tint e2.epos;
			ctx.t.tint
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
			(* we only have to check one type here, because unification fails if one is Void and the other is not *)
			(match follow e2.etype with TAbstract({a_path=[],"Void"},_) -> error "Cannot compare Void" p | _ -> ());
			AbstractCast.cast_or_unify_raise ctx e2.etype e1 p,e2
		with Error (Unify _,_) ->
			e1,AbstractCast.cast_or_unify ctx e1.etype e2 p
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
	let find_overload a c tl left =
		let map = apply_params a.a_params tl in
		let make op_cf cf e1 e2 tret =
			if cf.cf_expr = None then begin
				if not (Meta.has Meta.NoExpr cf.cf_meta) then display_error ctx "Recursive operator method" p;
				if not (Meta.has Meta.CoreType a.a_meta) then begin
					(* for non core-types we require that the return type is compatible to the native result type *)
					let e' = make {e1 with etype = Abstract.follow_with_abstracts e1.etype} {e1 with etype = Abstract.follow_with_abstracts e2.etype} in
					let t_expected = e'.etype in
					begin try
						unify_raise ctx tret t_expected p
					with Error (Unify _,_) ->
						match follow tret with
							| TAbstract(a,tl) when type_iseq (Abstract.get_underlying_type a tl) t_expected ->
								()
							| _ ->
								let st = s_type (print_context()) in
								error (Printf.sprintf "The result of this operation (%s) is not compatible with declared return type %s" (st t_expected) (st tret)) p
					end;
				end;
				let e = Codegen.binop op e1 e2 tret p in
				mk_cast e tret p
				(* Codegen.maybe_cast e tret *)
			end else begin
				let e = make_static_call ctx c cf map [e1;e2] tret p in
				e
			end
		in
		(* special case for == and !=: if the second type is a monomorph, assume that we want to unify
		   it with the first type to preserve comparison semantics. *)
	let is_eq_op = match op with OpEq | OpNotEq -> true | _ -> false in
		if is_eq_op then begin match follow e1.etype,follow e2.etype with
			| TMono _,_ | _,TMono _ ->
				Type.unify e1.etype e2.etype
			| _ ->
				()
		end;
		let rec loop ol = match ol with
			| (op_cf,cf) :: ol when op_cf <> op && (not is_assign_op || op_cf <> OpAssignOp(op)) ->
				loop ol
			| (op_cf,cf) :: ol ->
				let is_impl = Meta.has Meta.Impl cf.cf_meta in
				begin match follow cf.cf_type with
					| TFun([(_,_,t1);(_,_,t2)],tret) ->
						let check e1 e2 swapped =
							let map_arguments () =
								let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
								let map t = map (apply_params cf.cf_params monos t) in
								let t1 = map t1 in
								let t2 = map t2 in
								let tret = map tret in
								monos,t1,t2,tret
							in
							let monos,t1,t2,tret = map_arguments() in
							let make e1 e2 = make op_cf cf e1 e2 tret in
							let t1 = if is_impl then Abstract.follow_with_abstracts t1 else t1 in
							let e1,e2 = if left || not left && swapped then begin
								Type.type_eq EqStrict (if is_impl then Abstract.follow_with_abstracts e1.etype else e1.etype) t1;
								e1,AbstractCast.cast_or_unify_raise ctx t2 e2 p
							end else begin
								Type.type_eq EqStrict e2.etype t2;
								AbstractCast.cast_or_unify_raise ctx t1 e1 p,e2
							end in
							check_constraints ctx "" cf.cf_params monos (apply_params a.a_params tl) false cf.cf_pos;
							let check_null e t = if is_eq_op then match e.eexpr with
								| TConst TNull when not (is_explicit_null t) -> raise (Unify_error [])
								| _ -> ()
							in
							(* If either expression is `null` we only allow operator resolving if the argument type
							   is explicitly Null<T> (issue #3376) *)
							if is_eq_op then begin
								check_null e2 t2;
								check_null e1 t1;
							end;
							let e = if not swapped then
								make e1 e2
							else if not (OptimizerTexpr.has_side_effect e1) && not (OptimizerTexpr.has_side_effect e2) then
								make e1 e2
							else
								let v1,v2 = gen_local ctx t1 e1.epos, gen_local ctx t2 e2.epos in
								let ev1,ev2 = mk (TVar(v1,Some e1)) ctx.t.tvoid p,mk (TVar(v2,Some e2)) ctx.t.tvoid p in
								let eloc1,eloc2 = mk (TLocal v1) v1.v_type p,mk (TLocal v2) v2.v_type p in
								let e = make eloc1 eloc2 in
								let e = mk (TBlock [
									ev2;
									ev1;
									e
								]) e.etype e.epos in
								e
							in
							if is_assign_op && op_cf = op then (mk (TMeta((Meta.RequiresAssign,[],p),e)) e.etype e.epos)
							else e
						in
						begin try
							check e1 e2 false
						with Error (Unify _,_) | Unify_error _ -> try
							if not (Meta.has Meta.Commutative cf.cf_meta) then raise Not_found;
							check e2 e1 true
						with Not_found | Error (Unify _,_) | Unify_error _ ->
							loop ol
						end
					| _ ->
						assert false
				end
			| [] ->
				raise Not_found
		in
		loop (if left then a.a_ops else List.filter (fun (_,cf) -> not (Meta.has Meta.Impl cf.cf_meta)) a.a_ops)
	in
	try
		begin match follow e1.etype with
			| TAbstract({a_impl = Some c} as a,tl) -> find_overload a c tl true
			| _ -> raise Not_found
		end
	with Not_found -> try
		begin match follow e2.etype with
			| TAbstract({a_impl = Some c} as a,tl) -> find_overload a c tl false
			| _ -> raise Not_found
		end
	with Not_found ->
		make e1 e2

and type_unop ctx op flag e p =
	let set = (op = Increment || op = Decrement) in
	let acc = type_access ctx (fst e) (snd e) (if set then MSet else MGet) in
	let access e =
		let make e =
			let t = (match op with
			| Not ->
				if flag = Postfix then error "Postfix ! is not supported" p;
				unify ctx e.etype ctx.t.tbool e.epos;
				ctx.t.tbool
			| NegBits ->
				unify ctx e.etype ctx.t.tint e.epos;
				ctx.t.tint
			| Increment
			| Decrement
			| Neg ->
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
						let tcf = apply_params a.a_params pl (monomorphs cf.cf_params cf.cf_type) in
						if Meta.has Meta.Impl cf.cf_meta then begin
							if type_iseq (tfun [apply_params a.a_params pl a.a_this] m) tcf then cf,tcf,m else loop opl
						end else
							if type_iseq (tfun [e.etype] m) tcf then cf,tcf,m else loop opl
					| _ :: opl -> loop opl
				in
				let cf,t,r = try loop a.a_unops with Not_found -> raise Not_found in
				(match cf.cf_expr with
				| None ->
					let e = {e with etype = apply_params a.a_params pl a.a_this} in
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
	let rec loop acc =
		match acc with
		| AKExpr e -> access e
		| AKInline _ | AKUsing _ when not set -> access (acc_get ctx acc p)
		| AKNo s ->
			error ("The field or identifier " ^ s ^ " is not accessible for " ^ (if set then "writing" else "reading")) p
		| AKAccess(a,tl,c,ebase,ekey) ->
			begin try
				(match op with Increment | Decrement -> () | _ -> raise Not_found);
				let v_key = alloc_var "tmp" ekey.etype ekey.epos in
				let evar_key = mk (TVar(v_key,Some ekey)) ctx.com.basic.tvoid ekey.epos in
				let ekey = mk (TLocal v_key) ekey.etype ekey.epos in
				(* get *)
				let e_get = mk_array_get_call ctx (AbstractCast.find_array_access_raise ctx a tl ekey None p) c ebase p in
				let v_get = alloc_var "tmp" e_get.etype e_get.epos in
				let ev_get = mk (TLocal v_get) v_get.v_type p in
				let evar_get = mk (TVar(v_get,Some e_get)) ctx.com.basic.tvoid p in
				(* op *)
				let e_one = mk (TConst (TInt (Int32.of_int 1))) ctx.com.basic.tint p in
				let e_op = mk (TBinop((if op = Increment then OpAdd else OpSub),ev_get,e_one)) ev_get.etype p in
				(* set *)
				let e_set = mk_array_set_call ctx (AbstractCast.find_array_access_raise ctx a tl ekey (Some e_op) p) c ebase p in
				let el = evar_key :: evar_get :: e_set :: (if flag = Postfix then [ev_get] else []) in
				mk (TBlock el) e_set.etype p
			with Not_found ->
				let e = mk_array_get_call ctx (AbstractCast.find_array_access ctx a tl ekey None p) c ebase p in
				loop (AKExpr e)
			end
		| AKInline _ | AKUsing _ | AKMacro _ ->
			error "This kind of operation is not supported" p
		| AKSet (e,t,cf) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype p in
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
				let v2 = gen_local ctx t p in
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
	in
	loop acc

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
				let v = alloc_unbound_var i t p in
				AKExpr (mk (TLocal v) t p)
		end else begin
			if ctx.curfun = FunStatic && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			begin try
				let t = List.find (fun (i2,_) -> i2 = i) ctx.type_params in
				let c = match follow (snd t) with TInst(c,_) -> c | _ -> assert false in
				if Typeload.is_generic_parameter ctx c && Meta.has Meta.Const c.cl_meta then
					AKExpr (type_module_type ctx (TClassDecl c) None p)
				else begin
					display_error ctx ("Type parameter " ^ i ^ " is only available at compilation and is not a runtime value") p;
					AKExpr (mk (TConst TNull) t_dynamic p)
				end
			with Not_found ->
				let err = Unknown_ident i in
				if ctx.in_display then begin
					raise (Error (err,p))
				end;
				match ctx.com.display.dms_kind with
					| DMNone ->
						raise (Error(err,p))
					| DMDiagnostics b when b || ctx.is_display_file ->
						Display.ToplevelCollector.handle_unresolved_identifier ctx i p false;
						let t = mk_mono() in
						AKExpr (mk (TLocal (add_unbound_local ctx i t p)) t p)
					| _ ->
						display_error ctx (error_msg err) p;
						let t = mk_mono() in
						AKExpr (mk (TLocal (add_unbound_local ctx i t p)) t p)
			end
		end

(* MORDOR *)
and handle_efield ctx e p mode =
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
						fields path (type_ident ctx name p)
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
									if ctx.in_display then raise (Parser.TypePath (List.map (fun (n,_,_) -> n) (List.rev acc),None,false));
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
							let path_match t = snd (t_infos t).mt_path = name in
							let t = try
								List.find path_match ctx.m.curmod.m_types
							with Not_found ->
								let t,p = List.find (fun (t,_) -> path_match t) ctx.m.module_types in
								Display.ImportHandling.maybe_mark_import_position ctx p;
								t
							in
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
		let p = pos e in
		match fst e with
		| EField (e,s) ->
			loop ((s,not (is_lower_ident s),p) :: acc) e
		| EConst (Ident i) ->
			type_path ((i,not (is_lower_ident i),p) :: acc)
		| _ ->
			fields acc (type_access ctx (fst e) (snd e))
	in
	loop [] (e,p) mode

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
				let monos = List.map (fun _ -> mk_mono()) c.cl_params in
				let ct, cf = get_constructor ctx c monos p in
				let args = match follow ct with TFun(args,ret) -> args | _ -> assert false in
				let vl = List.map (fun (n,_,t) -> alloc_var n t c.cl_pos) args in
				let vexpr v = mk (TLocal v) v.v_type p in
				let el = List.map vexpr vl in
				let ec,t = match c.cl_kind with
					| KAbstractImpl a ->
						let e = type_module_type ctx (TClassDecl c) None p in
						let e = mk (TField (e,(FStatic (c,cf)))) ct p in
						let t = TAbstract(a,monos) in
						make_call ctx e el t p,t
					| _ ->
						let t = TInst(c,monos) in
						mk (TNew(c,monos,el)) t p,t
				in
				AKExpr(mk (TFunction {
					tf_args = List.map (fun v -> v,None) vl;
					tf_type = t;
					tf_expr = mk (TReturn (Some ec)) t p;
				}) (tfun (List.map (fun v -> v.v_type) vl) t) p)
			| _ -> error "Binding new is only allowed on class types" p
		end;
	| EField _ ->
		handle_efield ctx e p mode
	| EArray (e1,e2) ->
		let e1 = type_expr ctx e1 Value in
		let e2 = type_expr ctx e2 Value in
		let has_abstract_array_access = ref false in
		(try (match follow e1.etype with
		| TAbstract ({a_impl = Some c} as a,pl) when a.a_array <> [] ->
			begin match mode with
			| MSet ->
				(* resolve later *)
				AKAccess (a,pl,c,e1,e2)
			| _ ->
				has_abstract_array_access := true;
				let e = mk_array_get_call ctx (AbstractCast.find_array_access ctx a pl e2 None p) c e1 p in
				AKExpr e
			end
		| _ -> raise Not_found)
		with Not_found ->
		unify ctx e2.etype ctx.t.tint e2.epos;
		let rec loop et =
			match follow et with
			| TInst ({ cl_array_access = Some t; cl_params = pl },tl) ->
				apply_params pl tl t
			| TInst ({ cl_super = Some (c,stl); cl_params = pl },tl) ->
				apply_params pl tl (loop (TInst (c,stl)))
			| TInst ({ cl_path = [],"ArrayAccess" },[t]) ->
				t
			| TInst ({ cl_path = [],"Array"},[t]) when t == t_dynamic ->
				t_dynamic
			| TAbstract(a,tl) when Meta.has Meta.ArrayAccess a.a_meta ->
				loop (apply_params a.a_params tl a.a_this)
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

and type_vars ctx vl p =
	let vl = List.map (fun ((v,pv),t,e) ->
		try
			let t = Typeload.load_type_hint ctx p t in
			let e = (match e with
				| None -> None
				| Some e ->
					let e = type_expr ctx e (WithType t) in
					let e = AbstractCast.cast_or_unify ctx t e p in
					Some e
			) in
			if v.[0] = '$' then display_error ctx "Variables names starting with a dollar are not allowed" p;
			let v = add_local ctx v t pv in
			v.v_meta <- (Meta.UserVariable,[],pv) :: v.v_meta;
			if ctx.in_display && Display.is_display_position pv then
				Display.DisplayEmitter.display_variable ctx.com.display v pv;
			v,e
		with
			Error (e,p) ->
				check_error ctx e p;
				add_unbound_local ctx v t_dynamic pv, None
	) vl in
	match vl with
	| [v,eo] ->
		mk (TVar (v,eo)) ctx.t.tvoid p
	| _ ->
		let e = mk (TBlock (List.map (fun (v,e) -> (mk (TVar (v,e)) ctx.t.tvoid p)) vl)) ctx.t.tvoid p in
		mk (TMeta((Meta.MergeBlock,[],p), e)) e.etype e.epos

and format_string ctx s p =
	let e = ref None in
	let pmin = ref p.pmin in
	let min = ref (p.pmin + 1) in
	let add_expr (enext,p) len =
		min := !min + len;
		let enext = if ctx.in_display && Display.is_display_position p then
			Display.ExprPreprocessing.process_expr ctx.com (enext,p)
		else
			enext,p
		in
		match !e with
		| None -> e := Some enext
		| Some prev ->
			e := Some (EBinop (OpAdd,prev,enext),punion (pos prev) p)
	in
	let add enext len =
		let p = { p with pmin = !min; pmax = !min + len } in
		add_expr (enext,p) len
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
		if slen > 0 then
			add_expr (parse_expr_string ctx scode { p with pmin = !pmin + pos + 2; pmax = !pmin + send + 1 } true) slen;
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
		| [e] ->
			(try
				merge (type_expr ctx e with_type)
			with Error (e,p) -> check_error ctx e p; [])
		| e :: l ->
			try
				let e = type_expr ctx e NoValue in
				merge e @ loop l
			with
				Error (e,p) -> check_error ctx e p; loop l
	in
	let l = loop el in
	let rec loop = function
		| [] -> ctx.t.tvoid
		| [e] -> e.etype
		| _ :: l -> loop l
	in
	mk (TBlock l) (loop l) p

and type_object_decl ctx fl with_type p =
	let dynamic_parameter = ref None in
	let a = (match with_type with
	| WithType t ->
		let rec loop in_abstract_from t =
			match follow t with
			| TAnon a when not (PMap.is_empty a.a_fields) && not in_abstract_from -> ODKWithStructure a
			| TAbstract (a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				(match List.fold_left (fun acc t -> match loop true t with ODKPlain -> acc | t -> t :: acc) [] (get_abstract_froms a pl) with
				| [t] -> t
				| _ -> ODKPlain)
			| TDynamic t when (follow t != t_dynamic) ->
				dynamic_parameter := Some t;
				ODKWithStructure {
					a_status = ref Closed;
					a_fields = PMap.empty;
				}
			| TInst(c,tl) when Meta.has Meta.StructInit c.cl_meta ->
				ODKWithClass(c,tl)
			| _ ->
				ODKPlain
		in
		loop false t
	| _ ->
		ODKPlain
	) in
	let wrap_quoted_meta e =
		mk (TMeta((Meta.QuotedField,[],e.epos),e)) e.etype e.epos
	in
	let type_fields field_map =
		let fields = ref PMap.empty in
		let extra_fields = ref [] in
		let fl = List.map (fun (n,e) ->
			let n,is_quoted,is_valid = Parser.unquote_ident n in
			if PMap.mem n !fields then error ("Duplicate field in object declaration : " ^ n) p;
			let e = try
				let t = (match !dynamic_parameter with Some t -> t | None -> (PMap.find n field_map).cf_type) in
				let e = type_expr ctx e (WithType t) in
				let e = AbstractCast.cast_or_unify ctx t e p in
				(try type_eq EqStrict e.etype t; e with Unify_error _ -> mk (TCast (e,None)) t e.epos)
			with Not_found ->
				if is_valid then
					extra_fields := n :: !extra_fields;
				type_expr ctx e Value
			in
			if is_valid then begin
				if String.length n > 0 && n.[0] = '$' then error "Field names starting with a dollar are not allowed" p;
				let cf = mk_field n e.etype e.epos null_pos in (* TODO: use field pos if we have it *)
				fields := PMap.add n cf !fields;
			end;
			let e = if is_quoted then wrap_quoted_meta e else e in
			(n,e)
		) fl in
		let t = (TAnon { a_fields = !fields; a_status = ref Const }) in
		if not ctx.untyped then begin
			(match PMap.foldi (fun n cf acc -> if not (Meta.has Meta.Optional cf.cf_meta) && not (PMap.mem n !fields) then n :: acc else acc) field_map [] with
				| [] -> ()
				| [n] -> raise_or_display ctx [Unify_custom ("Object requires field " ^ n)] p
				| nl -> raise_or_display ctx [Unify_custom ("Object requires fields: " ^ (String.concat ", " nl))] p);
			(match !extra_fields with
			| [] -> ()
			| _ -> raise_or_display ctx (List.map (fun n -> has_extra_field t n) !extra_fields) p);
		end;
		t, fl
	in
	(match a with
	| ODKPlain ->
		let rec loop (l,acc) (f,e) =
			let f,is_quoted,is_valid = Parser.unquote_ident f in
			if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
			let e = type_expr ctx e Value in
			(match follow e.etype with TAbstract({a_path=[],"Void"},_) -> error "Fields of type Void are not allowed in structures" e.epos | _ -> ());
			let cf = mk_field f e.etype e.epos null_pos in
			let e = if is_quoted then wrap_quoted_meta e else e in
			((f,e) :: l, if is_valid then begin
				if String.length f > 0 && f.[0] = '$' then error "Field names starting with a dollar are not allowed" p;
				PMap.add f cf acc
			end else acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		let x = ref Const in
		ctx.opened <- x :: ctx.opened;
		mk (TObjectDecl (List.rev fields)) (TAnon { a_fields = types; a_status = x }) p
	| ODKWithStructure a ->
		let t, fl = type_fields a.a_fields in
		if !(a.a_status) <> Const then a.a_status := Closed;
		mk (TObjectDecl fl) t p
	| ODKWithClass (c,tl) ->
		let t,ctor = get_constructor ctx c tl p in
		let args = match follow t with
			| TFun(args,_) -> args
			| _ -> assert false
		in
		let fields = List.fold_left (fun acc (n,opt,t) ->
			let f = mk_field n t ctor.cf_pos ctor.cf_name_pos in
			if opt then f.cf_meta <- [(Meta.Optional,[],ctor.cf_pos)];
			PMap.add n f acc
		) PMap.empty args in
		let t,fl = type_fields fields in
		let evars,fl,_ = List.fold_left (fun (evars,elocs,had_side_effect) (s,e) ->
			begin match e.eexpr with
			| TConst _ | TTypeExpr _ | TFunction _ ->
				evars,(s,e) :: elocs,had_side_effect
			| _ ->
				if had_side_effect then begin
					let v = gen_local ctx e.etype e.epos in
					let ev = mk (TVar(v,Some e)) e.etype e.epos in
					let eloc = mk (TLocal v) v.v_type e.epos in
					(ev :: evars),((s,eloc) :: elocs),had_side_effect
				end else
					evars,(s,e) :: elocs,OptimizerTexpr.has_side_effect e
			end
		) ([],[],false) (List.rev fl) in
		let el = List.map (fun (n,_,t) ->
			try List.assoc n fl
			with Not_found -> mk (TConst TNull) t p
		) args in
		let e = mk (TNew(c,tl,el)) (TInst(c,tl)) p in
		mk (TBlock (List.rev (e :: (List.rev evars)))) e.etype e.epos
	)

and type_new ctx path el with_type p =
	let unify_constructor_call c params f ct = match follow ct with
		| TFun (args,r) ->
			(try
				let el,_,_ = unify_field_call ctx (FInstance(c,params,f)) el args r p false in
				el
			with Error (e,p) ->
				display_error ctx (error_msg e) p;
				[])
		| _ ->
			error "Constructor is not a function" p
	in
	let t = if (fst path).tparams <> [] then
		follow (Typeload.load_instance ctx path false p)
	else try
		ctx.call_argument_stack <- el :: ctx.call_argument_stack;
		let t = follow (Typeload.load_instance ctx path true p) in
		ctx.call_argument_stack <- List.tl ctx.call_argument_stack;
		(* Try to properly build @:generic classes here (issue #2016) *)
		begin match t with
			| TInst({cl_kind = KGeneric } as c,tl) -> follow (Typeload.build_generic ctx c p tl)
			| _ -> t
		end
	with Typeload.Generic_Exception _ ->
		(* Try to infer generic parameters from the argument list (issue #2044) *)
		match resolve_typedef (Typeload.load_type_def ctx p (fst path)) with
		| TClassDecl ({cl_constructor = Some cf} as c) ->
			let monos = List.map (fun _ -> mk_mono()) c.cl_params in
			let ct, f = get_constructor ctx c monos p in
			ignore (unify_constructor_call c monos f ct);
			begin try
				let t = Typeload.build_generic ctx c p monos in
				let map = apply_params c.cl_params monos in
				check_constraints ctx (s_type_path c.cl_path) c.cl_params monos map true p;
				t
			with Typeload.Generic_Exception _ as exc ->
				(* If we have an expected type, just use that (issue #3804) *)
				begin match with_type with
					| WithType t ->
						begin match follow t with
							| TMono _ -> raise exc
							| t -> t
						end
					| _ ->
						raise exc
				end
			end
		| mt ->
			error ((s_type_path (t_infos mt).mt_path) ^ " cannot be constructed") p
	in
	Display.DisplayEmitter.check_display_type ctx t (pos path);
	let build_constructor_call c tl =
		let ct, f = get_constructor ctx c tl p in
		if (Meta.has Meta.CompilerGenerated f.cf_meta) then display_error ctx (error_msg (No_constructor (TClassDecl c))) p;
		if not (can_access ctx c f true || is_parent c ctx.curclass) && not ctx.untyped then display_error ctx "Cannot access private constructor" p;
		(match f.cf_kind with
		| Var { v_read = AccRequire (r,msg) } -> (match msg with Some msg -> error msg p | None -> error_require r p)
		| _ -> ());
		let el = unify_constructor_call c tl f ct in
		el,f,ct
	in
	try begin match t with
	| TInst ({cl_kind = KTypeParameter tl} as c,params) ->
		if not (Typeload.is_generic_parameter ctx c) then error "Only generic type parameters can be constructed" p;
		let el = List.map (fun e -> type_expr ctx e Value) el in
		let ct = (tfun (List.map (fun e -> e.etype) el) ctx.t.tvoid) in
		let rec loop t = match follow t with
			| TAnon a ->
				(try
					unify ctx (PMap.find "new" a.a_fields).cf_type ct p;
					true
				with Not_found ->
					 false)
			| TAbstract({a_path = ["haxe"],"Constructible"},_) -> true
			| TInst({cl_kind = KTypeParameter tl},_) -> List.exists loop tl
			| _ -> false
		in
		if not (List.exists loop tl) then raise_error (No_constructor (TClassDecl c)) p;
		mk (TNew (c,params,el)) t p
	| TAbstract({a_impl = Some c} as a,tl) when not (Meta.has Meta.MultiType a.a_meta) ->
		let el,cf,ct = build_constructor_call c tl in
		let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
		let e = mk (TTypeExpr (TClassDecl c)) ta p in
		let e = mk (TField (e,(FStatic (c,cf)))) ct p in
		make_call ctx e el t p
	| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
		let el,_,_ = build_constructor_call c params in
		mk (TNew (c,params,el)) t p
	| _ ->
		error (s_type (print_context()) t ^ " cannot be constructed") p
	end with Error(No_constructor _ as err,p) when ctx.com.display.dms_display ->
		display_error ctx (error_msg err) p;
		Display.Diagnostics.secure_generated_code ctx (mk (TConst TNull) t p)

and type_try ctx e1 catches with_type p =
	let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
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
	let check_catch_type path params =
		List.iter (fun pt ->
			if pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
		) params;
		(match path with
		| x :: _ , _ -> x
		| [] , name -> name)
	in
	let catches = List.fold_left (fun acc ((v,pv),t,e_ast,pc) ->
		let t = Typeload.load_complex_type ctx true p t in
		let rec loop t = match follow t with
			| TInst ({ cl_kind = KTypeParameter _} as c,_) when not (Typeload.is_generic_parameter ctx c) ->
				error "Cannot catch non-generic type parameter" p
			| TInst ({ cl_path = path },params)
			| TEnum ({ e_path = path },params) ->
				check_catch_type path params,t
			| TAbstract(a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
				check_catch_type a.a_path params,t
			| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
				loop (Abstract.get_underlying_type a tl)
			| TDynamic _ -> "",t
			| _ -> error "Catch type must be a class, an enum or Dynamic" (pos e_ast)
		in
		let name,t2 = loop t in
		if v.[0] = '$' then display_error ctx "Catch variable names starting with a dollar are not allowed" p;
		check_unreachable acc t2 (pos e_ast);
		let locals = save_locals ctx in
		let v = add_local ctx v t pv in
		if ctx.is_display_file && Display.is_display_position pv then
			Display.DisplayEmitter.display_variable ctx.com.display v pv;
		let e = type_expr ctx e_ast with_type in
		(* If the catch position is the display position it means we get completion on the catch keyword or some
		   punctuation. Otherwise we wouldn't reach this point. *)
		if ctx.is_display_file && Display.is_display_position pc then ignore(display_expr ctx e_ast e with_type pc);
		v.v_type <- t2;
		locals();
		if with_type <> NoValue then unify ctx e.etype e1.etype e.epos;
		if PMap.mem name ctx.locals then error ("Local variable " ^ name ^ " is preventing usage of this type here") e.epos;
		(v , e) :: acc
	) [] catches in
	mk (TTry (e1,List.rev catches)) (if with_type = NoValue then ctx.t.tvoid else e1.etype) p

and type_map_declaration ctx e1 el with_type p =
	let (tkey,tval,has_type) =
		let get_map_params t = match follow t with
			| TAbstract({a_path=[],"Map"},[tk;tv]) -> tk,tv,true
			| TInst({cl_path=["haxe";"ds"],"IntMap"},[tv]) -> ctx.t.tint,tv,true
			| TInst({cl_path=["haxe";"ds"],"StringMap"},[tv]) -> ctx.t.tstring,tv,true
			| TInst({cl_path=["haxe";"ds"],("ObjectMap" | "EnumValueMap")},[tk;tv]) -> tk,tv,true
			| _ -> mk_mono(),mk_mono(),false
		in
		match with_type with
		| WithType t -> get_map_params t
		| _ -> (mk_mono(),mk_mono(),false)
	in
	let keys = Hashtbl.create 0 in
	let check_key e_key =
		try
			let p = Hashtbl.find keys e_key.eexpr in
			display_error ctx "Duplicate key" e_key.epos;
			error "Previously defined here" p
		with Not_found ->
			Hashtbl.add keys e_key.eexpr e_key.epos;
	in
	let el = e1 :: el in
	let el_kv = List.map (fun e -> match fst e with
		| EBinop(OpArrow,e1,e2) -> e1,e2
		| _ -> error "Expected a => b" (pos e)
	) el in
	let el_k,el_v,tkey,tval = if has_type then begin
		let el_k,el_v = List.fold_left (fun (el_k,el_v) (e1,e2) ->
			let e1 = type_expr ctx e1 (WithType tkey) in
			check_key e1;
			let e1 = AbstractCast.cast_or_unify ctx tkey e1 e1.epos in
			let e2 = type_expr ctx e2 (WithType tval) in
			let e2 = AbstractCast.cast_or_unify ctx tval e2 e2.epos in
			(e1 :: el_k,e2 :: el_v)
		) ([],[]) el_kv in
		el_k,el_v,tkey,tval
	end else begin
		let el_k,el_v = List.fold_left (fun (el_k,el_v) (e1,e2) ->
			let e1 = type_expr ctx e1 Value in
			check_key e1;
			let e2 = type_expr ctx e2 Value in
			(e1 :: el_k,e2 :: el_v)
		) ([],[]) el_kv in
		let unify_min_resume el = try
			unify_min_raise ctx el
		with Error (Unify l,p) when ctx.in_call_args ->
			 raise (WithTypeError(Unify l,p))
		in
		let tkey = unify_min_resume el_k in
		let tval = unify_min_resume el_v in
		el_k,el_v,tkey,tval
	end in
	let m = Typeload.load_module ctx ([],"Map") null_pos in
	let a,c = match m.m_types with
		| (TAbstractDecl ({a_impl = Some c} as a)) :: _ -> a,c
		| _ -> assert false
	in
	let tmap = TAbstract(a,[tkey;tval]) in
	let cf = PMap.find "set" c.cl_statics in
	let v = gen_local ctx tmap p in
	let ev = mk (TLocal v) tmap p in
	let ec = type_module_type ctx (TClassDecl c) None p in
	let ef = mk (TField(ec,FStatic(c,cf))) (tfun [tkey;tval] ctx.t.tvoid) p in
	let el = ev :: List.map2 (fun e1 e2 -> (make_call ctx ef [ev;e1;e2] ctx.com.basic.tvoid p)) el_k el_v in
	let enew = mk (TNew(c,[tkey;tval],[])) tmap p in
	let el = (mk (TVar (v,Some enew)) t_dynamic p) :: (List.rev el) in
	mk (TBlock el) tmap p

and type_local_function ctx name f with_type p =
	let params = Typeload.type_function_params ctx f (match name with None -> "localfun" | Some n -> n) p in
	if params <> [] then begin
		if name = None then display_error ctx "Type parameters not supported in unnamed local functions" p;
		if with_type <> NoValue then error "Type parameters are not supported for rvalue functions" p
	end;
	List.iter (fun tp -> if tp.tp_constraints <> [] then display_error ctx "Type parameter constraints are not supported for local functions" p) f.f_params;
	let inline, v = (match name with
		| None -> false, None
		| Some v when ExtString.String.starts_with v "inline_" -> true, Some (String.sub v 7 (String.length v - 7))
		| Some v -> false, Some v
	) in
	let old_tp,old_in_loop = ctx.type_params,ctx.in_loop in
	ctx.type_params <- params @ ctx.type_params;
	if not inline then ctx.in_loop <- false;
	let rt = Typeload.load_type_hint ctx p f.f_type in
	let args = List.map (fun ((s,_),opt,_,t,c) ->
		let t = Typeload.load_type_hint ctx p t in
		let t, c = Typeload.type_function_arg ctx t c opt p in
		s, c, t
	) f.f_args in
	(match with_type with
	| WithType t ->
		let rec loop t =
			(match follow t with
			| TFun (args2,tr) when List.length args2 = List.length args ->
				List.iter2 (fun (_,_,t1) (_,_,t2) ->
					match follow t1 with
					| TMono _ -> unify ctx t2 t1 p
					| _ -> ()
				) args args2;
				(* unify for top-down inference unless we are expecting Void *)
				begin match follow tr,follow rt with
					| TAbstract({a_path = [],"Void"},_),_ -> ()
					| _,TMono _ -> unify ctx rt tr p
					| _ -> ()
				end
			| TAbstract(a,tl) ->
				loop (Abstract.get_underlying_type a tl)
			| _ -> ())
		in
		loop t
	| NoValue ->
		if name = None then display_error ctx "Unnamed lvalue functions are not supported" p
	| _ ->
		());
	let ft = TFun (fun_args args,rt) in
	let v = (match v with
		| None -> None
		| Some v ->
			if v.[0] = '$' then display_error ctx "Variable names starting with a dollar are not allowed" p;
			Some (add_local ctx v ft p) (* TODO: var pos *)
	) in
	let curfun = match ctx.curfun with
		| FunStatic -> FunStatic
		| FunMemberAbstract -> FunMemberAbstractLocal
		| _ -> FunMemberClassLocal
	in
	let e , fargs = Typeload.type_function ctx args rt curfun f ctx.in_display p in
	ctx.type_params <- old_tp;
	ctx.in_loop <- old_in_loop;
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
			| Filters.Use v2 | Filters.Assign v2 when v == v2 -> raise Exit
			| Filters.Use _ | Filters.Assign _ | Filters.Declare _ -> ()
		in
		let is_rec = (try Filters.local_usage loop e; false with Exit -> true) in
		let decl = (if is_rec then begin
			if inline then display_error ctx "Inline function cannot be recursive" e.epos;
			let vnew = add_local ctx v.v_name ft v.v_pos in
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

and type_array_decl ctx el with_type p =
	let tp = (match with_type with
	| WithType t ->
		let rec loop t =
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
			| TAbstract (a,pl) ->
				(match List.fold_left (fun acc t -> match loop t with None -> acc | Some t -> t :: acc) [] (get_abstract_froms a pl) with
				| [t] -> Some t
				| _ -> None)
			| t ->
				if t == t_dynamic then Some t else None)
		in
		loop t
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
			let e = type_expr ctx e (WithType t) in
			AbstractCast.cast_or_unify ctx t e p;
		) el in
		mk (TArrayDecl el) (ctx.t.tarray t) p)

and type_expr ctx (e,p) (with_type:with_type) =
	match e with
	| EField ((EConst (String s),ps),"code") ->
		if UTF8.length s <> 1 then error "String must be a single UTF8 char" ps;
		mk (TConst (TInt (Int32.of_int (UChar.code (UTF8.get s 0))))) ctx.t.tint p
	| EField(_,n) when n.[0] = '$' ->
		error "Field names starting with $ are not allowed" p
	| EConst (Ident s) ->
		if s = "super" && with_type <> NoValue && not ctx.in_display then error "Cannot use super as value" p;
		let e = maybe_type_against_enum ctx (fun () -> type_ident ctx s p MGet) with_type p in
		acc_get ctx e p
	| EField _
	| EArray _ ->
		acc_get ctx (type_access ctx e p MGet) p
	| EConst (Regexp (r,opt)) ->
		let str = mk (TConst (TString r)) ctx.t.tstring p in
		let opt = mk (TConst (TString opt)) ctx.t.tstring p in
		let t = Typeload.load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[str;opt])) t p
	| EConst (String s) when s <> "" && Lexer.is_fmt_string p ->
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
		type_object_decl ctx fl with_type p
	| EArrayDecl [(EFor _,_) | (EWhile _,_) as e] ->
		let v = gen_local ctx (mk_mono()) p in
		let et = ref (EConst(Ident "null"),p) in
		let rec map_compr (e,p) =
			match e with
			| EFor(it,e2) -> (EFor (it, map_compr e2),p)
			| EWhile(cond,e2,flag) -> (EWhile (cond,map_compr e2,flag),p)
			| EIf (cond,e2,None) -> (EIf (cond,map_compr e2,None),p)
			| EBlock [e] -> (EBlock [map_compr e],p)
			| EParenthesis e2 -> (EParenthesis (map_compr e2),p)
			| EBinop(OpArrow,a,b) ->
				et := (ENew(({tpackage=[];tname="Map";tparams=[];tsub=None},null_pos),[]),p);
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
		type_map_declaration ctx e1 el with_type p
	| EArrayDecl el ->
		type_array_decl ctx el with_type p
	| EVars vl ->
		type_vars ctx vl p
	| EFor (it,e2) ->
		let rec loop_ident display e1 = match e1 with
			| EConst(Ident i),p -> i,p,display
			| EDisplay(e1,_),_ -> loop_ident true e1
			| _ -> error "Identifier expected" (pos e1)
		in
		let rec loop display e1 = match fst e1 with
			| EIn(e1,e2) -> loop_ident display e1,e2
			| EDisplay(e1,_) -> loop true e1
			| _ -> error "For expression should be 'v in expr'" (snd it)
		in
		let (i, pi, display), e1 = loop false it in
		let e1 = type_expr ctx e1 Value in
		let old_loop = ctx.in_loop in
		let old_locals = save_locals ctx in
		ctx.in_loop <- true;
		let e2 = Expr.ensure_block e2 in
		let default() =
			let t, pt = Typeload.t_iterator ctx in
			let i = add_local ctx i pt pi in
			let e1 = (match follow e1.etype with
			| TMono _
			| TDynamic _ ->
				display_error ctx "You can't iterate on a Dynamic value, please specify Iterator or Iterable" e1.epos;
				e1
			| TLazy _ ->
				assert false
			| _ ->
				(try
					AbstractCast.cast_or_unify_raise ctx t e1 p
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
			if display then ignore(handle_display ctx (EConst(Ident i.v_name),i.v_pos) (WithType i.v_type));
			let e2 = type_expr ctx e2 NoValue in
			(try Optimizer.optimize_for_loop_iterator ctx i e1 e2 p with Exit -> mk (TFor (i,e1,e2)) ctx.t.tvoid p)
		in
		let e = match Optimizer.optimize_for_loop ctx (i,pi) e1 e2 p with
			| Some e ->
				if display then ignore(handle_display ctx (EConst(Ident i),pi) Value);
				e
			| None -> default()
		in
		ctx.in_loop <- old_loop;
		old_locals();
		e
	| EIn _ ->
		error "This expression is not allowed outside a for loop" p
	| ETernary (e1,e2,e3) ->
		type_expr ctx (EIf (e1,e2,Some e3),p) with_type
	| EIf (e,e1,e2) ->
		let e = type_expr ctx e Value in
		let e = AbstractCast.cast_or_unify ctx ctx.t.tbool e p in
		let e1 = type_expr ctx (Expr.ensure_block e1) with_type in
		(match e2 with
		| None ->
			mk (TIf (e,e1,None)) ctx.t.tvoid p
		| Some e2 ->
			let e2 = type_expr ctx (Expr.ensure_block e2) with_type in
			let e1,e2,t = match with_type with
				| NoValue -> e1,e2,ctx.t.tvoid
				| Value -> e1,e2,unify_min ctx [e1; e2]
				| WithType t when (match follow t with TMono _ -> true | _ -> false) -> e1,e2,unify_min ctx [e1; e2]
				| WithType t ->
					let e1 = AbstractCast.cast_or_unify ctx t e1 e1.epos in
					let e2 = AbstractCast.cast_or_unify ctx t e2 e2.epos in
					e1,e2,t
			in
			mk (TIf (e,e1,Some e2)) t p)
	| EWhile (cond,e,NormalWhile) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond Value in
		let cond = AbstractCast.cast_or_unify ctx ctx.t.tbool cond p in
		ctx.in_loop <- true;
		let e = type_expr ctx (Expr.ensure_block e) NoValue in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,NormalWhile)) ctx.t.tvoid p
	| EWhile (cond,e,DoWhile) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let e = type_expr ctx (Expr.ensure_block e) NoValue in
		ctx.in_loop <- old_loop;
		let cond = type_expr ctx cond Value in
		let cond = AbstractCast.cast_or_unify ctx ctx.t.tbool cond cond.epos in
		mk (TWhile (cond,e,DoWhile)) ctx.t.tvoid p
	| ESwitch (e1,cases,def) ->
		let wrap e1 = mk (TMeta((Meta.Ast,[e,p],p),e1)) e1.etype e1.epos in
		let e = match_expr ctx e1 cases def with_type p in
		wrap e
	| EReturn e ->
		begin match e with
			| None ->
				let v = ctx.t.tvoid in
				unify ctx v ctx.ret p;
				mk (TReturn None) t_dynamic p
			| Some e ->
				let e = type_expr ctx e (WithType ctx.ret) in
				let e = AbstractCast.cast_or_unify ctx ctx.ret e p in
				begin match follow e.etype with
				| TAbstract({a_path=[],"Void"},_) ->
					(* if we get a Void expression (e.g. from inlining) we don't want to return it (issue #4323) *)
					mk (TBlock [
						e;
						mk (TReturn None) t_dynamic p
					]) t_dynamic e.epos;
				| _ ->
					mk (TReturn (Some e)) t_dynamic p
				end
		end
	| EBreak ->
		if not ctx.in_loop then display_error ctx "Break outside loop" p;
		mk TBreak t_dynamic p
	| EContinue ->
		if not ctx.in_loop then display_error ctx "Continue outside loop" p;
		mk TContinue t_dynamic p
	| ETry (e1,[]) ->
		type_expr ctx e1 with_type
	| ETry (e1,catches) ->
		type_try ctx e1 catches with_type p
	| EThrow e ->
		let e = type_expr ctx e Value in
		mk (TThrow e) (mk_mono()) p
	| ECall (e,el) ->
		type_call ctx e el with_type p
	| ENew (t,el) ->
		type_new ctx t el with_type p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction (name,f) ->
		type_local_function ctx name f with_type p
	| EUntyped e ->
		let old = ctx.untyped in
		ctx.untyped <- true;
		if not (Meta.has Meta.HasUntyped ctx.curfield.cf_meta) then ctx.curfield.cf_meta <- (Meta.HasUntyped,[],p) :: ctx.curfield.cf_meta;
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
		let t = Typeload.load_complex_type ctx true p t in
		let check_param pt = match follow pt with
			| TMono _ -> () (* This probably means that Dynamic wasn't bound (issue #4675). *)
			| t when t == t_dynamic -> ()
			| _ ->error "Cast type parameters must be Dynamic" p
		in
		let rec loop t = match follow t with
			| TInst (_,params) | TEnum (_,params) ->
				List.iter check_param params;
				(match follow t with
				| TInst (c,_) ->
					(match c.cl_kind with KTypeParameter _ -> error "Can't cast to a type parameter" p | _ -> ());
					TClassDecl c
				| TEnum (e,_) -> TEnumDecl e
				| _ -> assert false);
			| TAbstract (a,params) when Meta.has Meta.RuntimeValue a.a_meta ->
				List.iter check_param params;
				TAbstractDecl a
			| TAbstract (a,params) ->
				loop (Abstract.get_underlying_type a params)
			| _ ->
				error "Cast type must be a class or an enum" p
		in
		let texpr = loop t in
		mk (TCast (type_expr ctx e Value,Some texpr)) t p
	| EDisplay (e,iscall) ->
		if iscall then handle_signature_display ctx e with_type
		else handle_display ctx e with_type
	| EDisplayNew t ->
		assert false
		(*let t = Typeload.load_instance ctx t true p in
		(match follow t with
		| TInst (c,params) | TAbstract({a_impl = Some c},params) ->
			let ct, f = get_constructor ctx c params p in
			raise (Display.DisplaySignatures ((ct,f.cf_doc) :: List.map (fun f -> (f.cf_type,f.cf_doc)) f.cf_overloads))
		| _ ->
			error "Not a class" p)*)
	| ECheckType (e,t) ->
		let t = Typeload.load_complex_type ctx true p t in
		let e = type_expr ctx e (WithType t) in
		let e = AbstractCast.cast_or_unify ctx t e p in
		if e.etype == t then e else mk (TCast (e,None)) t p
	| EMeta (m,e1) ->
		let old = ctx.meta in
		ctx.meta <- m :: ctx.meta;
		let e () = type_expr ctx e1 with_type in
		let e = match m with
			| (Meta.ToString,_,_) ->
				let e = e() in
				(match follow e.etype with
					| TAbstract({a_impl = Some c},_) when PMap.mem "toString" c.cl_statics -> call_to_string ctx e
					| _ -> e)
			| (Meta.This,_,_) ->
				let e = match ctx.this_stack with
					| [] -> error "Cannot type @:this this here" p
					| e :: _ -> e
				in
				let rec loop e = match e.eexpr with
					| TConst TThis -> get_this ctx e.epos
					| _ -> Type.map_expr loop e
				in
				loop e
			| (Meta.Analyzer,_,_) ->
				let e = e() in
				{e with eexpr = TMeta(m,e)}
			| (Meta.MergeBlock,_,_) ->
				begin match fst e1 with
				| EBlock el ->
					let e = type_block ctx el with_type p in
					{e with eexpr = TMeta(m,e)}
				| _ -> e()
				end
			| (Meta.StoredTypedExpr,_,_) ->
				let id = match e1 with (EConst (Int s),_) -> int_of_string s | _ -> assert false in
				get_stored_typed_expr ctx.com id
			| (Meta.NoPrivateAccess,_,_) ->
				ctx.meta <- List.filter (fun(m,_,_) -> m <> Meta.PrivateAccess) ctx.meta;
				e()
			| (Meta.Fixed,_,_) when ctx.com.platform=Cpp ->
				let e = e() in
				{e with eexpr = TMeta(m,e)}
			| _ -> e()
		in
		ctx.meta <- old;
		e

and get_submodule_fields ctx path =
	let m = Hashtbl.find ctx.g.modules path in
	let tl = List.filter (fun t -> path <> (t_infos t).mt_path && not (t_infos t).mt_private) m.m_types in
	let tl = List.map (fun mt ->
		let infos = t_infos mt in
		(snd infos.mt_path),Display.FKType (type_of_module_type mt),infos.mt_doc
	) tl in
	tl

and handle_display ctx e_ast with_type =
	let old = ctx.in_display,ctx.in_call_args in
	ctx.in_display <- true;
	ctx.in_call_args <- false;
	let e = match e_ast,with_type with
	| (EConst (Ident "$type"),_),_ ->
		let mono = mk_mono() in
		raise (Display.DisplaySignatures ([(TFun(["expression",false,mono],mono),Some "Outputs type of argument as a warning and uses argument as value")],0))
	| (EConst (Ident "trace"),_),_ ->
		raise (Display.DisplaySignatures ([(tfun [t_dynamic] ctx.com.basic.tvoid,Some "Print given arguments")],0))
	| (EConst (Ident "_"),p),WithType t ->
		mk (TConst TNull) t p (* This is "probably" a bind skip, let's just use the expected type *)
	| _ -> try
		type_expr ctx e_ast with_type
	with Error (Unknown_ident n,_) ->
		raise (Parser.TypePath ([n],None,false))
	| Error (Type_not_found (path,_),_) as err ->
		begin try
			raise (Display.DisplayFields (get_submodule_fields ctx path))
		with Not_found ->
			raise err
		end
	in
	let p = e.epos in
	let e = match with_type with
		| WithType t -> (try AbstractCast.cast_or_unify_raise ctx t e e.epos with Error (Unify l,p) -> e)
		| _ -> e
	in
	ctx.in_display <- fst old;
	ctx.in_call_args <- snd old;
	display_expr ctx e_ast e with_type p

and handle_signature_display ctx e_ast with_type =
	ctx.in_display <- true;
	let p = pos e_ast in
	let find_constructor_types t = match follow t with
		| TInst (c,tl) | TAbstract({a_impl = Some c},tl) ->
			let ct,cf = get_constructor ctx c tl p in
			let tl = (ct,cf.cf_doc) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc) cf.cf_overloads in
			tl
		| _ ->
			[]
	in
	let tl,el,p0 = match fst e_ast with
		| ECall(e1,el) ->
			let e1 = try
				type_expr ctx e1 Value
			with Error (Unknown_ident "trace",_) ->
				let e = expr_of_type_path (["haxe";"Log"],"trace") p in
				type_expr ctx e Value
			in
			let tl = match e1.eexpr with
				| TField(_,fa) ->
					begin match extract_field fa with
						| Some cf -> (e1.etype,cf.cf_doc) :: List.rev_map (fun cf' -> cf'.cf_type,cf.cf_doc) cf.cf_overloads
						| None -> [e1.etype,None]
					end
				| TConst TSuper ->
					find_constructor_types e1.etype
				| _ -> [e1.etype,None]
			in
			tl,el,e1.epos
		| ENew(tpath,el) ->
			let t = Typeload.load_instance ctx tpath true p in
			find_constructor_types t,el,pos tpath
		| _ -> error "Call expected" p
	in
	let rec follow_with_callable (t,doc) = match follow t with
		| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta -> follow_with_callable (Abstract.get_underlying_type a tl,doc)
		| t -> (t,doc)
	in
	let tl = List.map follow_with_callable tl in
	let rec loop i p1 el = match el with
		| (e,p2) :: el ->
			if Display.is_display_position (punion p1 p2) then i else loop (i + 1) p2 el
		| [] ->
			i
	in
	let display_arg = loop 0 p0 el in
	(* If our display position exceeds the argument number we add a null expression in order to make
	   unify_call_args error out. *)
	let el = if display_arg >= List.length el then el @ [EConst (Ident "null"),null_pos] else el in
	let rec loop acc tl = match tl with
		| (t,doc) :: tl ->
			let keep t = match t with
				| TFun (args,r) ->
					begin try
						let _ = unify_call_args' ctx el args r p false false in
						true
					with
					| Error(Call_error (Not_enough_arguments _),_) -> true
					| _ -> false
					end
				| _ -> false
			in
			loop (if keep t then (t,doc) :: acc else acc) tl
		| [] ->
			acc
	in
	let overloads = match loop [] tl with [] -> tl | tl -> tl in
	raise (Display.DisplaySignatures(overloads,display_arg))

and display_expr ctx e_ast e with_type p =
	let get_super_constructor () = match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let _, f = get_constructor ctx c params p in
			f
	in
	match ctx.com.display.dms_kind with
	| DMResolve _ | DMPackage | DMSignature ->
		assert false
	| DMType ->
		let rec loop e = match e.eexpr with
			| TVar(v,_) -> v.v_type,None
			| TCall({eexpr = TConst TSuper; etype = t},_) -> t,None
			| TNew({cl_kind = KAbstractImpl a},tl,_) -> TType(abstract_module_type a tl,[]),None
			| TNew(c,tl,_) -> TInst(c,tl),None
			| TTypeExpr (TClassDecl {cl_kind = KAbstractImpl a}) -> TType(abstract_module_type a (List.map snd a.a_params),[]),None
			| TField(e1,FDynamic "bind") when (match follow e1.etype with TFun _ -> true | _ -> false) -> e1.etype,None
			| TReturn (Some e1) -> loop e1 (* No point in letting the internal Dynamic surface (issue #5655) *)
			| TField(_,(FStatic(c,cf) | FInstance(c,_,cf) | FClosure(Some(c,_),cf))) ->
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
				e.etype,cf.cf_doc
			| TField(_,FEnum(_,ef)) ->
				e.etype,ef.ef_doc
			| _ -> e.etype,None
		in
		let t,doc = loop e in
		raise (Display.DisplayType (t,p,doc))
	| DMUsage _ ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) ->
			ef.ef_meta <- (Meta.Usage,[],p) :: ef.ef_meta;
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) ->
			cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
		| TLocal v | TVar(v,_) ->
			v.v_meta <- (Meta.Usage,[],p) :: v.v_meta;
		| TTypeExpr mt ->
			let ti = t_infos mt in
			ti.mt_meta <- (Meta.Usage,[],p) :: ti.mt_meta;
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
			with Not_found ->
				()
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				cf.cf_meta <- (Meta.Usage,[],p) :: cf.cf_meta;
			with Not_found ->
				()
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> ()
				| Some (c,_) -> c.cl_meta <- (Meta.Usage,[],p) :: c.cl_meta;
			end
		| TCall(e1,_) ->
			loop e1
		| _ ->
			()
		in
		loop e;
		e
	| DMPosition ->
		let rec loop e = match e.eexpr with
		| TField(_,FEnum(_,ef)) -> [ef.ef_pos]
		| TField(_,(FAnon cf | FInstance (_,_,cf) | FStatic (_,cf) | FClosure (_,cf))) -> [cf.cf_pos]
		| TLocal v | TVar(v,_) -> [v.v_pos]
		| TTypeExpr mt -> [(t_infos mt).mt_pos]
		| TNew(c,tl,_) ->
			begin try
				let _,cf = get_constructor ctx c tl p in
				[cf.cf_pos]
			with Not_found ->
				[]
			end
		| TCall({eexpr = TConst TSuper},_) ->
			begin try
				let cf = get_super_constructor() in
				[cf.cf_pos]
			with Not_found ->
				[]
			end
		| TConst TSuper ->
			begin match ctx.curclass.cl_super with
				| None -> []
				| Some (c,_) -> [c.cl_pos]
			end
		| TCall(e1,_) ->
			loop e1
		| _ ->
			[]
		in
		let pl = loop e in
		raise (Display.DisplayPosition pl);
	| DMToplevel ->
		raise (Display.DisplayToplevel (Display.ToplevelCollector.run ctx false))
	| DMField | DMNone | DMModuleSymbols _ | DMDiagnostics _ | DMStatistics ->
		let opt_args args ret = TFun(List.map(fun (n,o,t) -> n,true,t) args,ret) in
		let e = match e.eexpr with
			| TField (e1,fa) ->
				if field_name fa = "bind" then (match follow e1.etype with
					| TFun(args,ret) -> {e1 with etype = opt_args args ret}
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
		let should_access c cf stat =
			if c != ctx.curclass && not cf.cf_public && String.length cf.cf_name > 4 then begin match String.sub cf.cf_name 0 4 with
				| "get_" | "set_" -> false
				| _ -> can_access ctx c cf stat
			end else
				can_access ctx c cf stat
		in
		let rec get_fields t =
			match follow t with
			| TInst (c,params) ->
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
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
					let m = merge ~cond:(fun f -> should_access c f false) c.cl_fields m in
					let m = (match c.cl_kind with
						| KTypeParameter pl -> List.fold_left (fun acc t -> merge acc (get_fields t)) m pl
						| _ -> m
					) in
					PMap.map (fun f -> { f with cf_type = apply_params c.cl_params params (opt_type f.cf_type); cf_public = true; }) m
				in
				loop c params
			| TAbstract({a_impl = Some c} as a,pl) ->
				if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
				let fields = try
					let _,el,_ = Meta.get Meta.Forward a.a_meta in
					let sl = ExtList.List.filter_map (fun e -> match fst e with
						| EConst(Ident s) -> Some s
						| _ -> None
					) el in
					let fields = get_fields (apply_params a.a_params pl a.a_this) in
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
					if f.cf_name <> "_new" && should_access c f true && Meta.has Meta.Impl f.cf_meta && not (Meta.has Meta.Enum f.cf_meta) then begin
						let f = prepare_using_field f in
						let t = apply_params a.a_params pl (follow f.cf_type) in
						PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type t } acc
					end else
						acc
				) c.cl_statics fields
			| TAnon a when PMap.is_empty a.a_fields ->
				begin match with_type with
				| WithType t -> get_fields t
				| _ -> a.a_fields
				end
			| TAnon a ->
				(match !(a.a_status) with
				| Statics c ->
					if Meta.has Meta.CoreApi c.cl_meta then merge_core_doc ctx c;
					let is_abstract_impl = match c.cl_kind with KAbstractImpl _ -> true | _ -> false in
					let pm = match c.cl_constructor with None -> PMap.empty | Some cf -> PMap.add "new" cf PMap.empty in
					PMap.fold (fun f acc ->
						if should_access c f true && (not is_abstract_impl || not (Meta.has Meta.Impl f.cf_meta) || Meta.has Meta.Enum f.cf_meta) then
							PMap.add f.cf_name { f with cf_public = true; cf_type = opt_type f.cf_type } acc else acc
					) a.a_fields pm
				| _ ->
					a.a_fields)
			| TFun (args,ret) ->
				let t = opt_args args ret in
				let cf = mk_field "bind" (tfun [t] t) p null_pos in
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
			| (c,_) :: l ->
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
		let fields = match fst e_ast with
			| EConst(String s) when String.length s = 1 ->
				let cf = mk_field "code" ctx.t.tint e.epos null_pos in
				cf.cf_doc <- Some "The character code of this character (inlined at compile-time).";
				cf.cf_kind <- Var { v_read = AccNormal; v_write = AccNever };
				PMap.add cf.cf_name cf fields
			| _ ->
				fields
		in
		let fields = PMap.fold (fun f acc -> if Meta.has Meta.NoCompletion f.cf_meta then acc else f :: acc) fields [] in
		let get_field acc f =
			List.fold_left (fun acc f ->
				let kind = match f.cf_kind with Method _ -> Display.FKMethod f.cf_type | Var _ -> Display.FKVar f.cf_type in
				if f.cf_public then (f.cf_name,kind,f.cf_doc) :: acc else acc
			) acc (f :: f.cf_overloads)
		in
		let fields = List.fold_left get_field [] fields in
		let fields = try
			let sl = string_list_of_expr_path_raise e_ast in
			fields @ get_submodule_fields ctx (List.tl sl,List.hd sl)
		with Exit | Not_found ->
			fields
		in
		raise (Display.DisplayFields fields)

and maybe_type_against_enum ctx f with_type p =
	try
		begin match with_type with
		| WithType t ->
			let rec loop t = match follow t with
				| TEnum (en,_) ->
					en.e_path,en.e_names,TEnumDecl en
				| TAbstract ({a_impl = Some c} as a,_) when has_meta Meta.Enum a.a_meta ->
					a.a_path,List.map (fun cf -> cf.cf_name) c.cl_ordered_fields,TAbstractDecl a
				| TAbstract (a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					begin match get_abstract_froms a pl with
						| [t] -> loop t
						| _ -> raise Exit
					end
				(* We might type against an enum constructor. *)
				| TFun(_,tr) ->
					loop tr
				| _ ->
					raise Exit
			in
			let path,fields,mt = loop t in
			let old = ctx.m.curmod.m_types in
			ctx.m.curmod.m_types <- ctx.m.curmod.m_types @ [mt];
			let e = try
				f()
			with Error (Unknown_ident n,_) ->
				raise_or_display_message ctx (StringError.string_error n fields ("Identifier '" ^ n ^ "' is not part of " ^ s_type_path path)) p;
				AKExpr (mk (TConst TNull) (mk_mono()) p)
			in
			ctx.m.curmod.m_types <- old;
			e
		| _ ->
			raise Exit
		end
	with Exit ->
		f()

and type_call ctx e el (with_type:with_type) p =
	let def () =
		let e = maybe_type_against_enum ctx (fun () -> type_access ctx (fst e) (snd e) MCall) with_type p in
		let e = build_call ctx e el with_type p in
		e
	in
	match e, el with
	| (EConst (Ident "trace"),p) , e :: el ->
		if Common.defined ctx.com Define.NoTraces then
			null ctx.t.tvoid p
		else
		let mk_to_string_meta e = EMeta((Meta.ToString,[],pos e),e),pos e in
		let params = (match el with [] -> [] | _ -> ["customParams",(EArrayDecl (List.map mk_to_string_meta el) , p)]) in
		let infos = mk_infos ctx p params in
		if (platform ctx.com Js || platform ctx.com Python) && el = [] && has_dce ctx.com then
			let e = type_expr ctx e Value in
			let infos = type_expr ctx infos Value in
			let e = try
				begin match follow e.etype with
					| TInst({cl_path=[],"String"},_) -> raise Not_found
					| TMono _ -> raise Not_found
					| TDynamic _ -> raise Not_found
					| _ -> ()
				end;
				let acc = type_field ~resume:true ctx e "toString" p MCall in
				build_call ctx acc [] (WithType ctx.t.tstring) p
			with Not_found ->
				e
			in
			let v_trace = alloc_unbound_var "`trace" t_dynamic p in
			mk (TCall (mk (TLocal v_trace) t_dynamic p,[e;infos])) ctx.t.tvoid p
		else
			type_expr ctx (ECall ((EField ((EField ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[mk_to_string_meta e;infos]),p) NoValue
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
			| TEnum _ ->
				let e = match_expr ctx e [[epat],None,Some (EConst(Ident "true"),p),p] (Some (Some (EConst(Ident "false"),p),p)) (WithType ctx.t.tbool) p in
				(* TODO: add that back *)
(* 				let locals = !get_pattern_locals_ref ctx epat t in
				PMap.iter (fun _ (_,p) -> display_error ctx "Capture variables are not allowed" p) locals; *)
				e
			| _ -> def ())
	| (EConst (Ident "__unprotect__"),_) , [(EConst (String _),_) as e] ->
		let e = type_expr ctx e Value in
		if Common.platform ctx.com Flash then
			let t = tfun [e.etype] e.etype in
			let v_unprotect = alloc_unbound_var "__unprotect__" t p in
			mk (TCall (mk (TLocal v_unprotect) t p,[e])) e.etype e.epos
		else
			e
	| (EDisplay((EConst (Ident "super"),_ as e1),false),_),_ ->
		handle_display ctx (ECall(e1,el),p) with_type
	| (EConst (Ident "super"),sp) , el ->
		if ctx.curfun <> FunConstructor then error "Cannot call super constructor outside class constructor" p;
		let el, t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let ct, f = get_constructor ctx c params p in
			if (Meta.has Meta.CompilerGenerated f.cf_meta) then display_error ctx (error_msg (No_constructor (TClassDecl c))) p;
			let el = (match follow ct with
			| TFun (args,r) ->
				let el,_,_ = unify_field_call ctx (FInstance(c,params,f)) el args r p false in
				el
			| _ ->
				error "Constructor is not a function" p
			) in
			el , TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) ctx.t.tvoid p
	| _ ->
		def ()

and build_call ctx acc el (with_type:with_type) p =
	match acc with
	| AKInline (ethis,f,fmode,t) when Meta.has Meta.Generic f.cf_meta ->
		type_generic_function ctx (ethis,fmode) el with_type p
	| AKInline (ethis,f,fmode,t) ->
		(match follow t with
			| TFun (args,r) ->
				let _,_,mk_call = unify_field_call ctx fmode el args r p true in
				mk_call ethis p
			| _ ->
				error (s_type (print_context()) t ^ " cannot be called") p
		)
	| AKUsing (et,cl,ef,eparam) when Meta.has Meta.Generic ef.cf_meta ->
		(match et.eexpr with
		| TField(ec,fa) ->
			type_generic_function ctx (ec,fa) el ~using_param:(Some eparam) with_type p
		| _ -> assert false)
	| AKUsing (et,cl,ef,eparam) ->
		begin match ef.cf_kind with
		| Method MethMacro ->
			let ethis = type_module_type ctx (TClassDecl cl) None p in
			let eparam,f = push_this ctx eparam in
			let e = build_call ctx (AKMacro (ethis,ef)) (eparam :: el) with_type p in
			f();
			e
		| _ ->
			let t = follow (field_type ctx cl [] ef p) in
			(* for abstracts we have to apply their parameters to the static function *)
			let t,tthis = match follow eparam.etype with
				| TAbstract(a,tl) when Meta.has Meta.Impl ef.cf_meta -> apply_params a.a_params tl t,apply_params a.a_params tl a.a_this
				| te -> t,te
			in
			let params,args,r,eparam = match t with
				| TFun ((_,_,t1) :: args,r) ->
					unify ctx tthis t1 eparam.epos;
					let ef = prepare_using_field ef in
					begin match unify_call_args ctx el args r p (ef.cf_kind = Method MethInline) (is_forced_inline (Some cl) ef) with
					| el,TFun(args,r) -> el,args,r,eparam
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
			| Some e -> (fun() -> type_expr ctx e with_type))
		| _ ->
			(* member-macro call : since we will make a static call, let's found the actual class and not its subclass *)
			(match follow ethis.etype with
			| TInst (c,_) ->
				let rec loop c =
					if PMap.mem cf.cf_name c.cl_fields then
						let eparam,f = push_this ctx ethis in
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
			(* display additional info in the case the error is not part of our original call *)
			if ep.pfile <> p.pfile || ep.pmax < p.pmin || ep.pmin > p.pmax then begin
				Typeload.locate_macro_error := false;
				old ctx msg ep;
				Typeload.locate_macro_error := true;
				ctx.com.error "Called from macro here" p;
			end else
				old ctx msg ep;
		);
		let e = try
			f()
		with Error (m,p) ->
			ctx.on_error <- old;
			!ethis_f();
			raise (Fatal_error ((error_msg m),p))
		in
		let e = Display.Diagnostics.secure_generated_code ctx e in
		ctx.on_error <- old;
		!ethis_f();
		e
	| AKNo _ | AKSet _ | AKAccess _ ->
		ignore(acc_get ctx acc p);
		assert false
	| AKExpr e ->
		let rec loop t = match follow t with
		| TFun (args,r) ->
			begin match e.eexpr with
				| TField(e1,fa) when not (match fa with FEnum _ -> true | _ -> false) ->
					begin match fa with
						| FInstance(_,_,cf) | FStatic(_,cf) when Meta.has Meta.Generic cf.cf_meta ->
							type_generic_function ctx (e1,fa) el with_type p
						| _ ->
							let _,_,mk_call = unify_field_call ctx fa el args r p false in
							mk_call e1 e.epos
					end
				| _ ->
					let el, tfunc = unify_call_args ctx el args r p false false in
					let r = match tfunc with TFun(_,r) -> r | _ -> assert false in
					mk (TCall ({e with etype = tfunc},el)) r p
			end
		| TAbstract(a,tl) when Meta.has Meta.Callable a.a_meta ->
			loop (Abstract.get_underlying_type a tl)
		| TMono _ ->
			let t = mk_mono() in
			let el = List.map (fun e -> type_expr ctx e Value) el in
			unify ctx (tfun (List.map (fun e -> e.etype) el) t) e.etype e.epos;
			mk (TCall (e,el)) t p
		| t ->
			let el = List.map (fun e -> type_expr ctx e Value) el in
			let t = if t == t_dynamic then
				t_dynamic
			else if ctx.untyped then
				mk_mono()
			else
				error (s_type (print_context()) e.etype ^ " cannot be called") e.epos
			in
			mk (TCall (e,el)) t p
		in
		loop e.etype

(* ---------------------------------------------------------------------- *)
(* FINALIZATION *)

let get_main ctx types =
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
		let main = mk (TCall (mk (TField (emain,fmode)) ft null_pos,[])) r null_pos in
		(* add haxe.EntryPoint.run() call *)
		let main = (try
			let et = List.find (fun t -> t_path t = (["haxe"],"EntryPoint")) types in
			let ec = (match et with TClassDecl c -> c | _ -> assert false) in
			let ef = PMap.find "run" ec.cl_statics in
			let p = null_pos in
			let et = mk (TTypeExpr et) (TAnon { a_fields = PMap.empty; a_status = ref (Statics ec) }) p in
			let call = mk (TCall (mk (TField (et,FStatic (ec,ef))) ef.cf_type p,[])) ctx.t.tvoid p in
			mk (TBlock [main;call]) ctx.t.tvoid p
		with Not_found ->
			main
		) in
		Some main

let finalize ctx =
	flush_pass ctx PFinal "final";
	match ctx.com.callbacks.after_typing with
		| [] ->
			()
		| fl ->
			let rec loop handled_types =
				let all_types = Hashtbl.fold (fun _ m acc -> m.m_types @ acc) ctx.g.modules [] in
				match (List.filter (fun mt -> not (List.memq mt handled_types)) all_types) with
				| [] ->
					()
				| new_types ->
					List.iter (fun f -> f new_types) fl;
					flush_pass ctx PFinal "final";
					loop all_types
			in
			loop []

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
	get_main ctx !types, List.rev !types, sorted_modules

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

let typing_timer ctx need_type f =
	let t = Common.timer "typing" in
	let old = ctx.com.error and oldp = ctx.pass in
	(*
		disable resumable errors... unless we are in display mode (we want to reach point of completion)
	*)
	(*if ctx.com.display = DMNone then ctx.com.error <- (fun e p -> raise (Error(Custom e,p)));*) (* TODO: review this... *)
	ctx.com.error <- (fun e p -> raise (Error(Custom e,p)));
	if need_type && ctx.pass < PTypeField then ctx.pass <- PTypeField;
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
			Interp.compiler_error (error_msg ekind) p
		| WithTypeError (l,p) ->
			exit();
			Interp.compiler_error (error_msg l) p
		| e ->
			exit();
			raise e

let load_macro_ref : (typer -> bool -> path -> string -> pos -> (typer * ((string * bool * t) list * t * tclass * Type.tclass_field) * (Interp.value list -> Interp.value option))) ref = ref (fun _ _ _ _ -> assert false)

let make_macro_api ctx p =
	let parse_expr_string s p inl =
		typing_timer ctx false (fun() -> parse_expr_string ctx s p inl)
	in
	{
		Interp.pos = p;
		Interp.get_com = (fun() -> ctx.com);
		Interp.get_type = (fun s ->
			typing_timer ctx true (fun() ->
				let path = parse_path s in
				let tp = match List.rev (fst path) with
					| s :: sl when String.length s > 0 && (match s.[0] with 'A'..'Z' -> true | _ -> false) ->
						{ tpackage = List.rev sl; tname = s; tparams = []; tsub = Some (snd path) }
					| _ ->
						{ tpackage = fst path; tname = snd path; tparams = []; tsub = None }
				in
				try
					let m = Some (Typeload.load_instance ctx (tp,null_pos) true p) in
					m
				with Error (Module_not_found _,p2) when p == p2 ->
					None
			)
		);
		Interp.resolve_type = (fun t p ->
			typing_timer ctx true (fun() -> Typeload.load_complex_type ctx false p (t,null_pos))
		);
		Interp.get_module = (fun s ->
			typing_timer ctx true (fun() ->
				let path = parse_path s in
				let m = List.map type_of_module_type (Typeload.load_module ctx path p).m_types in
				m
			)
		);
		Interp.after_typing = (fun f ->
			Common.add_typing_filter ctx.com (fun tl ->
				let t = macro_timer ctx "afterTyping" in
				f tl;
				t()
			)
		);
		Interp.on_generate = (fun f ->
			Common.add_filter ctx.com (fun() ->
				let t = macro_timer ctx "onGenerate" in
				f (List.map type_of_module_type ctx.com.types);
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
			typing_timer ctx true (fun() -> type_expr ctx e Value)
		);
		Interp.type_macro_expr = (fun e ->
			let e = typing_timer ctx true (fun() -> type_expr ctx e Value) in
			let rec loop e = match e.eexpr with
				| TField(_,FStatic(c,({cf_kind = Method _} as cf))) -> ignore(!load_macro_ref ctx false c.cl_path cf.cf_name e.epos)
				| _ -> Type.iter loop e
			in
			loop e;
			e
		);
		Interp.store_typed_expr = (fun te ->
			let p = te.epos in
			let id = get_next_stored_typed_expr_id() in
			ctx.com.stored_typed_exprs <- PMap.add id te ctx.com.stored_typed_exprs;
			let eid = (EConst (Int (string_of_int id))), p in
			(EMeta ((Meta.StoredTypedExpr,[],p), eid)), p
		);
		Interp.allow_package = (fun v -> Common.allow_package ctx.com v);
		Interp.type_patch = (fun t f s v ->
			typing_timer ctx false (fun() ->
				let v = (match v with None -> None | Some s ->
					match parse_string ctx.com ("typedef T = " ^ s) null_pos false with
					| _,[ETypedef { d_data = ct },_] -> Some ct
					| _ -> assert false
				) in
				let tp = get_type_patch ctx t (Some (f,s)) in
				match v with
				| None -> tp.tp_remove <- true
				| Some _ -> tp.tp_type <- Option.map fst v
			);
		);
		Interp.meta_patch = (fun m t f s ->
			let m = (match parse_string ctx.com (m ^ " typedef T = T") null_pos false with
				| _,[ETypedef t,_] -> t.d_meta
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
					"types", Interp.enc_array (List.map (fun t -> Interp.encode_type (type_of_module_type t)) ctx.com.types);
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
							let v = Interp.encode_type (type_of_module_type t) in
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
				| (WithType t) :: _ -> Some t
				| _ -> None
		);
		Interp.get_call_arguments = (fun() ->
			match ctx.call_argument_stack with
				| [] -> None
				| el :: _ -> Some el
		);
		Interp.get_local_method = (fun() ->
			ctx.curfield.cf_name;
		);
		Interp.get_local_using = (fun() ->
			List.map fst ctx.m.module_using;
		);
		Interp.get_local_imports = (fun() ->
			ctx.m.module_imports;
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
				let mnew = Typeload.type_module ctx m ctx.m.curmod.m_extra.m_file [tdef,pos] pos in
				mnew.m_extra.m_kind <- MFake;
				add_dependency mnew ctx.m.curmod;
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
			let pos = (match types with [] -> Ast.null_pos | (_,p) :: _ -> p) in
			let imports = List.map (fun (il,ik) -> EImport(il,ik),pos) imports in
			let usings = List.map (fun tp ->
				let sl = tp.tpackage @ [tp.tname] @ (match tp.tsub with None -> [] | Some s -> [s]) in
				EUsing (List.map (fun s -> s,null_pos) sl),pos
			) usings in
			let types = imports @ usings @ types in
			let mpath = Ast.parse_path m in
			begin try
				let m = Hashtbl.find ctx.g.modules mpath in
				ignore(Typeload.type_types_into_module ctx m types pos)
			with Not_found ->
				let mnew = Typeload.type_module ctx mpath ctx.m.curmod.m_extra.m_file types pos in
				mnew.m_extra.m_kind <- MFake;
				add_dependency mnew ctx.m.curmod;
			end
		);
		Interp.module_dependency = (fun mpath file ismacro ->
			let m = typing_timer ctx false (fun() -> Typeload.load_module ctx (parse_path mpath) p) in
			if ismacro then
				m.m_extra.m_macro_calls <- file :: List.filter ((<>) file) m.m_extra.m_macro_calls
			else
				add_dependency m (create_fake_module ctx file);
		);
		Interp.current_module = (fun() ->
			ctx.m.curmod
		);
		Interp.current_macro_module = (fun () -> assert false);
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
		Interp.cast_or_unify = (fun t e p ->
			AbstractCast.cast_or_unify_raise ctx t e p
		);
		Interp.add_global_metadata = (fun s1 s2 config ->
			let meta = (match parse_string ctx.com (s2 ^ " typedef T = T") null_pos false with
				| _,[ETypedef t,_] -> t.d_meta
				| _ -> assert false
			) in
			List.iter (fun m ->
				ctx.g.global_metadata <- (ExtString.String.nsplit s1 ".",m,config) :: ctx.g.global_metadata;
			) meta;
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
		Interp.clear mint mctx.com;
		init_macro_interp ctx mctx mint;
		mint
	end else mint in
	(* we should maybe ensure that all filters in Main are applied. Not urgent atm *)
	let expr_filters = [AbstractCast.handle_abstract_casts mctx; Filters.captured_vars mctx.com; Filters.rename_local_vars mctx] in

	(*
		some filters here might cause side effects that would break compilation server.
		let's save the minimal amount of information we need
	*)
	let minimal_restore t =
		match t with
		| TClassDecl c ->
			let meta = c.cl_meta in
			let path = c.cl_path in
			c.cl_restore <- (fun() -> c.cl_meta <- meta; c.cl_path <- path);
		| _ ->
			()
	in
	let type_filters = [
		Filters.add_field_inits mctx;
		minimal_restore;
		Filters.apply_native_paths mctx
	] in
	let ready = fun t ->
		Filters.apply_filters_once mctx expr_filters t;
		List.iter (fun f -> f t) type_filters
	in
	(try Interp.add_types mint types ready
	with Error (e,p) -> raise (Fatal_error(error_msg e,p)));
	Filters.next_compilation()

let create_macro_interp ctx mctx =
	let com2 = mctx.com in
	let mint, init = (match !macro_interp_cache with
		| None ->
			let mint = Interp.create com2 (make_macro_api ctx Ast.null_pos) in
			mint, (fun() -> init_macro_interp ctx mctx mint)
		| Some mint ->
			Interp.do_reuse mint (make_macro_api ctx Ast.null_pos);
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
		com2.display <- DisplayMode.create DMNone;
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
		mctx.is_display_file <- ctx.is_display_file;
		create_macro_interp ctx mctx;
		api, mctx

let load_macro ctx display cpath f p =
	(*
		The time measured here takes into account both macro typing an init, but benchmarks
		shows that - unless you re doing heavy statics vars init - the time is mostly spent in
		typing the classes needed for macro execution.
	*)
	let t = macro_timer ctx ("typing (+init) " ^ (Ast.s_type_path cpath) ^ "." ^ f) in
	let api, mctx = get_macro_context ctx p in
	let mint = Interp.get_ctx() in
	let cpath, sub = (match List.rev (fst cpath) with
		| name :: pack when name.[0] >= 'A' && name.[0] <= 'Z' -> (List.rev pack,name), Some (snd cpath)
		| _ -> cpath, None
	) in
	let meth = try Hashtbl.find mctx.com.cached_macros (cpath,f) with Not_found ->
		(* Temporarily enter display mode while typing the macro. *)
		if display then mctx.com.display <- ctx.com.display;
		let m = (try Hashtbl.find ctx.g.types_module cpath with Not_found -> cpath) in
		let mloaded = Typeload.load_module mctx m p in
		api.Interp.current_macro_module <- (fun() -> mloaded);
		mctx.m <- {
			curmod = mloaded;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		add_dependency ctx.m.curmod mloaded;
		let mt = Typeload.load_type_def mctx p { tpackage = fst cpath; tname = snd cpath; tparams = []; tsub = sub } in
		let cl, meth = (match mt with
			| TClassDecl c ->
				finalize mctx;
				c, (try PMap.find f c.cl_statics with Not_found -> error ("Method " ^ f ^ " not found on class " ^ s_type_path cpath) p)
			| _ -> error "Macro should be called on a class" p
		) in
		let meth = (match follow meth.cf_type with TFun (args,ret) -> args,ret,cl,meth | _ -> error "Macro call should be a method" p) in
		mctx.com.display <- DisplayMode.create DMNone;
		if not ctx.in_macro then flush_macro_context mint ctx;
		Hashtbl.add mctx.com.cached_macros (cpath,f) meth;
		mctx.m <- {
			curmod = null_module;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		meth
	in
	t();
	let call args =
		let t = macro_timer ctx (s_type_path cpath ^ "." ^ f) in
		incr stats.s_macros_called;
		let r = Interp.call_path (Interp.get_ctx()) ((fst cpath) @ [(match sub with None -> snd cpath | Some s -> s)]) f args api in
		t();
		r
	in
	mctx, meth, call

type macro_arg_type =
	| MAExpr
	| MAFunction
	| MAOther

let type_macro ctx mode cpath f (el:Ast.expr list) p =
	let mctx, (margs,mret,mclass,mfield), call_macro = load_macro ctx (mode = MDisplay) cpath f p in
	let mpos = mfield.cf_pos in
	let ctexpr = { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = None } in
	let expr = Typeload.load_instance mctx (ctexpr,null_pos) false p in
	(match mode with
	| MDisplay ->
		raise Exit (* We don't have to actually call the macro. *)
	| MExpr ->
		unify mctx mret expr mpos;
	| MBuild ->
		let ctfields = { tpackage = []; tname = "Array"; tparams = [TPType (CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = Some "Field" },null_pos)]; tsub = None } in
		let tfields = Typeload.load_instance mctx (ctfields,null_pos) false p in
		unify mctx mret tfields mpos
	| MMacroType ->
		let cttype = { tpackage = ["haxe";"macro"]; tname = "Type"; tparams = []; tsub = None } in
		let ttype = Typeload.load_instance mctx (cttype,null_pos) false p in
		try
			unify_raise mctx mret ttype mpos;
			(* TODO: enable this again in the future *)
			(* ctx.com.warning "Returning Type from @:genericBuild macros is deprecated, consider returning ComplexType instead" p; *)
		with Error (Unify _,_) ->
			let cttype = { tpackage = ["haxe";"macro"]; tname = "Expr"; tparams = []; tsub = Some ("ComplexType") } in
			let ttype = Typeload.load_instance mctx (cttype,null_pos) false p in
			unify_raise mctx mret ttype mpos;
	);
	(*
		if the function's last argument is of Array<Expr>, split the argument list and use [] for unify_call_args
	*)
	let el,el2 = match List.rev margs with
		| (_,_,TInst({cl_path=([], "Array")},[e])) :: rest when (try Type.type_eq EqStrict e expr; true with Unify_error _ -> false) ->
			let rec loop (acc1,acc2) el1 el2 = match el1,el2 with
				| [],[] ->
					List.rev acc1, List.rev acc2
				| [], e2 :: [] ->
					(List.rev ((EArrayDecl [],p) :: acc1), [])
				| [], _ ->
					(* not enough arguments, will be handled by unify_call_args *)
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
		let eargs = List.map (fun (n,o,t) ->
			try unify_raise mctx t expr p; (n, o, t_dynamic), MAExpr
			with Error (Unify _,_) -> match follow t with
				| TFun _ ->
					(n,o,t_dynamic), MAFunction
				| _ ->
					(n,o,t), MAOther
			) margs in
		(*
			this is quite tricky here : we want to use unify_call_args which will type our AST expr
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
					(EVars [("__tmp",null_pos),Some (CTPath ctexpr,p),Some (EConst (Ident "null"),p)],p);
					(EConst (Ident "__tmp"),p);
				],p)
			) in
			(* let's track the index by doing [e][index] (we will keep the expression type this way) *)
			incr index;
			(EArray ((EArrayDecl [e],p),(EConst (Int (string_of_int (!index))),p)),p)
		) el in
		let elt, _ = unify_call_args mctx constants (List.map fst eargs) t_dynamic p false false in
		List.iter (fun f -> f()) (!todo);
		List.map2 (fun (_,mct) e ->
			let e, et = (match e.eexpr with
				(* get back our index and real expression *)
				| TArray ({ eexpr = TArrayDecl [e] }, { eexpr = TConst (TInt index) }) -> List.nth el (Int32.to_int index), e
				(* added by unify_call_args *)
				| TConst TNull -> (EConst (Ident "null"),e.epos), e
				| _ -> assert false
			) in
			let ictx = Interp.get_ctx() in
			match mct with
			| MAExpr ->
				Interp.encode_expr e
			| MAFunction ->
				let e = ictx.Interp.curapi.Interp.type_macro_expr e in
				begin match Interp.eval_expr ictx e with
				| Some v -> v
				| None -> Interp.VNull
				end
			| MAOther -> match Interp.eval_expr ictx et with
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
				| MExpr | MDisplay -> Interp.decode_expr v
				| MBuild ->
					let fields = (match v with
						| Interp.VNull ->
							(match ctx.g.get_build_infos() with
							| None -> assert false
							| Some (_,_,fields) -> fields)
						| _ ->
							List.map Interp.decode_field (Interp.dec_array v)
					) in
					(EVars [("fields",null_pos),Some (CTAnonymous fields,p),None],p)
				| MMacroType ->
					let t = if v = Interp.VNull then
						mk_mono()
					else try
						let ct = Interp.decode_ctype v in
						Typeload.load_complex_type ctx false p ct;
					with Interp.Invalid_expr ->
						Interp.decode_type v
					in
					ctx.ret <- t;
					(EBlock [],p)
				)
			with Interp.Invalid_expr ->
				if v = Interp.VNull then
					error "Unexpected null value returned from macro" p
				else
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
		if Common.defined ctx.com Define.MacroDebug then
			ctx.com.warning "Macro-in-macro call detected" p;
		let e = (EConst (Ident "__dollar__delay_call"),p) in
		Some (EUntyped (ECall (e,[EConst (Int (string_of_int pos)),p]),p),p)
	end else
		call()
	) in
	e

let call_macro ctx path meth args p =
	let mctx, (margs,_,mclass,mfield), call = load_macro ctx false path meth p in
	let el, _ = unify_call_args mctx args margs t_dynamic p false false in
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
			global_metadata = [];
			delayed = [];
			debug_delayed = [];
			delayed_macros = DynArray.create();
			doinline = com.display.dms_inline && not (Common.defined com Define.NoInline);
			hook_generate = [];
			get_build_infos = (fun() -> None);
			std = null_module;
			global_using = [];
			do_inherit = Typeload.on_inherit;
			do_create = create;
			do_macro = type_macro;
			do_load_module = Typeload.load_module;
			do_optimize = Optimizer.reduce_expression;
			do_build_instance = Typeload.build_instance;
		};
		m = {
			curmod = null_module;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = [];
		};
		is_display_file = false;
		meta = [];
		this_stack = [];
		with_type_stack = [];
		call_argument_stack = [];
		pass = PBuildModule;
		macro_depth = 0;
		untyped = false;
		curfun = FunStatic;
		in_loop = false;
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
		in_call_args = false;
		on_error = (fun ctx msg p -> ctx.com.error msg p);
	} in
	ctx.g.std <- (try
		Typeload.load_module ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) -> error "Standard library not found" null_pos
	);
	(* We always want core types to be available so we add them as default imports (issue #1904 and #3131). *)
	ctx.m.module_types <- List.map (fun t -> t,null_pos) ctx.g.std.m_types;
	List.iter (fun t ->
		match t with
		| TAbstractDecl a ->
			(match snd a.a_path with
			| "Void" -> ctx.t.tvoid <- TAbstract (a,[]);
			| "Float" -> ctx.t.tfloat <- TAbstract (a,[]);
			| "Int" -> ctx.t.tint <- TAbstract (a,[])
			| "Bool" -> ctx.t.tbool <- TAbstract (a,[])
			| "Dynamic" -> t_dynamic_def := TAbstract(a,List.map snd a.a_params);
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
						if not (is_null ~no_lazy:true t) then TType (td,[t]) else t
					with Exit ->
						(* don't force lazy evaluation *)
						let r = ref (fun() -> assert false) in
						r := (fun() ->
							let t = (if not (is_null t) then TType (td,[t]) else t) in
							r := (fun() -> t);
							t
						);
						TLazy r
				in
				ctx.t.tnull <- mk_null;
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
	| [TClassDecl c1;TClassDecl c2] -> ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
	| [TClassDecl c1] ->
		let m = Typeload.load_module ctx (["haxe"],"EnumValueTools") null_pos in
		(match m.m_types with
		| [TClassDecl c2 ] -> ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
		| _ -> assert false);
	| _ -> assert false);
	ctx

;;
unify_min_ref := unify_min;
make_call_ref := make_call;
get_constructor_ref := get_constructor;
cast_or_unify_ref := AbstractCast.cast_or_unify_raise;
type_module_type_ref := type_module_type;
find_array_access_raise_ref := AbstractCast.find_array_access_raise;
build_call_ref := build_call;
load_macro_ref := load_macro;
merge_core_doc_ref := merge_core_doc