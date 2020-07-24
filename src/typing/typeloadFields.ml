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

(* Logic for building fields. *)

open Globals
open Ast
open Type
open Typecore
open Typeload
open DisplayTypes.DisplayMode
open CompletionItem.ClassFieldOrigin
open Common
open Error

class context_init = object(self)
	val mutable l = []

	method add (f : unit -> unit) =
		l <- f :: l

	method run =
		let l' = l in
		l <- [];
		List.iter (fun f -> f()) (List.rev l')
end

type class_init_ctx = {
	tclass : tclass; (* I don't trust ctx.curclass because it's mutable. *)
	is_lib : bool;
	is_native : bool;
	is_core_api : bool;
	is_class_debug : bool;
	extends_public : bool;
	abstract : tabstract option;
	context_init : context_init;
	mutable has_display_field : bool;
	mutable delayed_expr : (typer * tlazy ref option) list;
	mutable force_constructor : bool;
	mutable uninitialized_final : pos option;
}

type field_kind =
	| FKNormal
	| FKConstructor
	| FKInit

type field_init_ctx = {
	is_inline : bool;
	is_final : bool;
	is_static : bool;
	override : pos option;
	overload : pos option;
	is_extern : bool;
	is_abstract : bool;
	is_macro : bool;
	is_abstract_member : bool;
	is_display_field : bool;
	is_field_debug : bool;
	field_kind : field_kind;
	display_modifier : placed_access option;
	mutable do_bind : bool;
	mutable do_add : bool;
	(* If true, cf_expr = None makes a difference in the logic. We insert a dummy expression in
	   display mode in order to address this. *)
	mutable expr_presence_matters : bool;
}

let locate_macro_error = ref true

let dump_class_context cctx =
	Printer.s_record_fields "" [
		"tclass",Printer.s_tclass "\t" cctx.tclass;
		"is_lib",string_of_bool cctx.is_lib;
		"is_native",string_of_bool cctx.is_native;
		"is_core_api",string_of_bool cctx.is_core_api;
		"is_class_debug",string_of_bool cctx.is_class_debug;
		"extends_public",string_of_bool cctx.extends_public;
		"abstract",Printer.s_opt (Printer.s_tabstract "\t") cctx.abstract;
		"force_constructor",string_of_bool cctx.force_constructor;
	]

let s_field_kind = function
	| FKNormal -> "FKNormal"
	| FKConstructor -> "FKConstructor"
	| FKInit -> "FKInit"

let dump_field_context fctx =
	Printer.s_record_fields "" [
		"is_inline",string_of_bool fctx.is_inline;
		"is_static",string_of_bool fctx.is_static;
		"is_extern",string_of_bool fctx.is_extern;
		"is_macro",string_of_bool fctx.is_macro;
		"is_abstract_member",string_of_bool fctx.is_abstract_member;
		"is_display_field",string_of_bool fctx.is_display_field;
		"is_field_debug",string_of_bool fctx.is_field_debug;
		"field_kind",s_field_kind fctx.field_kind;
		"do_bind",string_of_bool fctx.do_bind;
		"do_add",string_of_bool fctx.do_add;
		"expr_presence_matters",string_of_bool fctx.expr_presence_matters;
	]


let is_java_native_function ctx meta pos = try
	match Meta.get Meta.Native meta with
		| (Meta.Native,[],_) ->
			ctx.com.warning "@:native metadata for jni functions is deprecated. Use @:java.native instead." pos;
			true
		| _ -> false
	with | Not_found -> Meta.has Meta.NativeJni meta

(**** end of strict meta handling *****)

let get_method_args field =
	match field.cf_expr with
		| Some { eexpr = TFunction { tf_args = args } } -> args
		| _ -> raise Not_found

(**
	Get super constructor data required for @:structInit descendants.
*)
let get_struct_init_super_info ctx c p =
	match c.cl_super with
		| Some ({ cl_constructor = Some ctor } as csup, cparams) ->
			let args = (try get_method_args ctor with Not_found -> []) in
			let tl_rev,el_rev =
				List.fold_left (fun (args,exprs) (v,value) ->
					let opt = match value with
						| Some _ -> true
						| None -> Meta.has Meta.Optional v.v_meta
					in
					let t = if opt then ctx.t.tnull v.v_type else v.v_type in
					(v.v_name,opt,t) :: args,(mk (TLocal v) v.v_type p) :: exprs
				) ([],[]) args
			in
			let super_expr = mk (TCall (mk (TConst TSuper) (TInst (csup,cparams)) p, List.rev el_rev)) ctx.t.tvoid p in
			(args,Some super_expr,List.rev tl_rev)
		| _ ->
			[],None,[]

(**
	Generates a constructor for a @:structInit class `c` if it does not have one yet.
*)
let ensure_struct_init_constructor ctx c ast_fields p =
	match c.cl_constructor with
	| Some _ ->
		()
	| None ->
		let field_has_default_expr field_name =
			List.exists
				(fun ast_field ->
					match ast_field.cff_name with
						| (name, _) when name <> field_name -> false
						| _ ->
							match ast_field.cff_kind with
								| FVar (_, Some _) | FProp (_, _, _, Some _) -> true
								| _ -> false
				)
				ast_fields
		in
		let super_args,super_expr,super_tl = get_struct_init_super_info ctx c p in
		let params = List.map snd c.cl_params in
		let ethis = mk (TConst TThis) (TInst(c,params)) p in
		let args,el,tl = List.fold_left (fun (args,el,tl) cf -> match cf.cf_kind with
			| Var { v_write = AccNever } -> args,el,tl
			| Var _ ->
				let has_default_expr = field_has_default_expr cf.cf_name in
				let opt = has_default_expr || (Meta.has Meta.Optional cf.cf_meta) in
				let t = if opt then ctx.t.tnull cf.cf_type else cf.cf_type in
				let v = alloc_var VGenerated cf.cf_name t p in
				let ef = mk (TField(ethis,FInstance(c,params,cf))) cf.cf_type p in
				let ev = mk (TLocal v) v.v_type p in
				if opt && not (Meta.has Meta.Optional v.v_meta) then
					v.v_meta <- (Meta.Optional,[],null_pos) :: v.v_meta;
				(* this.field = <constructor_argument> *)
				let assign_expr = mk (TBinop(OpAssign,ef,ev)) cf.cf_type p in
				let e =
					if has_default_expr then
						begin
							(* <constructor_argument> != null *)
							let condition = mk (TBinop(OpNotEq, ev, (null t p))) ctx.t.tbool p in
							(* if(<constructor_argument> != null) this.field = <constructor_argument> *)
							mk (TIf(condition, assign_expr, None)) ctx.t.tvoid p
						end
					else
						assign_expr
				in
				(v,None) :: args,e :: el,(cf.cf_name,opt,t) :: tl
			| Method _ ->
				args,el,tl
		) ([],[],[]) (List.rev c.cl_ordered_fields) in
		let el = match super_expr with Some e -> e :: el | None -> el in
		let tf = {
			tf_args = args @ super_args;
			tf_type = ctx.t.tvoid;
			tf_expr = mk (TBlock el) ctx.t.tvoid p
		} in
		let e = mk (TFunction tf) (TFun(tl @ super_tl,ctx.t.tvoid)) p in
		let cf = mk_field "new" e.etype p null_pos in
		cf.cf_expr <- Some e;
		cf.cf_type <- e.etype;
		cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos];
		cf.cf_kind <- Method MethNormal;
		c.cl_constructor <- Some cf

let transform_abstract_field com this_t a_t a f =
	let stat = List.mem_assoc AStatic f.cff_access in
	let p = f.cff_pos in
	match f.cff_kind with
	| FProp ((("get" | "never"),_),(("set" | "never"),_),_,_) when not stat ->
		f
	| FProp _ when not stat && not (Meta.has Meta.Enum f.cff_meta) ->
		error "Member property accessors must be get/set or never" p;
	| FFun fu when fst f.cff_name = "new" && not stat ->
		let init p = (EVars [mk_evar ~t:this_t ("this",null_pos)],p) in
		let cast e = (ECast(e,None)),pos e in
		let ret p = (EReturn (Some (cast (EConst (Ident "this"),p))),p) in
		let meta = (Meta.NoCompletion,[],null_pos) :: f.cff_meta in
		if Meta.has Meta.MultiType a.a_meta then begin
			if List.mem_assoc AInline f.cff_access then error "MultiType constructors cannot be inline" f.cff_pos;
			if fu.f_expr <> None then error "MultiType constructors cannot have a body" f.cff_pos;
			f.cff_access <- (AExtern,null_pos) :: f.cff_access;
		end;
		(try
			let _, p = List.find (fun (acc, _) -> acc = AMacro) f.cff_access in
			error "Macro abstract constructors are not supported" p
		with Not_found -> ());
		(* We don't want the generated expression positions to shadow the real code. *)
		let p = { p with pmax = p.pmin } in
		let fu = {
			fu with
			f_expr = (match fu.f_expr with
			| None -> None
			| Some (EBlock el,_) -> Some (EBlock (init p :: el @ [ret p]),p)
			| Some e -> Some (EBlock [init p;e;ret p],p)
			);
			f_type = Some a_t;
		} in
		{ f with cff_name = "_new",pos f.cff_name; cff_kind = FFun fu; cff_meta = meta }
	| FFun fu when not stat ->
		if Meta.has Meta.From f.cff_meta then error "@:from cast functions must be static" f.cff_pos;
		{ f with cff_kind = FFun fu }
	| _ ->
		f

let patch_class ctx c fields =
	let path = match c.cl_kind with
		| KAbstractImpl a -> a.a_path
		| _ -> c.cl_path
	in
	let h = (try Some (Hashtbl.find ctx.g.type_patches path) with Not_found -> None) in
	match h with
	| None -> fields
	| Some (h,hcl) ->
		c.cl_meta <- c.cl_meta @ hcl.tp_meta;
		let patch_getter t fn =
			{ fn with f_type = t }
		in
		let patch_setter t fn =
			match fn.f_args with
			| [(name,opt,meta,_,expr)] ->
				{ fn with f_args = [(name,opt,meta,t,expr)]; f_type = t }
			| _ -> fn
		in
		let rec loop acc accessor_acc = function
			| [] -> acc, accessor_acc
			| f :: l ->
				(* patch arguments types *)
				(match f.cff_kind with
				| FFun ff ->
					let param (((n,pn),opt,m,_,e) as p) =
						try
							let t2 = (try Hashtbl.find h (("$" ^ (fst f.cff_name) ^ "__" ^ n),false) with Not_found -> Hashtbl.find h (("$" ^ n),false)) in
							(n,pn), opt, m, (match t2.tp_type with None -> None | Some t -> Some (t,null_pos)), e
						with Not_found ->
							p
					in
					f.cff_kind <- FFun { ff with f_args = List.map param ff.f_args }
				| _ -> ());
				(* other patches *)
				match (try Some (Hashtbl.find h (fst f.cff_name,List.mem_assoc AStatic f.cff_access)) with Not_found -> None) with
				| None -> loop (f :: acc) accessor_acc l
				| Some { tp_remove = true } -> loop acc accessor_acc l
				| Some p ->
					f.cff_meta <- f.cff_meta @ p.tp_meta;
					let accessor_acc =
						match p.tp_type with
						| None -> accessor_acc
						| Some t ->
							match f.cff_kind with
							| FVar (_,e) ->
								f.cff_kind <- FVar (Some (t,null_pos),e); accessor_acc
							| FProp (get,set,_,eo) ->
								let typehint = Some (t,null_pos) in
								let accessor_acc = if fst get = "get" then ("get_" ^ fst f.cff_name, patch_getter typehint) :: accessor_acc else accessor_acc in
								let accessor_acc = if fst set = "set" then ("set_" ^ fst f.cff_name, patch_setter typehint) :: accessor_acc else accessor_acc in
								f.cff_kind <- FProp (get,set,typehint,eo); accessor_acc
							| FFun fn ->
								f.cff_kind <- FFun { fn with f_type = Some (t,null_pos) }; accessor_acc
					in
					loop (f :: acc) accessor_acc l
		in
		let fields, accessor_patches = loop [] [] fields in
		List.iter (fun (accessor_name, patch) ->
			try
				let f_accessor = List.find (fun f -> fst f.cff_name = accessor_name) fields in
				match f_accessor.cff_kind with
				| FFun fn -> f_accessor.cff_kind <- FFun (patch fn)
				| _ -> ()
			with Not_found ->
				()
		) accessor_patches;
		List.rev fields

let lazy_display_type ctx f =
	(* if ctx.is_display_file then begin
		let r = exc_protect ctx (fun r ->
			let t = f () in
			r := lazy_processing (fun () -> t);
			t
		) "" in
		TLazy r
	end else *)
		f ()

type enum_abstract_mode =
	| EAString
	| EAInt of int ref
	| EAOther

type enum_constructor_visibility =
	| VUnknown
	| VPublic of placed_access
	| VPrivate of placed_access

let build_enum_abstract ctx c a fields p =
	let mode =
		if does_unify a.a_this ctx.t.tint then EAInt (ref 0)
		else if does_unify a.a_this ctx.t.tstring then EAString
		else EAOther
	in
	List.iter (fun field ->
		match field.cff_kind with
		| FVar(ct,eo) when not (List.mem_assoc AStatic field.cff_access) ->
			let check_visibility_conflict visibility p1 =
				match visibility with
				| VUnknown ->
					()
				| VPublic(access,p2) | VPrivate(access,p2) ->
					display_error ctx (Printf.sprintf "Conflicting access modifier %s" (Ast.s_access access)) p1;
					display_error ctx "Conflicts with this" p2;
			in
			let rec loop visibility acc = match acc with
				| (AExtern,p) :: acc ->
					display_error ctx "extern modifier is not allowed on enum abstract fields" p;
					loop visibility acc
				| (APrivate,p) as access :: acc ->
					check_visibility_conflict visibility p;
					loop (VPrivate access) acc
				| (APublic,p) as access :: acc ->
					check_visibility_conflict visibility p;
					loop (VPublic access) acc
				| _ :: acc ->
					loop visibility acc
				| [] ->
					visibility
			in
			let visibility = loop VUnknown field.cff_access in
			field.cff_access <- [match visibility with VPublic acc | VPrivate acc -> acc | VUnknown -> (APublic,null_pos)];
			field.cff_meta <- (Meta.Enum,[],null_pos) :: field.cff_meta;
			let ct = match ct with
				| Some _ -> ct
				| None -> Some (TExprToExpr.convert_type (TAbstract(a,List.map snd a.a_params)),null_pos)
			in
			let set_field e =
				field.cff_access <- (AInline,null_pos) :: field.cff_access;
				let e = (ECast(e,None),(pos e)) in
				field.cff_kind <- FVar(ct,Some e)
			in
			begin match eo with
				| None ->
					if not (has_class_flag c CExtern) then begin match mode with
						| EAString ->
							set_field (EConst (String (fst field.cff_name,SDoubleQuotes)),null_pos)
						| EAInt i ->
							set_field (EConst (Int (string_of_int !i)),null_pos);
							incr i;
						| EAOther ->
							error "Value required" field.cff_pos
					end else field.cff_kind <- FProp(("default",null_pos),("never",null_pos),ct,None)
				| Some e ->
					begin match mode,e with
						| EAInt i,(EConst(Int s),_) ->
							begin try
								let i' = int_of_string s in
								i := (i' + 1)
							with _ ->
								()
							end
						| _ -> ()
					end;
					set_field e
			end
		| _ ->
			()
	) fields;
	EVars [mk_evar ~t:(CTAnonymous fields,p) ("",null_pos)],p

let apply_macro ctx mode path el p =
	let cpath, meth = (match List.rev (ExtString.String.nsplit path ".") with
		| meth :: name :: pack -> (List.rev pack,name), meth
		| _ -> error "Invalid macro path" p
	) in
	ctx.g.do_macro ctx mode cpath meth el p

let build_module_def ctx mt meta fvars context_init fbuild =
	let loop (f_build,f_enum) = function
		| Meta.Build,args,p -> (fun () ->
				let epath, el = (match args with
					| [ECall (epath,el),p] -> epath, el
					| _ -> error "Invalid build parameters" p
				) in
				let s = try String.concat "." (List.rev (string_list_of_expr_path epath)) with Error (_,p) -> error "Build call parameter must be a class path" p in
				if ctx.in_macro then error "You cannot use @:build inside a macro : make sure that your type is not used in macro" p;
				let old = ctx.get_build_infos in
				ctx.get_build_infos <- (fun() -> Some (mt, List.map snd (t_infos mt).mt_params, fvars()));
				context_init#run;
				let r = try apply_macro ctx MBuild s el p with e -> ctx.get_build_infos <- old; raise e in
				ctx.get_build_infos <- old;
				(match r with
				| None -> error "Build failure" p
				| Some e -> fbuild e)
			) :: f_build,f_enum
		| Meta.Enum,_,p -> f_build,Some (fun () ->
				begin match mt with
					| TClassDecl ({cl_kind = KAbstractImpl a} as c) ->
						(* if p <> null_pos && not (Define.is_haxe3_compat ctx.com.defines) then
							ctx.com.warning "`@:enum abstract` is deprecated in favor of `enum abstract`" p; *)
						context_init#run;
						let e = build_enum_abstract ctx c a (fvars()) p in
						fbuild e;
					| _ ->
						()
				end
			)
		| Meta.Using,el,p -> (fun () ->
			List.iter (fun e ->
				try
					let path = List.rev (string_pos_list_of_expr_path_raise e) in
					let types,filter_classes = handle_using ctx path (pos e) in
					let ti =
						match mt with
							| TClassDecl { cl_kind = KAbstractImpl a } -> t_infos (TAbstractDecl a)
							| _ -> t_infos mt
					in
					ti.mt_using <- (filter_classes types) @ ti.mt_using;
				with Exit ->
					error "dot path expected" (pos e)
			) el;
		) :: f_build,f_enum
		| _ ->
			f_build,f_enum
	in
	(* let errors go through to prevent resume if build fails *)
	let f_build,f_enum = List.fold_left loop ([],None) meta in
	(* Go for @:using in parents and interfaces *)
	(match mt with
		| TClassDecl { cl_super = csup; cl_implements = interfaces; cl_kind = kind } ->
			let ti = t_infos mt in
			let inherit_using (c,_) =
				ti.mt_using <- ti.mt_using @ (t_infos (TClassDecl c)).mt_using
			in
			Option.may inherit_using csup;
			List.iter inherit_using interfaces
		| _ -> ()
	);
	List.iter (fun f -> f()) (List.rev f_build);
	(match f_enum with None -> () | Some f -> f())

let create_class_context ctx c context_init p =
	locate_macro_error := true;
	incr stats.s_classes_built;
	let abstract = match c.cl_kind with
		| KAbstractImpl a -> Some a
		| _ -> None
	in
	let ctx = {
		ctx with
		curclass = c;
		type_params = c.cl_params;
		pass = PBuildClass;
		tthis = (match abstract with
			| Some a ->
				(match a.a_this with
				| TMono r when r.tm_type = None -> TAbstract (a,List.map snd c.cl_params)
				| t -> t)
			| None -> TInst (c,List.map snd c.cl_params));
		on_error = (fun ctx msg ep ->
			ctx.com.error msg ep;
			(* macros expressions might reference other code, let's recall which class we are actually compiling *)
			let open TFunctions in
			if not (ExtString.String.starts_with msg "...") && !locate_macro_error && (is_pos_outside_class c ep) && not (is_module_fields_class c) then ctx.com.error (compl_msg "Defined in this class") c.cl_pos
		);
	} in
	(* a lib type will skip most checks *)
	let is_lib = Meta.has Meta.LibType c.cl_meta in
	if is_lib && not (has_class_flag c CExtern) then ctx.com.error "@:libType can only be used in extern classes" c.cl_pos;
	(* a native type will skip one check: the static vs non-static field *)
	let is_native = Meta.has Meta.JavaNative c.cl_meta || Meta.has Meta.CsNative c.cl_meta in
	if Meta.has Meta.Macro c.cl_meta then display_error ctx "Macro classes are no longer allowed in haxe 3" c.cl_pos;
	let rec extends_public c =
		Meta.has Meta.PublicFields c.cl_meta ||
		match c.cl_super with
		| None -> false
		| Some (c,_) -> extends_public c
	in
	let cctx = {
		tclass = c;
		is_lib = is_lib;
		is_native = is_native;
		is_core_api = Meta.has Meta.CoreApi c.cl_meta;
		is_class_debug = Meta.has (Meta.Custom ":debug.typeload") c.cl_meta;
		extends_public = extends_public c;
		abstract = abstract;
		context_init = context_init;
		force_constructor = false;
		uninitialized_final = None;
		delayed_expr = [];
		has_display_field = false;
	} in
	ctx,cctx

let create_field_context (ctx,cctx) c cff =
	DeprecationCheck.check_is ctx.com (fst cff.cff_name) cff.cff_meta (snd cff.cff_name);
	let ctx = {
		ctx with
		pass = PBuildClass; (* will be set later to PTypeExpr *)
		locals = PMap.empty;
		opened = [];
		monomorphs = {
			perfunction = [];
		};
	} in
	let display_modifier = Typeload.check_field_access ctx cff in
	let is_static = List.mem_assoc AStatic cff.cff_access in
	let is_static,is_abstract_member = if cctx.abstract <> None && not is_static then true,true else is_static,false in
	let is_extern = ref (List.mem_assoc AExtern cff.cff_access) in
	let is_abstract = List.mem_assoc AAbstract cff.cff_access in
	let is_final = ref (List.mem_assoc AFinal cff.cff_access) in
	List.iter (fun (m,_,p) ->
		match m with
		| Meta.Final ->
			is_final := true;
			(* if p <> null_pos && not (Define.is_haxe3_compat ctx.com.defines) then
				ctx.com.warning "`@:final` is deprecated in favor of `final`" p; *)
		| Meta.Extern ->
			(* if not (Define.is_haxe3_compat ctx.com.defines) then
				ctx.com.warning "`@:extern` on fields is deprecated in favor of `extern`" (pos cff.cff_name); *)
			is_extern := true;
		| _ ->
			()
	) cff.cff_meta;
	let is_inline = List.mem_assoc AInline cff.cff_access in
	if is_abstract then begin
		if is_static then
			display_error ctx "Static methods may not be abstract" (pos cff.cff_name)
		else if !is_final then
			display_error ctx "Abstract methods may not be final" (pos cff.cff_name)
		else if is_inline then
			display_error ctx "Abstract methods may not be inline" (pos cff.cff_name)
		else if not (has_class_flag c CAbstract) then begin
			display_error ctx "This class should be declared abstract because it has at least one abstract field" c.cl_name_pos;
			display_error ctx "First abstract field was here" (pos cff.cff_name);
			add_class_flag c CAbstract;
		end;
	end;
	let override = try Some (List.assoc AOverride cff.cff_access) with Not_found -> None in
	let overload = try Some (List.assoc AOverload cff.cff_access) with Not_found -> None in
	let is_macro = List.mem_assoc AMacro cff.cff_access in
	let field_kind = match fst cff.cff_name with
		| "new" -> FKConstructor
		| "__init__" when is_static -> FKInit
		| _ -> FKNormal
	in
	let fctx = {
		is_inline = is_inline;
		is_static = is_static;
		override = override;
		overload = overload;
		is_macro = is_macro;
		is_extern = !is_extern;
		is_abstract = is_abstract;
		is_final = !is_final;
		is_display_field = ctx.is_display_file && DisplayPosition.display_position#enclosed_in cff.cff_pos;
		is_field_debug = cctx.is_class_debug || Meta.has (Meta.Custom ":debug.typeload") cff.cff_meta;
		display_modifier = display_modifier;
		is_abstract_member = is_abstract_member;
		field_kind = field_kind;
		do_bind = (((not ((has_class_flag c CExtern) || !is_extern) || is_inline) && not is_abstract && not (has_class_flag c CInterface)) || field_kind = FKInit);
		do_add = true;
		expr_presence_matters = false;
	} in
	if fctx.is_display_field then cctx.has_display_field <- true;
	ctx,fctx

let is_public (ctx,cctx) access parent =
	let c = cctx.tclass in
	if List.mem_assoc APrivate access then
		false
	else if List.mem_assoc APublic access then
		true
	else match parent with
		| Some cf -> (has_class_field_flag cf CfPublic)
		| _ -> (has_class_flag c CExtern) || (has_class_flag c CInterface) || cctx.extends_public || (match c.cl_kind with KModuleFields _ -> true | _ -> false)

let rec get_parent c name =
	match c.cl_super with
	| None -> None
	| Some (csup,_) ->
		try
			Some (PMap.find name csup.cl_fields)
		with
			Not_found -> get_parent csup name

let type_opt (ctx,cctx) p t =
	let c = cctx.tclass in
	match t with
	| None when (has_class_flag c CExtern) || (has_class_flag c CInterface) ->
		display_error ctx "Type required for extern classes and interfaces" p;
		t_dynamic
	| None when cctx.is_core_api ->
		display_error ctx "Type required for core api classes" p;
		t_dynamic
	| _ ->
		load_type_hint ctx p t

let transform_field (ctx,cctx) c f fields p =
	let f = match cctx.abstract with
		| Some a ->
			let a_t = TExprToExpr.convert_type' (TAbstract(a,List.map snd a.a_params)) in
			let this_t = TExprToExpr.convert_type' a.a_this in (* TODO: better pos? *)
			transform_abstract_field ctx.com this_t a_t a f
		| None ->
			f
	in
	if List.mem_assoc AMacro f.cff_access then
		(match ctx.g.macros with
		| Some (_,mctx) when Hashtbl.mem mctx.g.types_module c.cl_path ->
			(* assume that if we had already a macro with the same name, it has not been changed during the @:build operation *)
			if not (List.exists (fun f2 -> f2.cff_name = f.cff_name && List.mem_assoc AMacro f2.cff_access) (!fields)) then
				error "Class build macro cannot return a macro function when the class has already been compiled into the macro context" p
		| _ -> ());
	f

let build_fields (ctx,cctx) c fields =
	let fields = ref fields in
	let get_fields() = !fields in
	let pending = ref [] in
	c.cl_build <- (fun() -> BuildMacro pending);
	build_module_def ctx (TClassDecl c) c.cl_meta get_fields cctx.context_init (fun (e,p) ->
		match e with
		| EVars [{ ev_type = Some (CTAnonymous f,p); ev_expr = None }] ->
			let f = List.map (fun f -> transform_field (ctx,cctx) c f fields p) f in
			fields := f
		| _ -> error "Class build macro must return a single variable with anonymous fields" p
	);
	c.cl_build <- (fun() -> Building [c]);
	List.iter (fun f -> f()) !pending;
	!fields

let bind_type (ctx,cctx,fctx) cf r p =
	let c = cctx.tclass in
	let rec is_full_type t =
		match t with
		| TFun (args,ret) -> is_full_type ret && List.for_all (fun (_,_,t) -> is_full_type t) args
		| TMono r -> (match r.tm_type with None -> false | Some t -> is_full_type t)
		| TAbstract _ | TInst _ | TEnum _ | TLazy _ | TDynamic _ | TAnon _ | TType _ -> true
	in
	let force_macro () =
		(* force macro system loading of this class in order to get completion *)
		delay ctx PTypeField (fun() -> try ignore(ctx.g.do_macro ctx MDisplay c.cl_path cf.cf_name [] p) with Exit | Error _ -> ())
	in
	let handle_display_field () =
		if fctx.is_macro && not ctx.in_macro then
			force_macro()
		else begin
			cf.cf_type <- TLazy r;
			cctx.delayed_expr <- (ctx,Some r) :: cctx.delayed_expr;
		end
	in
	if ctx.com.display.dms_full_typing then begin
		if fctx.is_macro && not ctx.in_macro then
			force_macro ()
		else begin
			cf.cf_type <- TLazy r;
			(* is_lib ? *)
			cctx.delayed_expr <- (ctx,Some r) :: cctx.delayed_expr;
		end
	end else if ctx.com.display.dms_force_macro_typing && fctx.is_macro && not ctx.in_macro then
		force_macro()
	else begin
		if fctx.is_display_field then begin
			handle_display_field()
		end else begin
			if not (is_full_type cf.cf_type) then begin
				cctx.delayed_expr <- (ctx, None) :: cctx.delayed_expr;
				cf.cf_type <- TLazy r;
			end else if fctx.expr_presence_matters then
				cf.cf_expr <- Some (mk (TConst TNull) t_dynamic null_pos)
		end
	end

let check_field_display ctx fctx c cf =
	if fctx.is_display_field then begin
		let scope, cf = match c.cl_kind with
			| KAbstractImpl _ ->
				if has_class_field_flag cf CfImpl then
					(if cf.cf_name = "_new" then
						CFSConstructor, {cf with cf_name = "new"}
					else
						CFSMember, cf)
				else
					CFSStatic, cf;
			| _ ->
				(if fctx.is_static then
					CFSStatic
				else if fctx.field_kind = FKConstructor then
					CFSConstructor
				else
					CFSMember), cf;
		in
		let origin = match c.cl_kind with
			| KAbstractImpl a -> Self (TAbstractDecl a)
			| _ -> Self (TClassDecl c)
		in
		DisplayEmitter.maybe_display_field ctx origin scope cf cf.cf_name_pos;
		DisplayEmitter.check_field_modifiers ctx c cf fctx.override fctx.display_modifier;
	end

let bind_var (ctx,cctx,fctx) cf e =
	let c = cctx.tclass in
	let p = cf.cf_pos in
	let rec get_declared f = function
		| None -> None
		| Some (c,a) when PMap.exists f c.cl_fields ->
			Some (c,a)
		| Some (c,_) ->
			let ret = get_declared f c.cl_super in
			match ret with
				| Some r -> Some r
				| None ->
					let rec loop ifaces = match ifaces with
						| [] -> None
						| i :: ifaces -> match get_declared f (Some i) with
							| Some r -> Some r
							| None -> loop ifaces
					in
					loop c.cl_implements
	in
	if not fctx.is_static && not cctx.is_lib then begin match get_declared cf.cf_name c.cl_super with
			| None -> ()
			| Some (csup,_) ->
				(* this can happen on -net-lib generated classes if a combination of explicit interfaces and variables with the same name happens *)
				if not ((has_class_flag csup CInterface) && Meta.has Meta.CsNative c.cl_meta) then
					error ("Redefinition of variable " ^ cf.cf_name ^ " in subclass is not allowed. Previously declared at " ^ (s_type_path csup.cl_path) ) p
	end;
	let t = cf.cf_type in

	match e with
	| None ->
		check_field_display ctx fctx c cf;
	| Some e ->
		if (has_class_flag c CInterface) then display_error ctx "Default values on interfaces are not allowed" (pos e);
		cf.cf_meta <- ((Meta.Value,[e],null_pos) :: cf.cf_meta);
		let check_cast e =
			(* insert cast to keep explicit field type (issue #1901) *)
			if type_iseq e.etype cf.cf_type then
				e
			else begin match e.eexpr,follow cf.cf_type with
				| TConst (TInt i),TAbstract({a_path=[],"Float"},_) ->
					(* turn int constant to float constant if expected type is float *)
					{e with eexpr = TConst (TFloat (Int32.to_string i)); etype = cf.cf_type}
				| _ ->
					mk_cast e cf.cf_type e.epos
			end
		in
		let r = exc_protect ~force:false ctx (fun r ->
			(* type constant init fields (issue #1956) *)
			if not !return_partial_type || (match fst e with EConst _ -> true | _ -> false) then begin
				r := lazy_processing (fun() -> t);
				cctx.context_init#run;
				if ctx.com.verbose then Common.log ctx.com ("Typing " ^ (if ctx.in_macro then "macro " else "") ^ s_type_path c.cl_path ^ "." ^ cf.cf_name);
				let e = TypeloadFunction.type_var_field ctx t e fctx.is_static fctx.is_display_field p in
				let maybe_run_analyzer e = match e.eexpr with
					| TConst _ | TLocal _ | TFunction _ -> e
					| _ -> !analyzer_run_on_expr_ref ctx.com e
				in
				let require_constant_expression e msg =
					if ctx.com.display.dms_kind <> DMNone && ctx.com.display.dms_error_policy <> EPCollect then
						e
					else match Optimizer.make_constant_expression ctx (maybe_run_analyzer e) with
					| Some e -> e
					| None -> display_error ctx msg p; e
				in
				let e = (match cf.cf_kind with
				| Var v when (has_class_flag c CExtern) || fctx.is_extern ->
					if not fctx.is_static then begin
						display_error ctx "Extern non-static variables may not be initialized" p;
						e
					end else if not fctx.is_inline then begin
						display_error ctx "Extern non-inline variables may not be initialized" p;
						e
					end else require_constant_expression e "Extern variable initialization must be a constant value"
				| Var v when not (is_physical_field cf) ->
					(* disallow initialization of non-physical fields (issue #1958) *)
					display_error ctx "This field cannot be initialized because it is not a real variable" p; e
				| Var v when not fctx.is_static ->
					let e = if ctx.com.display.dms_display && ctx.com.display.dms_error_policy <> EPCollect then
						e
					else begin
						let e = Optimizer.reduce_loop ctx (maybe_run_analyzer e) in
						let e = (match Optimizer.make_constant_expression ctx e with
							| Some e -> e
							| None -> e
						) in
						let rec check_this e = match e.eexpr with
							| TConst TThis ->
								display_error ctx "Cannot access this or other member field in variable initialization" e.epos;
								raise Exit
							| TLocal v when (match ctx.vthis with Some v2 -> v == v2 | None -> false) ->
								display_error ctx "Cannot access this or other member field in variable initialization" e.epos;
								raise Exit
							| _ ->
							Type.iter check_this e
						in
						(try check_this e with Exit -> ());
						e
					end in
					e
				| Var v when v.v_read = AccInline ->
					let e = require_constant_expression e "Inline variable initialization must be a constant value" in
					begin match c.cl_kind with
						| KAbstractImpl a when Meta.has Meta.Enum cf.cf_meta && Meta.has Meta.Enum a.a_meta ->
							unify ctx t (TAbstract(a,(Monomorph.spawn_constrained_monos (fun t -> t) a.a_params))) p;
							let e1 = match e.eexpr with TCast(e1,None) -> e1 | _ -> e in
							unify ctx e1.etype a.a_this e1.epos
						| _ ->
							()
					end;
					e
				| _ ->
					e
				) in
				let e = check_cast e in
				cf.cf_expr <- Some e;
				cf.cf_type <- t;
				check_field_display ctx fctx c cf;
			end;
			t
		) "bind_var" in
		if not fctx.is_static then cctx.force_constructor <- true;
		bind_type (ctx,cctx,fctx) cf r (snd e)

let create_variable (ctx,cctx,fctx) c f t eo p =
	if not fctx.is_static && cctx.abstract <> None then error (fst f.cff_name ^ ": Cannot declare member variable in abstract") p;
	if fctx.is_inline && not fctx.is_static then error (fst f.cff_name ^ ": Inline variable must be static") p;
	if fctx.is_inline && eo = None then error (fst f.cff_name ^ ": Inline variable must be initialized") p;
	if fctx.is_final && not (fctx.is_extern || (has_class_flag c CExtern) || (has_class_flag c CInterface))  && eo = None then begin
		if fctx.is_static then error (fst f.cff_name ^ ": Static final variable must be initialized") p
		else cctx.uninitialized_final <- Some f.cff_pos;
	end;
	let t = (match t with
		| None when eo = None ->
			error ("Variable requires type-hint or initialization") (pos f.cff_name);
		| None ->
			mk_mono()
		| Some t ->
			lazy_display_type ctx (fun () -> load_type_hint ctx p (Some t))
	) in
	let kind = if fctx.is_inline then
		{ v_read = AccInline ; v_write = AccNever }
	else if fctx.is_final then
		{ v_read = AccNormal ; v_write = if fctx.is_static then AccNever else AccCtor }
	else
		{ v_read = AccNormal ; v_write = AccNormal }
	in
	let cf = {
		(mk_field (fst f.cff_name) ~public:(is_public (ctx,cctx) f.cff_access None) t f.cff_pos (pos f.cff_name)) with
		cf_doc = f.cff_doc;
		cf_meta = f.cff_meta;
		cf_kind = Var kind;
	} in
	if fctx.is_final then add_class_field_flag cf CfFinal;
	if fctx.is_extern then add_class_field_flag cf CfExtern;
	if fctx.is_abstract_member then add_class_field_flag cf CfImpl;
	ctx.curfield <- cf;
	bind_var (ctx,cctx,fctx) cf eo;
	cf

let check_abstract (ctx,cctx,fctx) c cf fd t ret p =
	match cctx.abstract with
		| Some a ->
			let m = mk_mono() in
			let ta = TAbstract(a,List.map (fun _ -> mk_mono()) a.a_params) in
			let tthis = if fctx.is_abstract_member || Meta.has Meta.To cf.cf_meta then monomorphs a.a_params a.a_this else a.a_this in
			let allows_no_expr = ref (Meta.has Meta.CoreType a.a_meta) in
			let rec loop ml =
				(match ml with
				| (Meta.From,_,_) :: _ ->
					let r = exc_protect ctx (fun r ->
						r := lazy_processing (fun () -> t);
						(* the return type of a from-function must be the abstract, not the underlying type *)
						if not fctx.is_macro then (try type_eq EqStrict ret ta with Unify_error l -> error (error_msg (Unify l)) p);
						match t with
							| TFun([_,_,t],_) -> t
							| _ -> error (cf.cf_name ^ ": @:from cast functions must accept exactly one argument") p
					) "@:from" in
					a.a_from_field <- (TLazy r,cf) :: a.a_from_field;
				| (Meta.To,_,_) :: _ ->
					if fctx.is_macro then error (cf.cf_name ^ ": Macro cast functions are not supported") p;
					(match cf.cf_kind, cf.cf_type with
					| Var _, _ ->
						error "@:to meta should be used on methods" p
					| Method _, TFun(args, _) when not fctx.is_abstract_member && List.length args <> 1 ->
						if not (Meta.has Meta.MultiType a.a_meta) then (* TODO: get rid of this check once multitype is removed *)
						error ("static @:to method should have one argument") p
					| Method _, TFun(args, _) when fctx.is_abstract_member && List.length args <> 1 ->
						if not (Meta.has Meta.MultiType a.a_meta) then (* TODO: get rid of this check once multitype is removed *)
						error "@:to method should have no arguments" p
					| _ -> ()
					);
					(* TODO: this doesn't seem quite right... *)
					if not (has_class_field_flag cf CfImpl) then add_class_field_flag cf CfImpl;
					let resolve_m args =
						(try unify_raise ctx t (tfun (tthis :: args) m) cf.cf_pos with Error (Unify l,p) -> error (error_msg (Unify l)) p);
						match follow m with
							| TMono _ when (match t with TFun(_,r) -> r == t_dynamic | _ -> false) -> t_dynamic
							| m -> m
					in
					let r = exc_protect ctx (fun r ->
						r := lazy_processing (fun () -> t);
						let args = if Meta.has Meta.MultiType a.a_meta then begin
							let ctor = try
								PMap.find "_new" c.cl_statics
							with Not_found ->
								error "Constructor of multi-type abstract must be defined before the individual @:to-functions are" cf.cf_pos
							in
							(* delay ctx PFinal (fun () -> unify ctx m tthis f.cff_pos); *)
							let args = match follow (monomorphs a.a_params ctor.cf_type) with
								| TFun(args,_) -> List.map (fun (_,_,t) -> t) args
								| _ -> die "" __LOC__
							in
							args
						end else
							[]
						in
						let t = resolve_m args in
						t
					) "@:to" in
					a.a_to_field <- (TLazy r, cf) :: a.a_to_field
				| ((Meta.ArrayAccess,_,_) | (Meta.Op,[(EArrayDecl _),_],_)) :: _ ->
					if fctx.is_macro then error (cf.cf_name ^ ": Macro array-access functions are not supported") p;
					a.a_array <- cf :: a.a_array;
					fctx.expr_presence_matters <- true;
				| (Meta.Op,[EBinop(OpAssign,_,_),_],_) :: _ ->
					error (cf.cf_name ^ ": Assignment overloading is not supported") p;
				| (Meta.Op,[ETernary(_,_,_),_],_) :: _ ->
					error (cf.cf_name ^ ": Ternary overloading is not supported") p;
				| (Meta.Op,[EBinop(op,_,_),_],_) :: _ ->
					if fctx.is_macro then error (cf.cf_name ^ ": Macro operator functions are not supported") p;
					let targ = if fctx.is_abstract_member then tthis else ta in
					let left_eq,right_eq = match follow t with
						| TFun([(_,_,t1);(_,_,t2)],_) ->
							type_iseq targ t1,type_iseq targ t2
						| _ ->
							if fctx.is_abstract_member then
								error (cf.cf_name ^ ": Member @:op functions must accept exactly one argument") cf.cf_pos
							else
								error (cf.cf_name ^ ": Static @:op functions must accept exactly two arguments") cf.cf_pos
					in
					if not (left_eq || right_eq) then error (cf.cf_name ^ ": The left or right argument type must be " ^ (s_type (print_context()) targ)) cf.cf_pos;
					if right_eq && Meta.has Meta.Commutative cf.cf_meta then error (cf.cf_name ^ ": @:commutative is only allowed if the right argument is not " ^ (s_type (print_context()) targ)) cf.cf_pos;
					a.a_ops <- (op,cf) :: a.a_ops;
					allows_no_expr := true;
					fctx.expr_presence_matters <- true;
				| (Meta.Op,[EUnop(op,flag,_),_],_) :: _ ->
					if fctx.is_macro then error (cf.cf_name ^ ": Macro operator functions are not supported") p;
					let targ = if fctx.is_abstract_member then tthis else ta in
					(try type_eq EqStrict t (tfun [targ] (mk_mono())) with Unify_error l -> raise (Error ((Unify l),cf.cf_pos)));
					a.a_unops <- (op,flag,cf) :: a.a_unops;
					allows_no_expr := true;
					fctx.expr_presence_matters <- true;
				| ((Meta.Resolve,_,_) | (Meta.Op,[EField _,_],_)) :: _ ->
					let targ = if fctx.is_abstract_member then tthis else ta in
					let check_fun t1 t2 =
						if not fctx.is_macro then begin
							if not (type_iseq targ t1) then error ("First argument type must be " ^ (s_type (print_context()) targ)) cf.cf_pos;
							if not (type_iseq ctx.t.tstring t2) then error ("Second argument type must be String") cf.cf_pos
						end
					in
					begin match follow t with
						| TFun([(_,_,t1);(_,_,t2)],_) ->
							if a.a_read <> None then error "Multiple resolve-read methods are not supported" cf.cf_pos;
							check_fun t1 t2;
							a.a_read <- Some cf;
						| TFun([(_,_,t1);(_,_,t2);(_,_,t3)],_) ->
							if a.a_write <> None then error "Multiple resolve-write methods are not supported" cf.cf_pos;
							check_fun t1 t2;
							a.a_write <- Some cf;
						| _ ->
							error ("Field type of resolve must be " ^ (s_type (print_context()) targ) ^ " -> String -> T") cf.cf_pos
					end;
				| _ -> ());
				match ml with
				| _ :: ml -> loop ml
				| [] -> ()
			in
			loop cf.cf_meta;
			let check_bind () =
				if fd.f_expr = None then begin
					if fctx.is_inline then error (cf.cf_name ^ ": Inline functions must have an expression") cf.cf_pos;
					begin match fd.f_type with
						| None -> error (cf.cf_name ^ ": Functions without expressions must have an explicit return type") cf.cf_pos
						| Some _ -> ()
					end;
					cf.cf_meta <- (Meta.NoExpr,[],null_pos) :: cf.cf_meta;
					fctx.do_bind <- false;
					if not (Meta.has Meta.CoreType a.a_meta) then fctx.do_add <- false;
				end
			in
			if cf.cf_name = "_new" && Meta.has Meta.MultiType a.a_meta then fctx.do_bind <- false;
			if !allows_no_expr then check_bind()
		| _ ->
			()

let create_method (ctx,cctx,fctx) c f fd p =
	let params = TypeloadFunction.type_function_params ctx fd (fst f.cff_name) p in
	if Meta.has Meta.Generic f.cff_meta then begin
		if params = [] then error (fst f.cff_name ^ ": Generic functions must have type parameters") p;
	end;
	let fd = if fctx.is_macro && not ctx.in_macro && not fctx.is_static then
		(* remove display of first argument which will contain the "this" expression *)
		{ fd with f_args = match fd.f_args with [] -> [] | _ :: l -> l }
	else
		fd
	in
	let fd = if not fctx.is_macro then
		fd
	else begin
		if ctx.in_macro then begin
			(* a class with a macro cannot be extern in macro context (issue #2015) *)
			remove_class_flag c CExtern;
			let texpr = CTPath (mk_type_path (["haxe";"macro"],"Expr")) in
			(* ExprOf type parameter might contain platform-specific type, let's replace it by Expr *)
			let no_expr_of (t,p) = match t with
				| CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some ("ExprOf"); tparams = [TPType _] }
				| CTPath { tpackage = []; tname = ("ExprOf"); tsub = None; tparams = [TPType _] } -> Some (texpr,p)
				| t -> Some (t,p)
			in
			{
				f_params = fd.f_params;
				f_type = (match fd.f_type with None -> Some (texpr,null_pos) | Some t -> no_expr_of t);
				f_args = List.map (fun (a,o,m,t,e) -> a,o,m,(match t with None -> Some (texpr,null_pos) | Some t -> no_expr_of t),e) fd.f_args;
				f_expr = fd.f_expr;
			}
		end else
			let tdyn = Some (CTPath (mk_type_path ([],"Dynamic")),null_pos) in
			let to_dyn p t = match t with
				| { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some ("ExprOf"); tparams = [TPType t] } -> Some t
				| { tpackage = []; tname = ("ExprOf"); tsub = None; tparams = [TPType t] } -> Some t
				| { tpackage = ["haxe"]; tname = ("PosInfos"); tsub = None; tparams = [] } -> error "haxe.PosInfos is not allowed on macro functions, use Context.currentPos() instead" p
				| _ -> tdyn
			in
			{
				f_params = fd.f_params;
				f_type = (match fd.f_type with Some (CTPath t,p) -> to_dyn p t | _ -> tdyn);
				f_args = List.map (fun (a,o,m,t,_) -> a,o,m,(match t with Some (CTPath t,p) -> to_dyn p t | _ -> tdyn),None) fd.f_args;
				f_expr = None;
			}
	end in
	begin match (has_class_flag c CInterface),fctx.field_kind with
		| true,FKConstructor ->
			error "An interface cannot have a constructor" p;
		| true,_ ->
			if not fctx.is_static && fd.f_expr <> None then error (fst f.cff_name ^ ": An interface method cannot have a body") p;
			if fctx.is_inline && (has_class_flag c CInterface) then error (fst f.cff_name ^ ": You can't declare inline methods in interfaces") p;
		| false,FKConstructor ->
			if fctx.is_static then error "A constructor must not be static" p;
			begin match fd.f_type with
				| None -> ()
				| Some (CTPath ({ tpackage = []; tname = "Void" } as tp),p) ->
					if ctx.is_display_file && DisplayPosition.display_position#enclosed_in p then
						ignore(load_instance ~allow_display:true ctx (tp,p) false);
				| _ -> error "A class constructor can't have a return value" p;
			end
		| false,_ ->
			()
	end;
	let parent = (if not fctx.is_static then get_parent c (fst f.cff_name) else None) in
	let dynamic = List.mem_assoc ADynamic f.cff_access || (match parent with Some { cf_kind = Method MethDynamic } -> true | _ -> false) in
	if fctx.is_abstract && dynamic then display_error ctx "Abstract methods may not be dynamic" p;
	if fctx.is_inline && dynamic then error (fst f.cff_name ^ ": 'inline' is not allowed on 'dynamic' functions") p;
	let is_override = Option.is_some fctx.override in
	if (is_override && fctx.is_static) then error (fst f.cff_name ^ ": 'override' is not allowed on 'static' functions") p;

	ctx.type_params <- if fctx.is_static && not fctx.is_abstract_member then params else params @ ctx.type_params;
	(* TODO is_lib: avoid forcing the return type to be typed *)
	let ret = if fctx.field_kind = FKConstructor then ctx.t.tvoid else type_opt (ctx,cctx) p fd.f_type in
	let rec loop args = match args with
		| ((name,p),opt,m,t,ct) :: args ->
			(* TODO is_lib: avoid forcing the field to be typed *)
			let t, ct = TypeloadFunction.type_function_arg ctx (type_opt (ctx,cctx) p t) ct opt p in
			delay ctx PTypeField (fun() -> match follow t with
				| TAbstract({a_path = ["haxe";"extern"],"Rest"},_) ->
					if not fctx.is_extern && not (has_class_flag c CExtern) then error "Rest argument are only supported for extern methods" p;
					if opt then error "Rest argument cannot be optional" p;
					begin match ct with None -> () | Some (_,p) -> error "Rest argument cannot have default value" p end;
					if args <> [] then error "Rest should only be used for the last function argument" p;
				| _ ->
					()
			);
			(name, ct, t) :: (loop args)
		| [] ->
			[]
	in
	let args = loop fd.f_args in
	let fargs = TypeloadFunction.convert_fargs fd in
	let args,fargs = match cctx.abstract with
		| Some a when fctx.is_abstract_member && fst f.cff_name <> "_new" (* TODO: this sucks *) && not fctx.is_macro ->
			("this",None,a.a_this) :: args,(null_pos,[]) :: fargs
		| _ ->
			args,fargs
	in
	let t = TFun (fun_args args,ret) in
	let cf = {
		(mk_field (fst f.cff_name) ~public:(is_public (ctx,cctx) f.cff_access parent) t f.cff_pos (pos f.cff_name)) with
		cf_doc = f.cff_doc;
		cf_meta = f.cff_meta;
		cf_kind = Method (if fctx.is_macro then MethMacro else if fctx.is_inline then MethInline else if dynamic then MethDynamic else MethNormal);
		cf_params = params;
	} in
	if fctx.is_final then add_class_field_flag cf CfFinal;
	if fctx.is_extern then add_class_field_flag cf CfExtern;
	if fctx.is_abstract then add_class_field_flag cf CfAbstract;
	if fctx.is_abstract_member then add_class_field_flag cf CfImpl;
	begin match fctx.overload with
	| Some p ->
		if ctx.com.config.pf_overload then
			add_class_field_flag cf CfOverload
		else
			display_error ctx "This platform does not support this kind of overload declaration. Try @:overload(function()... {}) instead" p
	| None ->
		()
	end;
	cf.cf_meta <- List.map (fun (m,el,p) -> match m,el with
		| Meta.AstSource,[] -> (m,(match fd.f_expr with None -> [] | Some e -> [e]),p)
		| _ -> m,el,p
	) cf.cf_meta;
	Option.may (fun cf_parent ->
		if not (Meta.has Meta.Native cf.cf_meta) then
			try
				let native_meta = Meta.get Meta.Native cf_parent.cf_meta in
				cf.cf_meta <- native_meta :: cf.cf_meta;
			with Not_found ->
				()
	) parent;
	generate_args_meta ctx.com (Some c) (fun meta -> cf.cf_meta <- meta :: cf.cf_meta) fd.f_args;
	check_abstract (ctx,cctx,fctx) c cf fd t ret p;
	init_meta_overloads ctx (Some c) cf;
	ctx.curfield <- cf;
	let r = exc_protect ~force:false ctx (fun r ->
		if not !return_partial_type then begin
			r := lazy_processing (fun() -> t);
			cctx.context_init#run;
			incr stats.s_methods_typed;
			if ctx.com.verbose then Common.log ctx.com ("Typing " ^ (if ctx.in_macro then "macro " else "") ^ s_type_path c.cl_path ^ "." ^ fst f.cff_name);
			let fmode = (match cctx.abstract with
				| Some _ ->
					(match args with
					| ("this",_,_) :: _ -> FunMemberAbstract
					| _ when fst f.cff_name = "_new" -> FunMemberAbstract
					| _ -> FunStatic)
				| None ->
					if fctx.field_kind = FKConstructor then FunConstructor else if fctx.is_static then FunStatic else FunMember
			) in
			begin match ctx.com.platform with
				| Java when is_java_native_function ctx cf.cf_meta cf.cf_pos ->
					if fd.f_expr <> None then
						ctx.com.warning "@:java.native function definitions shouldn't include an expression. This behaviour is deprecated." cf.cf_pos;
					cf.cf_expr <- None;
					cf.cf_type <- t
				| _ ->
					if Meta.has Meta.DisplayOverride cf.cf_meta then DisplayEmitter.check_field_modifiers ctx c cf fctx.override fctx.display_modifier;
					let e , fargs = TypeloadFunction.type_function ctx args fargs ret fmode fd.f_expr fctx.is_display_field p in
					begin match fctx.field_kind with
					| FKNormal when not fctx.is_static -> TypeloadCheck.check_overriding ctx c cf
					| _ -> ()
					end;
					(* Disabled for now, see https://github.com/HaxeFoundation/haxe/issues/3033 *)
					(* List.iter (fun (v,_) ->
						if v.v_name <> "_" && has_mono v.v_type then ctx.com.warning "Uninferred function argument, please add a type-hint" v.v_pos;
					) fargs; *)
					let tf = {
						tf_args = fargs;
						tf_type = ret;
						tf_expr = e;
					} in
					if fctx.field_kind = FKInit then
						(match e.eexpr with
						| TBlock [] | TBlock [{ eexpr = TConst _ }] | TConst _ | TObjectDecl [] -> ()
						| _ -> c.cl_init <- Some e);
					cf.cf_expr <- Some (mk (TFunction tf) t p);
					cf.cf_type <- t;
					check_field_display ctx fctx c cf;
			end;
		end;
		t
	) "type_fun" in
	if fctx.do_bind then
		bind_type (ctx,cctx,fctx) cf r (match fd.f_expr with Some e -> snd e | None -> f.cff_pos)
	else begin
		delay ctx PTypeField (fun () ->
			(* We never enter type_function so we're missing out on the argument processing there. Let's do it here. *)
			List.iter2 (fun (n,ct,t) (pn,m) ->
				(* dirty dodge to avoid flash extern problems until somebody fixes that *)
				begin if ctx.com.platform = Flash && (has_class_flag c CExtern) then
					()
				else
					ignore(TypeloadFunction.process_function_arg ctx n t ct fctx.is_display_field (not (has_class_flag c CExtern) && not fctx.is_extern) pn)
				end;
				if fctx.is_display_field && DisplayPosition.display_position#enclosed_in pn then begin
					let v = add_local_with_origin ctx TVOArgument n t pn in
					DisplayEmitter.display_variable ctx v pn;
				end
			) args fargs;
		);
		check_field_display ctx fctx c cf;
		if fd.f_expr <> None then begin
			if fctx.is_abstract then display_error ctx "Abstract methods may not have an expression" p
			else if not (fctx.is_inline || fctx.is_macro) then ctx.com.warning "Extern non-inline function may not have an expression" p;
		end;
	end;
	cf

let create_property (ctx,cctx,fctx) c f (get,set,t,eo) p =
	let name = fst f.cff_name in
	(* TODO is_lib: lazify load_complex_type *)
	let ret = (match t, eo with
		| None, None -> error (name ^ ": Property requires type-hint or initialization") p;
		| None, _ -> mk_mono()
		| Some t, _ -> lazy_display_type ctx (fun () -> load_type_hint ctx p (Some t))
	) in
	let t_get,t_set = match cctx.abstract with
		| Some a when fctx.is_abstract_member ->
			if Meta.has Meta.IsVar f.cff_meta then error (name ^ ": Abstract properties cannot be real variables") f.cff_pos;
			let ta = apply_params a.a_params (List.map snd a.a_params) a.a_this in
			tfun [ta] ret, tfun [ta;ret] ret
		| _ -> tfun [] ret, TFun(["value",false,ret],ret)
	in
	let find_accessor m =
		(* on pf_overload platforms, the getter/setter may have been defined as an overloaded function; get all overloads *)
		if ctx.com.config.pf_overload then
			if fctx.is_static then
				let f = PMap.find m c.cl_statics in
				(f.cf_type, f) :: (List.map (fun f -> f.cf_type, f) f.cf_overloads)
			else
				Overloads.get_overloads ctx.com c m
		else
			[ if fctx.is_static then
				let f = PMap.find m c.cl_statics in
				f.cf_type, f
			else match class_field c (List.map snd c.cl_params) m with
				| _, t,f -> t,f ]
	in
	let cf = {
		(mk_field name ~public:(is_public (ctx,cctx) f.cff_access None) ret f.cff_pos (pos f.cff_name)) with
		cf_doc = f.cff_doc;
		cf_meta = f.cff_meta;
	} in
	if fctx.is_abstract_member then add_class_field_flag cf CfImpl;
	let check_method m t is_getter =
		if ctx.com.display.dms_error_policy = EPIgnore then () else
		try
			let overloads = find_accessor m in
			(* choose the correct overload if and only if there is more than one overload found *)
			let rec get_overload overl = match overl with
				| [tf] -> tf
				| (t2,f2) :: overl ->
					if type_iseq t t2 then
						(t2,f2)
					else
						get_overload overl
				| [] ->
					if (has_class_flag c CInterface) || Diagnostics.is_diagnostics_run ctx.com f.cff_pos then
						raise Not_found
					else
						raise (Error (Custom
							(Printf.sprintf "No overloaded method named %s was compatible with the property %s with expected type %s" m (name) (s_type (print_context()) t)
						), p))
			in
			let t2, f2 = get_overload overloads in
			(match f2.cf_kind with
				| Method MethMacro ->
					display_error ctx (f2.cf_name ^ ": Macro methods cannot be used as property accessor") p;
					display_error ctx (compl_msg (f2.cf_name ^ ": Accessor method is here")) f2.cf_pos;
				| _ -> ());
			unify_raise ctx t2 t f2.cf_pos;
			if (fctx.is_abstract_member && not (has_class_field_flag f2 CfImpl)) || (has_class_field_flag f2 CfImpl && not (fctx.is_abstract_member)) then
				display_error ctx "Mixing abstract implementation and static properties/accessors is not allowed" f2.cf_pos;
			f2.cf_meta <- List.fold_left (fun acc ((m,_,_) as meta) -> match m with
				| Meta.Deprecated -> meta :: acc
				| _ -> acc
			) f2.cf_meta f.cff_meta;
		with
			| Error (Unify l,p) -> raise (Error (Stack (Custom ("In method " ^ m ^ " required by property " ^ name),Unify l),p))
			| Not_found ->
				let generate_field () =
					let cf = mk_field m t p null_pos in
					cf.cf_meta <- [Meta.CompilerGenerated,[],null_pos;Meta.NoCompletion,[],null_pos];
					cf.cf_kind <- Method MethNormal;
					cf
				in
				if (has_class_flag c CInterface) then begin
					let cf = generate_field () in
					c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
					c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
				end else if Diagnostics.is_diagnostics_run ctx.com f.cff_pos then begin
					let cf_accessor = generate_field() in
					remove_class_field_flag cf_accessor CfPublic;
					if fctx.is_static then add_class_field_flag cf_accessor CfStatic;
					let diag = {
						mf_pos = (pos f.cff_name);
						mf_on = TClassDecl c;
						mf_fields = [(cf_accessor,t,CompletionItem.CompletionType.from_type (Display.get_import_status ctx) t)];
						mf_cause = PropertyAccessor(cf,is_getter);
					} in
					let display = ctx.com.display_information in
					display.module_diagnostics <- MissingFields diag :: display.module_diagnostics
				end else if not (has_class_flag c CExtern) then begin
					try
						let _, _, f2 = (if not fctx.is_static then let f = PMap.find m c.cl_statics in None, f.cf_type, f else class_field c (List.map snd c.cl_params) m) in
						display_error ctx (Printf.sprintf "Method %s is no valid accessor for %s because it is %sstatic" m (name) (if fctx.is_static then "not " else "")) f2.cf_pos
					with Not_found ->
						display_error ctx ("Method " ^ m ^ " required by property " ^ name ^ " is missing") p
				end
	in
	let display_accessor m p =
		try
			let cf = match find_accessor m with [_,cf] -> cf | _ -> raise Not_found in
			DisplayEmitter.display_field ctx (Self (TClassDecl c)) (if fctx.is_static then CFSStatic else CFSMember) cf p
		with Not_found ->
			()
	in
	let delay_check = delay ctx PConnectField in
	let get = (match get with
		| "null",_ -> AccNo
		| "dynamic",_ -> AccCall
		| "never",_ -> AccNever
		| "default",_ -> AccNormal
		| "get",pget ->
			let get = "get_" ^ name in
			if fctx.is_display_field && DisplayPosition.display_position#enclosed_in pget then delay ctx PConnectField (fun () -> display_accessor get pget);
			if not cctx.is_lib then delay_check (fun() -> check_method get t_get true);
			AccCall
		| _,pget ->
			display_error ctx (name ^ ": Custom property accessor is no longer supported, please use `get`") pget;
			AccCall
	) in
	let set = (match set with
		| "null",_ ->
			(* standard flash library read-only variables can't be accessed for writing, even in subclasses *)
			if (has_class_flag c CExtern) && (match c.cl_path with "flash" :: _	, _ -> true | _ -> false) && ctx.com.platform = Flash then
				AccNever
			else
				AccNo
		| "never",_ -> AccNever
		| "dynamic",_ -> AccCall
		| "default",_ -> AccNormal
		| "set",pset ->
			let set = "set_" ^ name in
			if fctx.is_display_field && DisplayPosition.display_position#enclosed_in pset then delay ctx PConnectField (fun () -> display_accessor set pset);
			if not cctx.is_lib then delay_check (fun() -> check_method set t_set false);
			AccCall
		| _,pset ->
			display_error ctx (name ^ ": Custom property accessor is no longer supported, please use `set`") pset;
			AccCall
	) in
	if (set = AccNever && get = AccNever)  then error (name ^ ": Unsupported property combination") p;
	cf.cf_kind <- Var { v_read = get; v_write = set };
	if fctx.is_extern then add_class_field_flag cf CfExtern;
	ctx.curfield <- cf;
	bind_var (ctx,cctx,fctx) cf eo;
	cf

(**
	Emit compilation error on `final static function`
*)
let reject_final_static_method ctx cctx fctx f =
	if fctx.is_static && fctx.is_final && not (has_class_flag cctx.tclass CExtern) then
		let p =
			try snd (List.find (fun (a,p) -> a = AFinal) f.cff_access)
			with Not_found ->
				try match Meta.get Meta.Final f.cff_meta with _, _, p -> p
				with Not_found ->
					try snd (List.find (fun (a,p) -> a = AStatic) f.cff_access)
					with Not_found -> f.cff_pos
		in
		ctx.com.error "Static method cannot be final" p

let init_field (ctx,cctx,fctx) f =
	let c = cctx.tclass in
	let name = fst f.cff_name in
	TypeloadCheck.check_global_metadata ctx f.cff_meta (fun m -> f.cff_meta <- m :: f.cff_meta) c.cl_module.m_path c.cl_path (Some name);
	let p = f.cff_pos in
	if not (has_class_flag c CExtern) && not (Meta.has Meta.Native f.cff_meta) then Typecore.check_field_name ctx name p;
	List.iter (fun acc ->
		match (fst acc, f.cff_kind) with
		| APublic, _ | APrivate, _ | AStatic, _ | AFinal, _ | AExtern, _ -> ()
		| ADynamic, FFun _ | AOverride, FFun _ | AMacro, FFun _ | AInline, FFun _ | AInline, FVar _ | AAbstract, FFun _ | AOverload, FFun _ -> ()
		| _, FVar _ -> display_error ctx ("Invalid accessor '" ^ Ast.s_placed_access acc ^ "' for variable " ^ name) (snd acc)
		| _, FProp _ -> display_error ctx ("Invalid accessor '" ^ Ast.s_placed_access acc ^ "' for property " ^ name) (snd acc)
	) f.cff_access;
	begin match fctx.override with
		| Some _ ->
			(match c.cl_super with
			| None ->
				let p =
					try List.assoc AOverride f.cff_access
					with Not_found -> p
				in
				error ("Invalid override on field '" ^ name ^ "': class has no super class") p
			| _ -> ()
			);
		| None -> ()
	end;
	begin match cctx.abstract with
		| Some a when fctx.is_abstract_member -> ctx.type_params <- a.a_params;
		| _ -> ()
	end;
	let cf =
		match f.cff_kind with
		| FVar (t,e) ->
			create_variable (ctx,cctx,fctx) c f t e p
		| FFun fd ->
			reject_final_static_method ctx cctx fctx f;
			create_method (ctx,cctx,fctx) c f fd p
		| FProp (get,set,t,eo) ->
			create_property (ctx,cctx,fctx) c f (get,set,t,eo) p
	in
	(if (fctx.is_static || fctx.is_macro && ctx.in_macro) then add_class_field_flag cf CfStatic);
	cf

let check_overload ctx f fs =
	try
		let f2 =
			List.find (fun f2 ->
				f != f2 &&
				Overloads.same_overload_args f.cf_type f2.cf_type f f2
			) fs
		in
		display_error ctx ("Another overloaded field of same signature was already declared : " ^ f.cf_name) f.cf_pos;
		display_error ctx (compl_msg "The second field is declared here") f2.cf_pos;
		false
	with Not_found -> try
		(* OVERLOADTODO: generalize this and respect whether or not we actually generate the functions *)
		if ctx.com.platform <> Java then raise Not_found;
		let get_vmtype = ambiguate_funs in
		let f2 =
			List.find (fun f2 ->
				f != f2 &&
				Overloads.same_overload_args ~get_vmtype f.cf_type f2.cf_type f f2
			) fs
		in
		display_error ctx (
			"Another overloaded field of similar signature was already declared : " ^
			f.cf_name ^
			"\nThe signatures are different in Haxe, but not in the target language"
		) f.cf_pos;
		display_error ctx (compl_msg "The second field is declared here") f2.cf_pos;
		false
	with Not_found ->
		true

let check_overloads ctx c =
	(* check if field with same signature was declared more than once *)
	let check_field f =
		if has_class_field_flag f CfOverload then begin
			let all = f :: f.cf_overloads in
			ignore(List.fold_left (fun b f -> b && check_overload ctx f all) true all)
		end
	in
	List.iter check_field c.cl_ordered_fields;
	List.iter check_field c.cl_ordered_statics;
	Option.may check_field c.cl_constructor

let init_class ctx c p context_init herits fields =
	let ctx,cctx = create_class_context ctx c context_init p in
	if cctx.is_class_debug then print_endline ("Created class context: " ^ dump_class_context cctx);
	let fields = patch_class ctx c fields in
	let fields = build_fields (ctx,cctx) c fields in
	if cctx.is_core_api && ctx.com.display.dms_check_core_api then delay ctx PForce (fun() -> init_core_api ctx c);
	if not cctx.is_lib then begin
		delay ctx PForce (fun() -> check_overloads ctx c);
		begin match c.cl_super with
		| Some(csup,tl) ->
			if (has_class_flag csup CAbstract) && not (has_class_flag c CAbstract) then
				delay ctx PForce (fun () -> TypeloadCheck.Inheritance.check_abstract_class ctx c csup tl);
		| None ->
			()
		end
	end;
	let rec has_field f = function
		| None -> false
		| Some (c,_) ->
			PMap.exists f c.cl_fields || has_field f c.cl_super || List.exists (fun i -> has_field f (Some i)) c.cl_implements
	in
	let rec check_require = function
		| [] -> None
		| (Meta.Require,conds,_) :: l ->
			let rec loop = function
				| [] -> check_require l
				| e :: l ->
					let sc = match fst e with
						| EConst (Ident s) -> s
						| EBinop ((OpEq|OpNotEq|OpGt|OpGte|OpLt|OpLte) as op,(EConst (Ident s),_),(EConst ((Int _ | Float _ | String _) as c),_)) -> s ^ s_binop op ^ s_constant c
						| _ -> ""
					in
					if not (ParserEntry.is_true (ParserEntry.eval ctx.com.defines e)) then
						Some (sc,(match List.rev l with (EConst (String(msg,_)),_) :: _ -> Some msg | _ -> None))
					else
						loop l
			in
			loop conds
		| _ :: l ->
			check_require l
	in
	let rec check_if_feature = function
		| [] -> []
		| (Meta.IfFeature,el,_) :: _ -> List.map (fun (e,p) -> match e with EConst (String(s,_)) -> s | _ -> error "String expected" p) el
		| _ :: l -> check_if_feature l
	in
	let cl_if_feature = check_if_feature c.cl_meta in
	let cl_req = check_require c.cl_meta in
	let has_init = ref false in
	List.iter (fun f ->
		let p = f.cff_pos in
		try
			let ctx,fctx = create_field_context (ctx,cctx) c f in
			if fctx.is_field_debug then print_endline ("Created field context: " ^ dump_field_context fctx);
			let cf = init_field (ctx,cctx,fctx) f in
			if fctx.field_kind = FKInit then begin
				if !has_init then
					display_error ctx ("Duplicate class field declaration : " ^ (s_type_path c.cl_path) ^ "." ^ cf.cf_name) cf.cf_name_pos
				else
					has_init := true
			end;
			if fctx.is_field_debug then print_endline ("Created field: " ^ Printer.s_tclass_field "" cf);
			if fctx.is_static && (has_class_flag c CInterface) && fctx.field_kind <> FKInit && not cctx.is_lib && not ((has_class_flag c CExtern)) then
				error "You can only declare static fields in extern interfaces" p;
			let set_feature s =
				ctx.m.curmod.m_extra.m_if_feature <- (s,(c,cf,fctx.is_static)) :: ctx.m.curmod.m_extra.m_if_feature
			in
			List.iter set_feature cl_if_feature;
			List.iter set_feature (check_if_feature cf.cf_meta);
			let req = check_require f.cff_meta in
			let req = (match req with None -> if fctx.is_static || fctx.field_kind = FKConstructor then cl_req else None | _ -> req) in
			(match req with
			| None -> ()
			| Some r -> cf.cf_kind <- Var { v_read = AccRequire (fst r, snd r); v_write = AccRequire (fst r, snd r) });
			begin match fctx.field_kind with
			| FKConstructor ->
				begin match c.cl_super with
				| Some ({ cl_constructor = Some ctor_sup } as c, _) when not (has_class_flag c CExtern) && has_class_field_flag ctor_sup CfFinal ->
					ctx.com.error "Cannot override final constructor" cf.cf_pos
				| _ -> ()
				end;
				begin match c.cl_constructor with
				| None ->
						c.cl_constructor <- Some cf
				| Some ctor when ctx.com.config.pf_overload ->
					if has_class_field_flag cf CfOverload && has_class_field_flag ctor CfOverload then
						ctor.cf_overloads <- cf :: ctor.cf_overloads
					else
						display_error ctx ("If using overloaded constructors, all constructors must be declared with @:overload") (if has_class_field_flag cf CfOverload then ctor.cf_pos else cf.cf_pos)
				| Some ctor ->
							display_error ctx "Duplicate constructor" p
				end
			| FKInit ->
				()
			| FKNormal ->
				let dup = if fctx.is_static then PMap.exists cf.cf_name c.cl_fields || has_field cf.cf_name c.cl_super else PMap.exists cf.cf_name c.cl_statics in
				if not cctx.is_native && not (has_class_flag c CExtern) && dup then error ("Same field name can't be used for both static and instance : " ^ cf.cf_name) p;
				if fctx.override <> None then
					add_class_field_flag cf CfOverride;
				let is_var cf = match cf.cf_kind with | Var _ -> true | _ -> false in
				if PMap.mem cf.cf_name (if fctx.is_static then c.cl_statics else c.cl_fields) then
					if has_class_field_flag cf CfOverload && not (is_var cf) then
						let mainf = PMap.find cf.cf_name (if fctx.is_static then c.cl_statics else c.cl_fields) in
						if is_var mainf then display_error ctx "Cannot declare a variable with same name as a method" mainf.cf_pos;
						(if not (has_class_field_flag mainf CfOverload) then display_error ctx ("Overloaded methods must have @:overload metadata") mainf.cf_pos);
						mainf.cf_overloads <- cf :: mainf.cf_overloads
					else
						let type_kind,path = match c.cl_kind with
							| KAbstractImpl a -> "abstract",a.a_path
							| _ -> "class",c.cl_path
						in
						display_error ctx ("Duplicate " ^ type_kind ^ " field declaration : " ^ s_type_path path ^ "." ^ cf.cf_name) cf.cf_name_pos
				else
				if fctx.do_add then TClass.add_field c cf
			end
		with Error (Custom str,p2) when p = p2 ->
			display_error ctx str p
	) fields;
	(match cctx.abstract with
	| Some a ->
		a.a_to_field <- List.rev a.a_to_field;
		a.a_from_field <- List.rev a.a_from_field;
		a.a_ops <- List.rev a.a_ops;
		a.a_unops <- List.rev a.a_unops;
		a.a_array <- List.rev a.a_array;
	| None -> ());
	c.cl_ordered_statics <- List.rev c.cl_ordered_statics;
	c.cl_ordered_fields <- List.rev c.cl_ordered_fields;
	(* if ctx.is_display_file && not cctx.has_display_field && Display.is_display_position c.cl_pos && ctx.com.display.dms_kind = DMToplevel then begin
		let rec loop acc c tl =
			let maybe_add acc cf = match cf.cf_kind with
				| Method MethNormal when not (PMap.mem cf.cf_name acc) -> PMap.add cf.cf_name cf acc
				| _ -> acc
			in
			let acc = List.fold_left maybe_add PMap.empty c.cl_ordered_fields in
			match c.cl_super with
			| Some(c,tl) -> loop acc c tl
			| None -> acc
		in
		let fields = match c.cl_super with
			| Some(c,tl) -> loop PMap.empty c tl
			| None -> PMap.empty
		in
		let open Display in
		let l = PMap.fold (fun cf acc ->
			if not (List.exists (fun cf' -> cf'.cf_name = cf.cf_name) c.cl_overrides) then
				(IdentifierType.ITClassMember cf) :: acc
			else acc
		) fields [] in
		raise (Display.DisplayToplevel l)
	end; *)
	(*
		make sure a default contructor with same access as super one will be added to the class structure at some point.
	*)
	let has_struct_init, struct_init_pos =
		try
			let _,_,p = Meta.get Meta.StructInit c.cl_meta in
			true, p
		with Not_found ->
			false, null_pos
	in
	if has_struct_init then
		if (has_class_flag c CInterface) then
			display_error ctx "@:structInit is not allowed on interfaces" struct_init_pos
		else if (has_class_flag c CAbstract) then
			display_error ctx "@:structInit is not allowed on abstract classes" struct_init_pos
		else
			ensure_struct_init_constructor ctx c fields p;
	begin match cctx.uninitialized_final with
		| Some pf when c.cl_constructor = None ->
			display_error ctx "This class has uninitialized final vars, which requires a constructor" p;
			display_error ctx "Example of an uninitialized final var" pf;
		| _ ->
			()
	end;
	if not has_struct_init then
		(* add_constructor does not deal with overloads correctly *)
		if not ctx.com.config.pf_overload then TypeloadFunction.add_constructor ctx c cctx.force_constructor p;
	(* push delays in reverse order so they will be run in correct order *)
	List.iter (fun (ctx,r) ->
		init_class_done ctx;
		(match r with
		| None -> ()
		| Some r -> delay ctx PTypeField (fun() -> ignore(lazy_type r)))
	) cctx.delayed_expr
