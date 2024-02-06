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

open Globals
open Ast
open Common
open Lookup
open Type
open Error
open Resolution
open FieldCallCandidate

type type_patch = {
	mutable tp_type : complex_type option;
	mutable tp_remove : bool;
	mutable tp_meta : metadata;
}

type current_fun =
	| FunMember
	| FunStatic
	| FunConstructor
	| FunMemberAbstract
	| FunMemberClassLocal
	| FunMemberAbstractLocal

type macro_mode =
	| MExpr
	| MBuild
	| MMacroType
	| MDisplay

type access_mode =
	| MGet
	| MSet of Ast.expr option (* rhs, if exists *)
	| MCall of Ast.expr list (* call arguments *)

type typer_pass =
	| PBuildModule			(* build the module structure and setup module type parameters *)
	| PBuildClass			(* build the class structure *)
	| PConnectField			(* handle associated fields, which may affect each other. E.g. a property and its getter *)
	| PTypeField			(* type the class field, allow access to types structures *)
	| PCheckConstraint		(* perform late constraint checks with inferred types *)
	| PForce				(* usually ensure that lazy have been evaluated *)
	| PFinal				(* not used, only mark for finalize *)

let all_typer_passes = [
	PBuildModule;PBuildClass;PConnectField;PTypeField;PCheckConstraint;PForce;PFinal
]

let all_typer_passes_length = List.length all_typer_passes

type typer_module = {
	curmod : module_def;
	import_resolution : resolution_list;
	mutable own_resolution : resolution_list option;
	mutable enum_with_type : module_type option;
	mutable module_using : (tclass * pos) list;
	mutable import_statements : import list;
	mutable is_display_file : bool;
}

type typer_class = {
	mutable curclass : tclass; (* TODO: should not be mutable *)
	mutable tthis : t;
	mutable get_build_infos : unit -> (module_type * t list * class_field list) option;
}

type build_kind =
	| BuildNormal
	| BuildGeneric of tclass
	| BuildGenericBuild
	| BuildMacroType

type build_info = {
	build_kind : build_kind;
	build_path : path;
	build_params : type_params;
	build_extern : bool;
	build_apply : Type.t list -> Type.t;
}

type macro_result =
	| MSuccess of expr
	| MError
	| MMacroInMacro

type typer_pass_tasks = {
	mutable tasks : (unit -> unit) list;
}

type typer_globals = {
	mutable delayed : typer_pass_tasks Array.t;
	mutable delayed_min_index : int;
	mutable debug_delayed : (typer_pass * ((unit -> unit) * (string * string list) * typer) list) list;
	doinline : bool;
	retain_meta : bool;
	mutable core_api : typer option;
	mutable macros : ((unit -> unit) * typer) option;
	mutable std_types : module_def;
	mutable module_check_policies : (string list * module_check_policy list * bool) list;
	mutable global_using : (tclass * pos) list;
	(* Indicates that Typer.create() finished building this instance *)
	mutable complete : bool;
	mutable type_hints : (module_def_display * pos * t) list;
	mutable load_only_cached_modules : bool;
	functional_interface_lut : (path,tclass_field) lookup;
	mutable return_partial_type : bool;
	mutable build_count : int;
	mutable t_dynamic_def : Type.t;
	mutable delayed_display : DisplayTypes.display_exception_kind option;
	root_typer : typer;
	(* api *)
	do_macro : typer -> macro_mode -> path -> string -> expr list -> pos -> macro_result;
	do_load_macro : typer -> bool -> path -> string -> pos -> ((string * bool * t) list * t * tclass * Type.tclass_field);
	do_load_module : typer -> path -> pos -> module_def;
	do_load_type_def : typer -> pos -> type_path -> module_type;
	get_build_info : typer -> module_type -> pos -> build_info;
	do_format_string : typer -> string -> pos -> Ast.expr;
	do_load_core_class : typer -> tclass -> tclass;
}

(* typer_expr holds information that is specific to a (function) expresssion, whereas typer_field
   is shared by local TFunctions. *)
and typer_expr = {
	curfun : current_fun;
	in_function : bool;
	mutable ret : t;
	mutable opened : anon_status ref list;
	mutable monomorphs : monomorphs;
	mutable in_loop : bool;
	mutable bypass_accessor : int;
	mutable with_type_stack : WithType.t list;
	mutable call_argument_stack : expr list list;
	mutable macro_depth : int;
}

and typer_field = {
	mutable curfield : tclass_field;
	mutable locals : (string, tvar) PMap.t;
	mutable vthis : tvar option;
	mutable untyped : bool;
	mutable meta : metadata;
	mutable in_display : bool;
	mutable in_call_args : bool;
	mutable in_overload_call_args : bool;
	mutable is_coroutine : bool;
}

and typer = {
	(* shared *)
	com : context;
	t : basic_types;
	g : typer_globals;
	mutable m : typer_module;
	c : typer_class;
	f : typer_field;
	e : typer_expr;
	pass : typer_pass;
	mutable type_params : type_params;
	mutable allow_inline : bool;
	mutable allow_transform : bool;
	(* events *)
	memory_marker : float array;
}

and monomorphs = {
	mutable perfunction : (tmono * pos) list;
}

let pass_name = function
	| PBuildModule -> "build-module"
	| PBuildClass -> "build-class"
	| PConnectField -> "connect-field"
	| PTypeField -> "type-field"
	| PCheckConstraint -> "check-constraint"
	| PForce -> "force"
	| PFinal -> "final"

module TyperManager = struct
	let create ctx m c f e pass params =
		if pass < ctx.pass then die (Printf.sprintf "Bad context clone from %s(%s) to %s(%s)" (s_type_path ctx.m.curmod.m_path) (pass_name ctx.pass) (s_type_path m.curmod.m_path) (pass_name pass)) __LOC__;
		let new_ctx = {
			com = ctx.com;
			g = ctx.g;
			t = ctx.com.basic;
			m = m;
			c = c;
			f = f;
			e = e;
			pass = pass;
			allow_inline = ctx.allow_inline;
			allow_transform = ctx.allow_transform;
			type_params = params;
			memory_marker = memory_marker;
		} in
		new_ctx

	let create_ctx_c c =
		{
			curclass = c;
			tthis = (match c.cl_kind with
				| KAbstractImpl a ->
					(match a.a_this with
					| TMono r when r.tm_type = None -> TAbstract (a,extract_param_types c.cl_params)
					| t -> t)
				| _ ->
					TInst (c,extract_param_types c.cl_params)
			);
			get_build_infos = (fun () -> None);
		}

	let create_ctx_f cf =
		{
			locals = PMap.empty;
			curfield = cf;
			vthis = None;
			untyped = false;
			meta = [];
			in_display = false;
			in_overload_call_args = false;
			in_call_args = false;
			is_coroutine = false;
		}

	let create_ctx_e curfun in_function =
		{
			curfun;
			in_function;
			ret = t_dynamic;
			opened = [];
			monomorphs = {
				perfunction = [];
			};
			in_loop = false;
			bypass_accessor = 0;
			with_type_stack = [];
			call_argument_stack = [];
			macro_depth = 0;
		}

	let clone_for_module ctx m =
		let ctx = create ctx m ctx.c ctx.f ctx.e PBuildModule [] in
		ctx.allow_transform <- true;
		ctx.allow_inline <- true;
		ctx

	let clone_for_class ctx c =
		let c = create_ctx_c c in
		let params = match c.curclass.cl_kind with KAbstractImpl a -> a.a_params | _ -> c.curclass.cl_params in
		create ctx ctx.m c ctx.f ctx.e PBuildClass params

	let clone_for_enum ctx en =
		let c = create_ctx_c null_class in
		create ctx ctx.m c ctx.f ctx.e PBuildClass en.e_params

	let clone_for_typedef ctx td =
		let c = create_ctx_c null_class in
		create ctx ctx.m c ctx.f ctx.e PBuildClass td.t_params

	let clone_for_abstract ctx a =
		let c = create_ctx_c null_class in
		create ctx ctx.m c ctx.f ctx.e PBuildClass a.a_params

	let clone_for_field ctx cf params =
		let f = create_ctx_f cf in
		create ctx ctx.m ctx.c f ctx.e PBuildClass params

	let clone_for_enum_field ctx params =
		let f = create_ctx_f null_field in
		create ctx ctx.m ctx.c f ctx.e PBuildClass params

	let clone_for_expr ctx curfun in_function =
		let e = create_ctx_e curfun in_function in
		create ctx ctx.m ctx.c ctx.f e PTypeField ctx.type_params

	let clone_for_type_params ctx params =
		create ctx ctx.m ctx.c ctx.f ctx.e ctx.pass params

	let clone_for_type_parameter_expression ctx =
		let f = create_ctx_f ctx.f.curfield in
		let e = create_ctx_e ctx.e.curfun false in
		create ctx ctx.m ctx.c f e PTypeField ctx.type_params
end

type field_host =
	| FHStatic of tclass
	| FHInstance of tclass * tparams
	| FHAbstract of tabstract * tparams * tclass
	| FHAnon

type field_access = {
	(* The expression on which the field is accessed. For abstracts, this is a type expression
	   to the implementation class. *)
	fa_on     : texpr;
	(* The field being accessed. Note that in case of overloads, this might refer to the main field which
	   hosts other overloads in its cf_overloads. *)
	fa_field  : tclass_field;
	(* The host of the field. *)
	fa_host   : field_host;
	(* Whether or not to inline the access. This can be set for non-inline fields via `inline call()` syntax. *)
	fa_inline : bool;
	(* The position of the field access expression in syntax. *)
	fa_pos    : pos;
}

type static_extension_access = {
	(* The `this` expression which should be passed as first argument. *)
	se_this   : texpr;
	(* The field access information. *)
	se_access : field_access;
}

type dot_path_part_case =
	| PUppercase
	| PLowercase

type dot_path_part = {
	name : string;
	case : dot_path_part_case;
	pos : pos
}
let make_build_info kind path params extern apply = {
	build_kind = kind;
	build_path = path;
	build_params = params;
	build_extern = extern;
	build_apply = apply;
}

exception Forbid_package of (string * path * pos) * pos list * string

exception WithTypeError of error

let memory_marker = [|Unix.time()|]

let make_call_ref : (typer -> texpr -> texpr list -> t -> ?force_inline:bool -> pos -> texpr) ref = ref (fun _ _ _ _ ?force_inline:bool _ -> die "" __LOC__)
let type_expr_ref : (?mode:access_mode -> typer -> expr -> WithType.t -> texpr) ref = ref (fun ?(mode=MGet) _ _ _ -> die "" __LOC__)
let type_block_ref : (typer -> expr list -> WithType.t -> pos -> texpr) ref = ref (fun _ _ _ _ -> die "" __LOC__)
let unify_min_ref : (typer -> texpr list -> t) ref = ref (fun _ _ -> die "" __LOC__)
let unify_min_for_type_source_ref : (typer -> texpr list -> WithType.with_type_source option -> t) ref = ref (fun _ _ _ -> die "" __LOC__)
let analyzer_run_on_expr_ref : (Common.context -> string -> texpr -> texpr) ref = ref (fun _ _ _ -> die "" __LOC__)
let cast_or_unify_raise_ref : (typer -> ?uctx:unification_context option -> Type.t -> texpr -> pos -> texpr) ref = ref (fun _ ?uctx _ _ _ -> assert false)
let type_generic_function_ref : (typer -> field_access -> (unit -> texpr) field_call_candidate -> WithType.t -> pos -> texpr) ref = ref (fun _ _ _ _ _ -> assert false)

let create_context_ref : (Common.context -> ((unit -> unit) * typer) option -> typer) ref = ref (fun _ -> assert false)

let warning ?(depth=0) ctx w msg p =
	let options = (Warning.from_meta ctx.c.curclass.cl_meta) @ (Warning.from_meta ctx.f.curfield.cf_meta) in
	match Warning.get_mode w options with
	| WMEnable ->
		module_warning ctx.com ctx.m.curmod w options msg p
	| WMDisable ->
		()

let make_call ctx e el t p = (!make_call_ref) ctx e el t p

let type_expr ?(mode=MGet) ctx e with_type = (!type_expr_ref) ~mode ctx e with_type

let unify_min ctx el = (!unify_min_ref) ctx el
let unify_min_for_type_source ctx el src = (!unify_min_for_type_source_ref) ctx el src

let spawn_monomorph' ctx p =
	let mono = Monomorph.create () in
	ctx.monomorphs.perfunction <- (mono,p) :: ctx.monomorphs.perfunction;
	mono

let spawn_monomorph ctx p =
	TMono (spawn_monomorph' ctx p)

let make_static_field_access c cf t p =
	let ethis = Texpr.Builder.make_static_this c p in
	mk (TField (ethis,(FStatic (c,cf)))) t p

let make_static_call ctx c cf map args t p =
	let monos = List.map (fun _ -> spawn_monomorph ctx.e p) cf.cf_params in
	let map t = map (apply_params cf.cf_params monos t) in
	let ef = make_static_field_access c cf (map cf.cf_type) p in
	make_call ctx ef args (map t) p

let raise_with_type_error ?(depth = 0) msg p =
	raise (WithTypeError (make_error ~depth (Custom msg) p))

let raise_or_display ctx l p =
	if ctx.f.untyped then ()
	else if ctx.f.in_call_args then raise (WithTypeError (make_error (Unify l) p))
	else display_error_ext ctx.com (make_error (Unify l) p)

let raise_or_display_error ctx err =
	if ctx.f.untyped then ()
	else if ctx.f.in_call_args then raise (WithTypeError err)
	else display_error_ext ctx.com err

let raise_or_display_message ctx msg p =
	if ctx.f.in_call_args then raise_with_type_error msg p
	else display_error ctx.com msg p

let unify ctx t1 t2 p =
	try
		Type.unify t1 t2
	with
		Unify_error l ->
			raise_or_display ctx l p

let unify_raise_custom uctx t1 t2 p =
	try
		Type.unify_custom uctx t1 t2
	with
		Unify_error l ->
			(* no untyped check *)
			raise_error_msg (Unify l) p

let unify_raise = unify_raise_custom default_unification_context

let save_locals ctx =
	let locals = ctx.f.locals in
	(fun() -> ctx.f.locals <- locals)

let add_local ctx k n t p =
	let v = alloc_var k n t p in
	if Define.defined ctx.com.defines Define.WarnVarShadowing && n <> "_" then begin
		match k with
		| VUser _ ->
			begin try
				let v' = PMap.find n ctx.f.locals in
				(* ignore std lib *)
				if not (List.exists (fun path -> ExtLib.String.starts_with p.pfile (path#path)) ctx.com.class_paths#get_std_paths) then begin
					warning ctx WVarShadow "This variable shadows a previously declared variable" p;
					warning ~depth:1 ctx WVarShadow (compl_msg "Previous variable was here") v'.v_pos
				end
			with Not_found ->
				()
			end
		| _ ->
			()
	end;
	ctx.f.locals <- PMap.add n v ctx.f.locals;
	v

let add_local_with_origin ctx origin n t p =
	Naming.check_local_variable_name ctx.com n origin p;
	add_local ctx (VUser origin) n t p

let gen_local_prefix = "`"

let gen_local ctx t p =
	add_local ctx VGenerated gen_local_prefix t p

let is_gen_local v = match v.v_kind with
	| VGenerated ->
		true
	| _ ->
		false

let delay g p f =
	let p = Obj.magic p in
	let tasks = g.delayed.(p) in
	tasks.tasks <- f :: tasks.tasks;
	if p < g.delayed_min_index then
		g.delayed_min_index <- p

let delay_late g p f =
	let p = Obj.magic p in
	let tasks = g.delayed.(p) in
	tasks.tasks <- tasks.tasks @ [f];
	if p < g.delayed_min_index then
		g.delayed_min_index <- p

let delay_if_mono g p t f = match follow t with
	| TMono _ ->
		delay g p f
	| _ ->
		f()

let rec flush_pass g p where =
	let rec loop i =
		if i > (Obj.magic p) then
			()
		else begin
			let tasks = g.delayed.(i) in
			match tasks.tasks with
			| f :: l ->
				tasks.tasks <- l;
				f();
				flush_pass g p where
			| [] ->
				(* Done with this pass (for now), update min index to next one *)
				let i = i + 1 in
				g.delayed_min_index <- i;
				loop i
		end
	in
	loop g.delayed_min_index

let make_pass ctx f = f

let enter_field_typing_pass g info =
	flush_pass g PConnectField info

let make_lazy ?(force=true) ctx t_proc f where =
	let r = ref (lazy_available t_dynamic) in
	r := lazy_wait (fun() ->
		try
			r := lazy_processing t_proc;
			let t = f r in
			r := lazy_available t;
			t
		with
			| Error e ->
				raise (Fatal_error e)
	);
	if force then delay ctx PForce (fun () -> ignore(lazy_type r));
	r

let is_removable_field com f =
	not (has_class_field_flag f CfOverride) && (
		has_class_field_flag f CfExtern || has_class_field_flag f CfGeneric
		|| (match f.cf_kind with
			| Var {v_read = AccRequire (s,_)} -> true
			| Method MethMacro -> not com.is_macro_context
			| _ -> false)
	)

let is_forced_inline c cf =
	match c with
	| Some { cl_kind = KAbstractImpl _ } -> true
	| Some c when has_class_flag c CExtern -> true
	| _ when has_class_field_flag cf CfExtern -> true
	| _ -> false

let needs_inline ctx c cf =
	cf.cf_kind = Method MethInline && ctx.allow_inline && (ctx.g.doinline || is_forced_inline c cf)

(** checks if we can access to a given class field using current context *)
let can_access ctx c cf stat =
	if (has_class_field_flag cf CfPublic) then
		true
	else if c == ctx.c.curclass then
		true
	else match ctx.m.curmod.m_statics with
		| Some c' when c == c' ->
			true
		| _ ->
	(* has metadata path *)
	let rec make_path c f = match c.cl_kind with
		| KAbstractImpl a -> fst a.a_path @ [snd a.a_path; f.cf_name]
		| KGenericInstance(c,_) -> make_path c f
		| _ when c.cl_private -> List.rev (f.cf_name :: snd c.cl_path :: (List.tl (List.rev (fst c.cl_path))))
		| _ -> fst c.cl_path @ [snd c.cl_path; f.cf_name]
	in
	let rec expr_path acc e =
		match fst e with
		| EField (e,f,_) -> expr_path (f :: acc) e
		| EConst (Ident n) -> n :: acc
		| _ -> []
	in
	let rec chk_path psub pfull is_current_path =
		match psub, pfull with
		| [], _ -> true
		| a :: l1, b :: l2 when a = b ->
			if
				(* means it's a path of a superclass or implemented interface *)
				not is_current_path &&
				(* it's the last part of path in a meta && it denotes a package *)
				l1 = [] && not (StringHelper.starts_uppercase_identifier a)
			then
				false
			else
				chk_path l1 l2 is_current_path
		| _ -> false
	in
	let has m c f (path,is_current_path) =
		let rec loop = function
			| (m2,el,_) :: l when m = m2 ->
				List.exists (fun e ->
					match fst e with
					| EConst (Ident "std") ->
						(* If we have `@:allow(std)`, check if our path has exactly two elements
						   (type name + field name) *)
						(match path with [_;_] -> true | _ -> false)
					| _ ->
						let p = expr_path [] e in
						(p <> [] && chk_path p path is_current_path)
				) el
				|| loop l
			| _ :: l -> loop l
			| [] -> false
		in
		loop c.cl_meta || loop f.cf_meta
	in
	let module_path = ctx.c.curclass.cl_module.m_path in
	let cur_paths = ref [fst module_path @ [snd module_path], false] in
	let rec loop c is_current_path =
		cur_paths := (make_path c ctx.f.curfield, is_current_path) :: !cur_paths;
		begin match c.cl_super with
			| Some (csup,_) -> loop csup false
			| None -> ()
		end;
		List.iter (fun (c,_) -> loop c false) c.cl_implements;
	in
	loop ctx.c.curclass true;
	let is_constr = cf.cf_name = "new" in
	let rec loop c =
		try
			has Meta.Access ctx.c.curclass ctx.f.curfield ((make_path c cf), true)
			|| (
				(* if our common ancestor declare/override the field, then we can access it *)
				let allowed f = extends ctx.c.curclass c || (List.exists (has Meta.Allow c f) !cur_paths) in
				if is_constr then (
					match c.cl_constructor with
					| Some cf ->
						if allowed cf then true
						else if cf.cf_expr = None then false (* maybe it's an inherited auto-generated constructor *)
						else raise Not_found
					| _ -> false
				) else
					try allowed (PMap.find cf.cf_name (if stat then c.cl_statics else c.cl_fields))
					with Not_found -> false
			)
			|| (match c.cl_super with
			| Some (csup,_) -> loop csup
			| None -> false)
		with Not_found -> false
	in
	loop c
	(* access is also allowed of we access a type parameter which is constrained to our (base) class *)
	|| (match c.cl_kind with
		| KTypeParameter ttp ->
			List.exists (fun t -> match follow t with TInst(c,_) -> loop c | _ -> false) (get_constraints ttp)
		| _ -> false)
	|| (Meta.has Meta.PrivateAccess ctx.f.meta)

let check_field_access ctx c f stat p =
	if not ctx.f.untyped && not (can_access ctx c f stat) then
		display_error ctx.com ("Cannot access private field " ^ f.cf_name) p

(** removes the first argument of the class field's function type and all its overloads *)
let prepare_using_field cf = match follow cf.cf_type with
	| TFun((_,_,tf) :: args,ret,coro) ->
		let rec loop acc overloads = match overloads with
			| ({cf_type = TFun((_,_,tfo) :: args,ret,_)} as cfo) :: l ->
				let tfo = apply_params cfo.cf_params (extract_param_types cfo.cf_params) tfo in
				(* ignore overloads which have a different first argument *)
				if type_iseq tf tfo then loop ({cfo with cf_type = TFun(args,ret,coro)} :: acc) l else loop acc l
			| _ :: l ->
				loop acc l
			| [] ->
				acc
		in
		{cf with cf_overloads = loop [] cf.cf_overloads; cf_type = TFun(args,ret,coro)}
	| _ -> cf

let merge_core_doc ctx mt =
	(match mt with
	| TClassDecl c | TAbstractDecl { a_impl = Some c } when Meta.has Meta.CoreApi c.cl_meta ->
		let c_core = ctx.g.do_load_core_class ctx c in
		if c.cl_doc = None then c.cl_doc <- c_core.cl_doc;
		let maybe_merge cf_map cf =
			if cf.cf_doc = None then try cf.cf_doc <- (PMap.find cf.cf_name cf_map).cf_doc with Not_found -> ()
		in
		List.iter (maybe_merge c_core.cl_fields) c.cl_ordered_fields;
		List.iter (maybe_merge c_core.cl_statics) c.cl_ordered_statics;
		begin match c.cl_constructor,c_core.cl_constructor with
			| Some ({cf_doc = None} as cf),Some cf2 -> cf.cf_doc <- cf2.cf_doc
			| _ -> ()
		end
	| _ -> ())

let safe_mono_close ctx m p =
	try
		Monomorph.close m
	with
		Unify_error l ->
			raise_or_display ctx l p

(* TODO: this is wrong *)
let coroutine_type ctx args ret =
	let args = args @ [("_hx_continuation",false,(tfun [ret; t_dynamic] ctx.com.basic.tvoid))] in
	let ret = ctx.com.basic.tvoid in
	TFun(args,ret,true)

let relative_path ctx file =
	ctx.com.class_paths#relative_path file

let mk_infos ctx p params =
	let file = if ctx.com.is_macro_context then p.pfile else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path p.pfile else relative_path ctx p.pfile in
	(EObjectDecl (
		(("fileName",null_pos,NoQuotes) , (EConst (String(file,SDoubleQuotes)) , p)) ::
		(("lineNumber",null_pos,NoQuotes) , (EConst (Int (string_of_int (Lexer.get_error_line p), None)),p)) ::
		(("className",null_pos,NoQuotes) , (EConst (String (s_type_path ctx.c.curclass.cl_path,SDoubleQuotes)),p)) ::
		if ctx.f.curfield.cf_name = "" then
			params
		else
			(("methodName",null_pos,NoQuotes), (EConst (String (ctx.f.curfield.cf_name,SDoubleQuotes)),p)) :: params
	) ,p)

let rec is_pos_infos = function
	| TMono r ->
		(match r.tm_type with
		| Some t -> is_pos_infos t
		| _ -> false)
	| TLazy f ->
		is_pos_infos (lazy_type f)
	| TType ({ t_path = ["haxe"] , "PosInfos" },[]) ->
		true
	| TType (t,tl) ->
		is_pos_infos (apply_typedef t tl)
	| TAbstract({a_path=[],"Null"},[t]) ->
		is_pos_infos t
	| _ ->
		false

let is_empty_or_pos_infos args =
	match args with
	| [_,true,t] -> is_pos_infos t
	| [] -> true
	| _ -> false

let get_next_stored_typed_expr_id =
	let uid = ref 0 in
	(fun() -> incr uid; !uid)

let make_stored_id_expr id p =
	(EConst (Int (string_of_int id, None))), p

let store_typed_expr com te p =
	let id = get_next_stored_typed_expr_id() in
	com.stored_typed_exprs#add id te;
	let eid = make_stored_id_expr id p in
	id,((EMeta ((Meta.StoredTypedExpr,[],null_pos), eid)),p)

let push_this ctx e = match e.eexpr with
| TConst ((TInt _ | TFloat _ | TString _ | TBool _) as ct) ->
	(EConst (tconst_to_const ct),e.epos),fun () -> ()
| _ ->
	let id,er = store_typed_expr ctx.com e e.epos in
	er,fun () -> ctx.com.stored_typed_exprs#remove id

let create_deprecation_context ctx = {
	(DeprecationCheck.create_context ctx.com) with
	class_meta = ctx.c.curclass.cl_meta;
	field_meta = ctx.f.curfield.cf_meta;
	curmod = ctx.m.curmod;
}

let get_overloads (com : Common.context) c i =
	try
		com.overload_cache#find (c.cl_path,i)
	with Not_found ->
		let l = Overloads.collect_overloads (fun t -> t) c i in
		com.overload_cache#add (c.cl_path,i) l;
		l

(* -------------- debug functions to activate when debugging typer passes ------------------------------- *)

(*
let delay_tabs = ref ""

let context_ident com =
	if Common.defined com Common.Define.CoreApi then
		" core "
	else if Common.defined com Common.Define.Macro then
		"macro "
	else
		"  out "

let debug_paths = [
	(* ["Main"] *)
]

let debug com (path : string list) str =
	if Common.raw_defined com "cdebug" then begin
		let emit () =
			let s = (context_ident com ^ string_of_int (String.length !delay_tabs) ^ " " ^ !delay_tabs ^ str) in
			match com.json_out with
			| None -> print_endline s
			| Some _ -> DynArray.add com.pass_debug_messages s
		in
		match debug_paths,path with
		| [],_
		| _,[] ->
			emit()
		| l ->
			if List.exists (Ast.match_path false path) debug_paths then emit();
	end

let ctx_pos ctx =
	let inf = fst ctx.m.curmod.m_path @ [snd ctx.m.curmod.m_path]in
	let inf = (match snd ctx.c.curclass.cl_path with "" -> inf | n when n = snd ctx.m.curmod.m_path -> inf | n -> inf @ [n]) in
	let inf = (match ctx.f.curfield.cf_name with "" -> inf | n -> inf @ [n]) in
	inf

let pass_infos ctx p =
	let path = ctx_pos ctx in
	let inf = pass_name p ^ " ("  ^ String.concat "." path ^ ")" in
	let inf = if ctx.pass > p then inf ^ " ??CURPASS=" ^ pass_name ctx.pass else inf in
	inf,path

let delay ctx p f =
	let inf = pass_infos ctx p in
	let rec loop = function
		| [] -> [p,[f,inf,ctx]]
		| (p2,l) :: rest ->
			if p2 = p then
				(p, (f,inf,ctx) :: l) :: rest
			else if p2 < p then
				(p2,l) :: loop rest
			else
				(p,[f,inf,ctx]) :: (p2,l) :: rest
	in
	ctx.g.debug_delayed <- loop ctx.g.debug_delayed;
	debug ctx.com (snd inf) ("add " ^ (fst inf))

let delay_late ctx p f =
	let inf = pass_infos ctx p in
	let rec loop = function
		| [] -> [p,[f,inf,ctx]]
		| (p2,l) :: rest ->
			if p2 <= p then
				(p2,l) :: loop rest
			else
				(p,[f,inf,ctx]) :: (p2,l) :: rest
	in
	ctx.g.debug_delayed <- loop ctx.g.debug_delayed;
	debug ctx.com (snd inf) ("add late " ^ (fst inf))

let pending_passes ctx =
	let rec loop acc = function
		| (p,l) :: pl when p < ctx.pass -> loop (acc @ l) pl
		| _ -> acc
	in
	match loop [] ctx.g.debug_delayed with
	| [] -> ""
	| l -> " ??PENDING[" ^ String.concat ";" (List.map (fun (_,(i,_),_) -> i) l) ^ "]"

let display_error com ?(depth=0) msg p =
	debug com [] ("ERROR " ^ msg);
	display_error com ~depth msg p

let display_error_ext com err =
	debug com [] ("ERROR " ^ (error_msg err.err_message));
	display_error_ext com err

let make_pass ?inf ctx f =
	let inf,path = (match inf with None -> pass_infos ctx ctx.pass | Some inf -> inf) in
	(fun v ->
		debug ctx.com path ("run " ^ inf ^ pending_passes ctx);
		let old = !delay_tabs in
		delay_tabs := !delay_tabs ^ "\t";
		let t = (try
			f v
		with
			| Fatal_error _ as exc ->
				delay_tabs := old;
				raise exc
			| exc when not (Common.raw_defined ctx.com "stack") ->
				debug ctx.com path ("FATAL " ^ Printexc.to_string exc);
				delay_tabs := old;
				raise exc
		) in
		delay_tabs := old;
		t
	)

let rec flush_pass ctx p where =
	let rec loop() =
		match ctx.g.debug_delayed with
		| (p2,l) :: rest when p2 <= p ->
			(match l with
			| [] ->
				ctx.g.debug_delayed <- rest
			| (f,inf,ctx2) :: l ->
				ctx.g.debug_delayed <- (p2,l) :: rest;
				match p2 with
				| PTypeField | PBuildClass -> f()
				| _ -> (make_pass ~inf ctx f)());
			loop()
		| _ ->
			()
	in
	match ctx.g.debug_delayed with
	| (p2,_) :: _ when p2 <= p ->
		let old = !delay_tabs in
		debug ctx.com (snd where) ("flush " ^ pass_name p ^ "(" ^ (fst where) ^ ")");
		delay_tabs := !delay_tabs ^ "\t";
		loop();
		delay_tabs := old;
		debug ctx.com (snd where) "flush-done";
	| _ ->
		()

let make_where ctx where =
	let inf = ctx_pos ctx in
	where ^ " (" ^ String.concat "." inf ^ ")",inf

let make_lazy ?(force=true) ctx t f (where:string) =
	let r = ref (lazy_available t_dynamic) in
	r := lazy_wait (make_pass ~inf:(make_where ctx where) ctx (fun() ->
		try
			r := lazy_processing t;
			let t = f r in
			r := lazy_available t;
			t
		with
			| Error e ->
				raise (Fatal_error e)
	));
	if force then delay ctx PForce (fun () -> ignore(lazy_type r));
	r

*)
(* --------------------------------------------------- *)


