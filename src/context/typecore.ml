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

type typer_module = {
	curmod : module_def;
	import_resolution : resolution_list;
	mutable own_resolution : resolution_list option;
	mutable enum_with_type : module_type option;
	mutable module_using : (tclass * pos) list;
	mutable import_statements : import list;
}

type delay = {
	delay_pass : typer_pass;
	delay_functions : (unit -> unit) list;
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

type typer_globals = {
	mutable delayed : delay list;
	mutable debug_delayed : (typer_pass * ((unit -> unit) * (string * string list) * typer) list) list;
	doinline : bool;
	retain_meta : bool;
	mutable core_api : typer option;
	mutable macros : ((unit -> unit) * typer) option;
	mutable std : tclass;
	mutable std_types : module_def;
	type_patches : (path, (string * bool, type_patch) Hashtbl.t * type_patch) Hashtbl.t;
	mutable module_check_policies : (string list * module_check_policy list * bool) list;
	mutable global_using : (tclass * pos) list;
	(* Indicates that Typer.create() finished building this instance *)
	mutable complete : bool;
	mutable type_hints : (module_def_display * pos * t) list;
	mutable load_only_cached_modules : bool;
	functional_interface_lut : (path,tclass_field) lookup;
	(* api *)
	do_macro : typer -> macro_mode -> path -> string -> expr list -> pos -> macro_result;
	do_load_macro : typer -> bool -> path -> string -> pos -> ((string * bool * t) list * t * tclass * Type.tclass_field);
	do_load_module : typer -> path -> pos -> module_def;
	do_load_type_def : typer -> pos -> type_path -> module_type;
	get_build_info : typer -> module_type -> pos -> build_info;
	do_format_string : typer -> string -> pos -> Ast.expr;
	do_load_core_class : typer -> tclass -> tclass;
}

and typer = {
	(* shared *)
	com : context;
	t : basic_types;
	g : typer_globals;
	mutable bypass_accessor : int;
	mutable meta : metadata;
	mutable with_type_stack : WithType.t list;
	mutable call_argument_stack : expr list list;
	(* variable *)
	mutable pass : typer_pass;
	(* per-module *)
	mutable m : typer_module;
	mutable is_display_file : bool;
	(* per-class *)
	mutable curclass : tclass;
	mutable tthis : t;
	mutable type_params : type_params;
	mutable get_build_infos : unit -> (module_type * t list * class_field list) option;
	(* per-function *)
	mutable allow_inline : bool;
	mutable allow_transform : bool;
	mutable curfield : tclass_field;
	mutable untyped : bool;
	mutable in_function : bool;
	mutable in_loop : bool;
	mutable in_display : bool;
	mutable macro_depth : int;
	mutable curfun : current_fun;
	mutable ret : t;
	mutable locals : (string, tvar) PMap.t;
	mutable opened : anon_status ref list;
	mutable vthis : tvar option;
	mutable in_call_args : bool;
	mutable in_overload_call_args : bool;
	mutable delayed_display : DisplayTypes.display_exception_kind option;
	mutable monomorphs : monomorphs;
	(* events *)
	memory_marker : float array;
}

and monomorphs = {
	mutable perfunction : (tmono * pos) list;
}

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

let locate_macro_error = ref true

let make_call_ref : (typer -> texpr -> texpr list -> t -> ?force_inline:bool -> pos -> texpr) ref = ref (fun _ _ _ _ ?force_inline:bool _ -> die "" __LOC__)
let type_expr_ref : (?mode:access_mode -> typer -> expr -> WithType.t -> texpr) ref = ref (fun ?(mode=MGet) _ _ _ -> die "" __LOC__)
let type_block_ref : (typer -> expr list -> WithType.t -> pos -> texpr) ref = ref (fun _ _ _ _ -> die "" __LOC__)
let unify_min_ref : (typer -> texpr list -> t) ref = ref (fun _ _ -> die "" __LOC__)
let unify_min_for_type_source_ref : (typer -> texpr list -> WithType.with_type_source option -> t) ref = ref (fun _ _ _ -> die "" __LOC__)
let analyzer_run_on_expr_ref : (Common.context -> string -> texpr -> texpr) ref = ref (fun _ _ _ -> die "" __LOC__)
let cast_or_unify_raise_ref : (typer -> ?uctx:unification_context option -> Type.t -> texpr -> pos -> texpr) ref = ref (fun _ ?uctx _ _ _ -> assert false)
let type_generic_function_ref : (typer -> field_access -> (unit -> texpr) field_call_candidate -> WithType.t -> pos -> texpr) ref = ref (fun _ _ _ _ _ -> assert false)

let create_context_ref : (Common.context -> ((unit -> unit) * typer) option -> typer) ref = ref (fun _ -> assert false)

let pass_name = function
	| PBuildModule -> "build-module"
	| PBuildClass -> "build-class"
	| PConnectField -> "connect-field"
	| PTypeField -> "type-field"
	| PCheckConstraint -> "check-constraint"
	| PForce -> "force"
	| PFinal -> "final"

let warning ?(depth=0) ctx w msg p =
	let options = (Warning.from_meta ctx.curclass.cl_meta) @ (Warning.from_meta ctx.curfield.cf_meta) in
	ctx.com.warning ~depth w options msg p

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

let make_static_this c p =
	let ta = mk_anon ~fields:c.cl_statics (ref (ClassStatics c)) in
	mk (TTypeExpr (TClassDecl c)) ta p

let make_static_field_access c cf t p =
	let ethis = make_static_this c p in
	mk (TField (ethis,(FStatic (c,cf)))) t p

let make_static_call ctx c cf map args t p =
	let monos = List.map (fun _ -> spawn_monomorph ctx p) cf.cf_params in
	let map t = map (apply_params cf.cf_params monos t) in
	let ef = make_static_field_access c cf (map cf.cf_type) p in
	make_call ctx ef args (map t) p

let raise_with_type_error ?(depth = 0) msg p =
	raise (WithTypeError (make_error ~depth (Custom msg) p))

let raise_or_display ctx l p =
	if ctx.untyped then ()
	else if ctx.in_call_args then raise (WithTypeError (make_error (Unify l) p))
	else display_error_ext ctx.com (make_error (Unify l) p)

let raise_or_display_error ctx err =
	if ctx.untyped then ()
	else if ctx.in_call_args then raise (WithTypeError err)
	else display_error_ext ctx.com err

let raise_or_display_message ctx msg p =
	if ctx.in_call_args then raise_with_type_error msg p
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
	let locals = ctx.locals in
	(fun() -> ctx.locals <- locals)

let add_local ctx k n t p =
	let v = alloc_var k n t p in
	if Define.defined ctx.com.defines Define.WarnVarShadowing && n <> "_" then begin
		match k with
		| VUser _ ->
			begin try
				let v' = PMap.find n ctx.locals in
				(* ignore std lib *)
				if not (List.exists (ExtLib.String.starts_with p.pfile) ctx.com.std_path) then begin
					warning ctx WVarShadow "This variable shadows a previously declared variable" p;
					warning ~depth:1 ctx WVarShadow (compl_msg "Previous variable was here") v'.v_pos
				end
			with Not_found ->
				()
			end
		| _ ->
			()
	end;
	ctx.locals <- PMap.add n v ctx.locals;
	v

let display_identifier_error ctx ?prepend_msg msg p =
	let prepend = match prepend_msg with Some s -> s ^ " " | _ -> "" in
	display_error ctx.com (prepend ^ msg) p

let check_identifier_name ?prepend_msg ctx name kind p =
	if starts_with name '$' then
		display_identifier_error ctx ?prepend_msg ((StringHelper.capitalize kind) ^ " names starting with a dollar are not allowed: \"" ^ name ^ "\"") p
	else if not (Lexer.is_valid_identifier name) then
		display_identifier_error ctx ?prepend_msg ("\"" ^ (StringHelper.s_escape name) ^ "\" is not a valid " ^ kind ^ " name.") p

let check_field_name ctx name p =
	match name with
	| "new" -> () (* the only keyword allowed in field names *)
	| _ -> check_identifier_name ctx name "field" p

let check_uppercase_identifier_name ?prepend_msg ctx name kind p =
	if String.length name = 0 then
		display_identifier_error ?prepend_msg ctx ((StringHelper.capitalize kind) ^ " name must not be empty.") p
	else if Ast.is_lower_ident name then
		display_identifier_error ?prepend_msg ctx ((StringHelper.capitalize kind) ^ " name should start with an uppercase letter: \"" ^ name ^ "\"") p
	else
		check_identifier_name ?prepend_msg ctx name kind p

let check_module_path ctx (pack,name) p =
	let full_path = StringHelper.s_escape (if pack = [] then name else (String.concat "." pack) ^ "." ^ name) in
	check_uppercase_identifier_name ~prepend_msg:("Module \"" ^ full_path ^ "\" does not have a valid name.") ctx name "module" p;
	try
		List.iter (fun part -> Path.check_package_name part) pack;
	with Failure msg ->
		display_error_ext ctx.com (make_error
			~sub:[make_error (Custom msg) p]
			(Custom ("\"" ^ (StringHelper.s_escape (String.concat "." pack)) ^ "\" is not a valid package name:"))
			p
		)

let check_local_variable_name ctx name origin p =
	match name with
	| "this" -> () (* TODO: vars named `this` should technically be VGenerated, not VUser *)
	| _ ->
		let s_var_origin origin =
			match origin with
			| TVOLocalVariable -> "variable"
			| TVOArgument -> "function argument"
			| TVOForVariable -> "for variable"
			| TVOPatternVariable -> "pattern variable"
			| TVOCatchVariable -> "catch variable"
			| TVOLocalFunction -> "function"
		in
		check_identifier_name ctx name (s_var_origin origin) p

let add_local_with_origin ctx origin n t p =
	check_local_variable_name ctx n origin p;
	add_local ctx (VUser origin) n t p

let gen_local_prefix = "`"

let gen_local ctx t p =
	add_local ctx VGenerated gen_local_prefix t p

let is_gen_local v = match v.v_kind with
	| VGenerated ->
		true
	| _ ->
		false

let make_delay pass fl = {
	delay_pass = pass;
	delay_functions = fl;
}

let delay ctx p f =
	let rec loop = function
		| [] ->
			[make_delay p [f]]
		| delay :: rest ->
			if delay.delay_pass = p then
				(make_delay p (f :: delay.delay_functions)) :: rest
			else if delay.delay_pass < p then
				delay :: loop rest
			else
				(make_delay p [f]) :: delay :: rest
	in
	ctx.g.delayed <- loop ctx.g.delayed

let delay_late ctx p f =
	let rec loop = function
		| [] ->
			[make_delay p [f]]
		| delay :: rest ->
			if delay.delay_pass <= p then
				delay :: loop rest
			else
				(make_delay p [f]) :: delay :: rest
	in
	ctx.g.delayed <- loop ctx.g.delayed

let delay_if_mono ctx p t f = match follow t with
	| TMono _ ->
		delay ctx p f
	| _ ->
		f()

let rec flush_pass ctx p where =
	match ctx.g.delayed with
	| delay :: rest when delay.delay_pass <= p ->
		(match delay.delay_functions with
		| [] ->
			ctx.g.delayed <- rest;
		| f :: l ->
			ctx.g.delayed <- (make_delay delay.delay_pass l) :: rest;
			f());
		flush_pass ctx p where
	| _ ->
		()

let make_pass ctx f = f

let init_class_done ctx =
	ctx.pass <- PConnectField

let enter_field_typing_pass ctx info =
	flush_pass ctx PConnectField info;
	ctx.pass <- PTypeField

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

let fake_modules = Hashtbl.create 0
let create_fake_module ctx file =
	let key = ctx.com.file_keys#get file in
	let file = Path.get_full_path file in
	let mdep = (try Hashtbl.find fake_modules key with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = (["$DEP"],file);
			m_types = [];
			m_statics = None;
			m_extra = module_extra file (Define.get_signature ctx.com.defines) (file_time file) MFake [];
		} in
		Hashtbl.add fake_modules key mdep;
		mdep
	) in
	ctx.com.module_lut#add mdep.m_path mdep;
	mdep

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
	else if c == ctx.curclass then
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
	let module_path = ctx.curclass.cl_module.m_path in
	let cur_paths = ref [fst module_path @ [snd module_path], false] in
	let rec loop c is_current_path =
		cur_paths := (make_path c ctx.curfield, is_current_path) :: !cur_paths;
		begin match c.cl_super with
			| Some (csup,_) -> loop csup false
			| None -> ()
		end;
		List.iter (fun (c,_) -> loop c false) c.cl_implements;
	in
	loop ctx.curclass true;
	let is_constr = cf.cf_name = "new" in
	let rec loop c =
		try
			has Meta.Access ctx.curclass ctx.curfield ((make_path c cf), true)
			|| (
				(* if our common ancestor declare/override the field, then we can access it *)
				let allowed f = extends ctx.curclass c || (List.exists (has Meta.Allow c f) !cur_paths) in
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
	|| (Meta.has Meta.PrivateAccess ctx.meta)

let check_field_access ctx c f stat p =
	if not ctx.untyped && not (can_access ctx c f stat) then
		display_error ctx.com ("Cannot access private field " ^ f.cf_name) p

(** removes the first argument of the class field's function type and all its overloads *)
let prepare_using_field cf = match follow cf.cf_type with
	| TFun((_,_,tf) :: args,ret) ->
		let rec loop acc overloads = match overloads with
			| ({cf_type = TFun((_,_,tfo) :: args,ret)} as cfo) :: l ->
				let tfo = apply_params cfo.cf_params (extract_param_types cfo.cf_params) tfo in
				(* ignore overloads which have a different first argument *)
				if type_iseq tf tfo then loop ({cfo with cf_type = TFun(args,ret)} :: acc) l else loop acc l
			| _ :: l ->
				loop acc l
			| [] ->
				acc
		in
		{cf with cf_overloads = loop [] cf.cf_overloads; cf_type = TFun(args,ret)}
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

let field_to_type_path com e =
	let rec loop e pack name = match e with
		| EField(e,f,_),p when Char.lowercase_ascii (String.get f 0) <> String.get f 0 -> (match name with
			| [] | _ :: [] ->
				loop e pack (f :: name)
			| _ -> (* too many name paths *)
				display_error com ("Unexpected " ^ f) p;
				raise Exit)
		| EField(e,f,_),_ ->
			loop e (f :: pack) name
		| EConst(Ident f),_ ->
			let pack, name, sub = match name with
				| [] ->
					let fchar = String.get f 0 in
					if Char.uppercase_ascii fchar = fchar then
						pack, f, None
					else begin
						display_error com "A class name must start with an uppercase letter" (snd e);
						raise Exit
					end
				| [name] ->
					f :: pack, name, None
				| [name; sub] ->
					f :: pack, name, Some sub
				| _ ->
					die "" __LOC__
			in
			{ tpackage=pack; tname=name; tparams=[]; tsub=sub }
		| _,pos ->
			display_error com "Unexpected expression when building strict meta" pos;
			raise Exit
	in
	loop e [] []

let safe_mono_close ctx m p =
	try
		Monomorph.close m
	with
		Unify_error l ->
			raise_or_display ctx l p

let relative_path ctx file =
	let slashes path = String.concat "/" (ExtString.String.nsplit path "\\") in
	let fpath = slashes (Path.get_full_path file) in
	let fpath_lower = String.lowercase_ascii fpath in
	let flen = String.length fpath_lower in
	let rec loop = function
		| [] -> file
		| path :: l ->
			let spath = String.lowercase_ascii (slashes path) in
			let slen = String.length spath in
			if slen > 0 && slen < flen && String.sub fpath_lower 0 slen = spath then String.sub fpath slen (flen - slen) else loop l
	in
	loop ctx.com.Common.class_path

let mk_infos ctx p params =
	let file = if ctx.com.is_macro_context then p.pfile else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path p.pfile else relative_path ctx p.pfile in
	(EObjectDecl (
		(("fileName",null_pos,NoQuotes) , (EConst (String(file,SDoubleQuotes)) , p)) ::
		(("lineNumber",null_pos,NoQuotes) , (EConst (Int (string_of_int (Lexer.get_error_line p), None)),p)) ::
		(("className",null_pos,NoQuotes) , (EConst (String (s_type_path ctx.curclass.cl_path,SDoubleQuotes)),p)) ::
		if ctx.curfield.cf_name = "" then
			params
		else
			(("methodName",null_pos,NoQuotes), (EConst (String (ctx.curfield.cf_name,SDoubleQuotes)),p)) :: params
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
	class_meta = ctx.curclass.cl_meta;
	field_meta = ctx.curfield.cf_meta;
}

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

let init_class_done ctx =
	let path = fst ctx.curclass.cl_path @ [snd ctx.curclass.cl_path] in
	debug ctx.com path ("init_class_done " ^ s_type_path ctx.curclass.cl_path);
	init_class_done ctx

let ctx_pos ctx =
	let inf = fst ctx.m.curmod.m_path @ [snd ctx.m.curmod.m_path]in
	let inf = (match snd ctx.curclass.cl_path with "" -> inf | n when n = snd ctx.m.curmod.m_path -> inf | n -> inf @ [n]) in
	let inf = (match ctx.curfield.cf_name with "" -> inf | n -> inf @ [n]) in
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


