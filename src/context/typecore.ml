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
open DisplayTypes

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
	| MSet
	| MCall

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
	mutable module_types : (module_type * pos) list;
	mutable module_using : (tclass * pos) list;
	mutable module_globals : (string, (module_type * string * pos)) PMap.t;
	mutable wildcard_packages : (string list * pos) list;
	mutable module_imports : import list;
}

type typer_globals = {
	types_module : (path, path) Hashtbl.t;
	modules : (path , module_def) Hashtbl.t;
	mutable delayed : (typer_pass * (unit -> unit) list) list;
	mutable debug_delayed : (typer_pass * ((unit -> unit) * string * typer) list) list;
	doinline : bool;
	mutable core_api : typer option;
	mutable macros : ((unit -> unit) * typer) option;
	mutable std : module_def;
	mutable hook_generate : (unit -> unit) list;
	type_patches : (path, (string * bool, type_patch) Hashtbl.t * type_patch) Hashtbl.t;
	mutable global_metadata : (string list * metadata_entry * (bool * bool * bool)) list;
	mutable module_check_policies : (string list * module_check_policy list * bool) list;
	mutable global_using : (tclass * pos) list;
	(* Indicates that Typer.create() finished building this instance *)
	mutable complete : bool;
	mutable type_hints : (module_def_display * pos * t) list;
	(* api *)
	do_inherit : typer -> Type.tclass -> pos -> (bool * placed_type_path) -> bool;
	do_create : Common.context -> typer;
	do_macro : typer -> macro_mode -> path -> string -> expr list -> pos -> expr option;
	do_load_macro : typer -> bool -> path -> string -> pos -> ((string * bool * t) list * t * tclass * Type.tclass_field);
	do_load_module : typer -> path -> pos -> module_def;
	do_load_type_def : typer -> pos -> type_path -> module_type;
	do_optimize : typer -> texpr -> texpr;
	do_build_instance : typer -> module_type -> pos -> ((string * t) list * path * (t list -> t));
	do_format_string : typer -> string -> pos -> Ast.expr;
	do_finalize : typer -> unit;
	do_generate : typer -> (texpr option * module_type list * module_def list);
	do_load_core_class : typer -> tclass -> tclass;
}

and typer = {
	(* shared *)
	com : context;
	t : basic_types;
	g : typer_globals;
	mutable bypass_accessor : int;
	mutable meta : metadata;
	mutable this_stack : texpr list;
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
	mutable type_params : (string * t) list;
	mutable get_build_infos : unit -> (module_type * t list * class_field list) option;
	(* per-function *)
	mutable curfield : tclass_field;
	mutable untyped : bool;
	mutable in_function : bool;
	mutable in_loop : bool;
	mutable in_display : bool;
	mutable in_macro : bool;
	mutable macro_depth : int;
	mutable curfun : current_fun;
	mutable ret : t;
	mutable locals : (string, tvar) PMap.t;
	mutable opened : anon_status ref list;
	mutable vthis : tvar option;
	mutable in_call_args : bool;
	(* events *)
	mutable on_error : typer -> string -> pos -> unit;
	memory_marker : float array;
}
exception Forbid_package of (string * path * pos) * pos list * string

exception WithTypeError of error_msg * pos

let memory_marker = [|Unix.time()|]

let make_call_ref : (typer -> texpr -> texpr list -> t -> ?force_inline:bool -> pos -> texpr) ref = ref (fun _ _ _ _ ?force_inline:bool _ -> die "" __LOC__)
let type_expr_ref : (?mode:access_mode -> typer -> expr -> WithType.t -> texpr) ref = ref (fun ?(mode=MGet) _ _ _ -> die "" __LOC__)
let type_block_ref : (typer -> expr list -> WithType.t -> pos -> texpr) ref = ref (fun _ _ _ _ -> die "" __LOC__)
let unify_min_ref : (typer -> texpr list -> t) ref = ref (fun _ _ -> die "" __LOC__)
let unify_min_for_type_source_ref : (typer -> texpr list -> WithType.with_type_source option -> t) ref = ref (fun _ _ _ -> die "" __LOC__)
let analyzer_run_on_expr_ref : (Common.context -> texpr -> texpr) ref = ref (fun _ _ -> die "" __LOC__)

let pass_name = function
	| PBuildModule -> "build-module"
	| PBuildClass -> "build-class"
	| PConnectField -> "connect-field"
	| PTypeField -> "type-field"
	| PCheckConstraint -> "check-constraint"
	| PForce -> "force"
	| PFinal -> "final"

let display_error ctx msg p = match ctx.com.display.DisplayMode.dms_error_policy with
	| DisplayMode.EPShow | DisplayMode.EPIgnore -> ctx.on_error ctx msg p
	| DisplayMode.EPCollect -> add_diagnostics_message ctx.com msg p DisplayTypes.DiagnosticsKind.DKCompilerError DisplayTypes.DiagnosticsSeverity.Error

let make_call ctx e el t p = (!make_call_ref) ctx e el t p

let type_expr ?(mode=MGet) ctx e with_type = (!type_expr_ref) ~mode ctx e with_type

let unify_min ctx el = (!unify_min_ref) ctx el
let unify_min_for_type_source ctx el src = (!unify_min_for_type_source_ref) ctx el src

let make_static_this c p =
	let ta = mk_anon ~fields:c.cl_statics (ref (Statics c)) in
	mk (TTypeExpr (TClassDecl c)) ta p

let make_static_field_access c cf t p =
	let ethis = make_static_this c p in
	mk (TField (ethis,(FStatic (c,cf)))) t p

let make_static_call ctx c cf map args t p =
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	let map t = map (apply_params cf.cf_params monos t) in
	let ef = make_static_field_access c cf (map cf.cf_type) p in
	make_call ctx ef args (map t) p

let raise_or_display ctx l p =
	if ctx.untyped then ()
	else if ctx.in_call_args then raise (WithTypeError(Unify l,p))
	else display_error ctx (error_msg (Unify l)) p

let raise_or_display_message ctx msg p =
	if ctx.in_call_args then raise (WithTypeError (Custom msg,p))
	else display_error ctx msg p

let unify ctx t1 t2 p =
	try
		Type.unify t1 t2
	with
		Unify_error l ->
			raise_or_display ctx l p

let unify_raise ctx t1 t2 p =
	try
		Type.unify t1 t2
	with
		Unify_error l ->
			(* no untyped check *)
			raise (Error (Unify l,p))

let save_locals ctx =
	let locals = ctx.locals in
	(fun() -> ctx.locals <- locals)

let add_local ctx k n t p =
	let v = alloc_var k n t p in
	if Define.defined ctx.com.defines Define.WarnVarShadowing && n <> "_" then begin
		try
			let v' = PMap.find n ctx.locals in
			(* ignore std lib *)
			if not (List.exists (ExtLib.String.starts_with p.pfile) ctx.com.std_path) then begin
				ctx.com.warning "This variable shadows a previously declared variable" p;
				ctx.com.warning "Previous variable was here" v'.v_pos
			end
		with Not_found ->
			()
	end;
	ctx.locals <- PMap.add n v ctx.locals;
	v

let display_identifier_error ctx ?prepend_msg msg p =
	let prepend = match prepend_msg with Some s -> s ^ " " | _ -> "" in
	display_error ctx (prepend ^ msg) p

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
		display_error ctx ("\"" ^ (StringHelper.s_escape (String.concat "." pack)) ^ "\" is not a valid package name:") p;
		display_error ctx msg p

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
	(* ensure that our generated local does not mask an existing one *)
	let rec loop n =
		let nv = (if n = 0 then gen_local_prefix else gen_local_prefix ^ string_of_int n) in
		if PMap.mem nv ctx.locals then
			loop (n+1)
		else
			nv
	in
	add_local ctx VGenerated (loop 0) t p

let is_gen_local v =
	String.unsafe_get v.v_name 0 = String.unsafe_get gen_local_prefix 0

let delay ctx p f =
	let rec loop = function
		| [] -> [p,[f]]
		| (p2,l) :: rest ->
			if p2 = p then
				(p, f :: l) :: rest
			else if p2 < p then
				(p2,l) :: loop rest
			else
				(p,[f]) :: (p2,l) :: rest
	in
	ctx.g.delayed <- loop ctx.g.delayed

let delay_late ctx p f =
	let rec loop = function
		| [] -> [p,[f]]
		| (p2,l) :: rest ->
			if p2 <= p then
				(p2,l) :: loop rest
			else
				(p,[f]) :: (p2,l) :: rest
	in
	ctx.g.delayed <- loop ctx.g.delayed

let rec flush_pass ctx p (where:string) =
	match ctx.g.delayed with
	| (p2,l) :: rest when p2 <= p ->
		(match l with
		| [] ->
			ctx.g.delayed <- rest;
		| f :: l ->
			ctx.g.delayed <- (p2,l) :: rest;
			f());
		flush_pass ctx p where
	| _ ->
		()

let make_pass ctx f = f

let init_class_done ctx =
	ctx.pass <- PTypeField

let exc_protect ?(force=true) ctx f (where:string) =
	let r = ref (lazy_available t_dynamic) in
	r := lazy_wait (fun() ->
		try
			let t = f r in
			r := lazy_available t;
			t
		with
			| Error (m,p) ->
				raise (Fatal_error ((error_msg m),p))
	);
	if force then delay ctx PForce (fun () -> ignore(lazy_type r));
	r

let fake_modules = Hashtbl.create 0
let create_fake_module ctx file =
	let key = Path.UniqueKey.create file in
	let file = Path.get_full_path file in
	let mdep = (try Hashtbl.find fake_modules key with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = (["$DEP"],file);
			m_types = [];
			m_extra = module_extra file (Define.get_signature ctx.com.defines) (file_time file) MFake [];
		} in
		Hashtbl.add fake_modules key mdep;
		mdep
	) in
	Hashtbl.replace ctx.g.modules mdep.m_path mdep;
	mdep

let push_this ctx e = match e.eexpr with
	| TConst ((TInt _ | TFloat _ | TString _ | TBool _) as ct) ->
		(EConst (tconst_to_const ct),e.epos),fun () -> ()
	| _ ->
		ctx.this_stack <- e :: ctx.this_stack;
		let er = EMeta((Meta.This,[],e.epos), (EConst(Ident "this"),e.epos)),e.epos in
		er,fun () -> ctx.this_stack <- List.tl ctx.this_stack

let is_removable_field ctx f =
	has_class_field_flag f CfExtern || Meta.has Meta.Generic f.cf_meta
	|| (match f.cf_kind with
		| Var {v_read = AccRequire (s,_)} -> true
		| Method MethMacro -> not ctx.in_macro
		| _ -> false)

(** checks if we can access to a given class field using current context *)
let rec can_access ctx ?(in_overload=false) c cf stat =
	if (has_class_field_flag cf CfPublic) then
		true
	else if not in_overload && ctx.com.config.pf_overload && Meta.has Meta.Overload cf.cf_meta then
		true
	else if c == ctx.curclass then
		true
	else
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
	let cur_paths = ref [] in
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
				if is_constr
				then (match c.cl_constructor with
					| Some cf ->
						if allowed cf then true
						else if cf.cf_expr = None then false (* maybe it's an inherited auto-generated constructor *)
						else raise Not_found
					| _ -> false
				)
				else try allowed (PMap.find cf.cf_name (if stat then c.cl_statics else c.cl_fields)) with Not_found -> false
			)
			|| (match c.cl_super with
			| Some (csup,_) -> loop csup
			| None -> false)
		with Not_found -> false
	in
	loop c
	(* access is also allowed of we access a type parameter which is constrained to our (base) class *)
	|| (match c.cl_kind with
		| KTypeParameter tl ->
			List.exists (fun t -> match follow t with TInst(c,_) -> loop c | _ -> false) tl
		| _ -> false)
	|| (Meta.has Meta.PrivateAccess ctx.meta)

(** removes the first argument of the class field's function type and all its overloads *)
let prepare_using_field cf = match follow cf.cf_type with
	| TFun((_,_,tf) :: args,ret) ->
		let rec loop acc overloads = match overloads with
			| ({cf_type = TFun((_,_,tfo) :: args,ret)} as cfo) :: l ->
				let tfo = apply_params cfo.cf_params (List.map snd cfo.cf_params) tfo in
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

(* -------------- debug functions to activate when debugging typer passes ------------------------------- *)
(*/*

let delay_tabs = ref ""

let context_ident ctx =
	if Common.defined ctx.com Common.Define.CoreApi then
		" core "
	else if Common.defined ctx.com Common.Define.Macro then
		"macro "
	else
		"  out "

let debug ctx str =
	if Common.raw_defined ctx.com "cdebug" then begin
		let s = (context_ident ctx ^ string_of_int (String.length !delay_tabs) ^ " " ^ !delay_tabs ^ str) in
		match ctx.com.json_out with
		| None -> print_endline s
		| Some _ -> DynArray.add ctx.com.pass_debug_messages s
	end

let init_class_done ctx =
	debug ctx ("init_class_done " ^ s_type_path ctx.curclass.cl_path);
	init_class_done ctx

let ctx_pos ctx =
	let inf = s_type_path ctx.m.curmod.m_path in
	let inf = (match snd ctx.curclass.cl_path with "" -> inf | n when n = snd ctx.m.curmod.m_path -> inf | n -> inf ^ "." ^ n) in
	let inf = (match ctx.curfield.cf_name with "" -> inf | n -> inf ^ ":" ^ n) in
	inf

let pass_infos ctx p =
	let inf = pass_name p ^ " ("  ^ ctx_pos ctx ^ ")" in
	let inf = if ctx.pass > p then inf ^ " ??CURPASS=" ^ pass_name ctx.pass else inf in
	inf

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
	debug ctx ("add " ^ inf)

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
	debug ctx ("add late " ^ inf)

let pending_passes ctx =
	let rec loop acc = function
		| (p,l) :: pl when p < ctx.pass -> loop (acc @ l) pl
		| _ -> acc
	in
	match loop [] ctx.g.debug_delayed with
	| [] -> ""
	| l -> " ??PENDING[" ^ String.concat ";" (List.map (fun (_,i,_) -> i) l) ^ "]"

let display_error ctx msg p =
	debug ctx ("ERROR " ^ msg);
	display_error ctx msg p

let make_pass ?inf ctx f =
	let inf = (match inf with None -> pass_infos ctx ctx.pass | Some inf -> inf) in
	(fun v ->
		debug ctx ("run " ^ inf ^ pending_passes ctx);
		let old = !delay_tabs in
		delay_tabs := !delay_tabs ^ "\t";
		let t = (try
			f v
		with
			| Fatal_error (e,p) ->
				delay_tabs := old;
				raise (Fatal_error (e,p))
			| exc when not (Common.raw_defined ctx.com "stack") ->
				debug ctx ("FATAL " ^ Printexc.to_string exc);
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
		debug ctx ("flush " ^ pass_name p ^ "(" ^ where ^ ")");
		delay_tabs := !delay_tabs ^ "\t";
		loop();
		delay_tabs := old;
		debug ctx "flush-done";
	| _ ->
		()

let make_where ctx where =
	where ^ " (" ^ ctx_pos ctx ^ ")"

let exc_protect ?(force=true) ctx f (where:string) =
	let r = ref (lazy_available t_dynamic) in
	r := lazy_wait (make_pass ~inf:(make_where ctx where) ctx (fun() ->
		try
			let t = f r in
			r := lazy_available t;
			t
		with
			| Error (m,p) ->
				raise (Fatal_error ((error_msg m),p))
	));
	if force then delay ctx PForce (fun () -> ignore(lazy_type r));
	r

*/*)
(* --------------------------------------------------- *)


