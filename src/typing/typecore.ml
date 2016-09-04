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

open Common
open Type

type with_type =
	| NoValue
	| Value
	| WithType of t

type type_patch = {
	mutable tp_type : Ast.complex_type option;
	mutable tp_remove : bool;
	mutable tp_meta : Ast.metadata;
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

type typer_pass =
	| PBuildModule			(* build the module structure and setup module type parameters *)
	| PBuildClass			(* build the class structure *)
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
	mutable module_imports : Ast.import list;
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
	mutable global_metadata : (string list * Ast.metadata_entry * (bool * bool * bool)) list;
	mutable get_build_infos : unit -> (module_type * t list * Ast.class_field list) option;
	delayed_macros : (unit -> unit) DynArray.t;
	mutable global_using : (tclass * Ast.pos) list;
	(* api *)
	do_inherit : typer -> Type.tclass -> Ast.pos -> (bool * Ast.placed_type_path) -> bool;
	do_create : Common.context -> typer;
	do_macro : typer -> macro_mode -> path -> string -> Ast.expr list -> Ast.pos -> Ast.expr option;
	do_load_module : typer -> path -> pos -> module_def;
	do_optimize : typer -> texpr -> texpr;
	do_build_instance : typer -> module_type -> pos -> ((string * t) list * path * (t list -> t));
}

and typer = {
	(* shared *)
	com : context;
	t : basic_types;
	g : typer_globals;
	mutable meta : metadata;
	mutable this_stack : texpr list;
	mutable with_type_stack : with_type list;
	mutable call_argument_stack : Ast.expr list list;
	(* variable *)
	mutable pass : typer_pass;
	(* per-module *)
	mutable m : typer_module;
	mutable is_display_file : bool;
	(* per-class *)
	mutable curclass : tclass;
	mutable tthis : t;
	mutable type_params : (string * t) list;
	(* per-function *)
	mutable curfield : tclass_field;
	mutable untyped : bool;
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
}

type call_error =
	| Not_enough_arguments of (string * bool * t) list
	| Too_many_arguments
	| Could_not_unify of error_msg
	| Cannot_skip_non_nullable of string

and error_msg =
	| Module_not_found of path
	| Type_not_found of path * string
	| Unify of unify_error list
	| Custom of string
	| Unknown_ident of string
	| Stack of error_msg * error_msg
	| Call_error of call_error

exception Fatal_error of string * Ast.pos

exception Forbid_package of (string * path * pos) * pos list * string

exception Error of error_msg * pos

exception WithTypeError of error_msg * pos

let make_call_ref : (typer -> texpr -> texpr list -> t -> pos -> texpr) ref = ref (fun _ _ _ _ _ -> assert false)
let type_expr_ref : (typer -> Ast.expr -> with_type -> texpr) ref = ref (fun _ _ _ -> assert false)
let type_module_type_ref : (typer -> module_type -> t list option -> pos -> texpr) ref = ref (fun _ _ _ _ -> assert false)
let unify_min_ref : (typer -> texpr list -> t) ref = ref (fun _ _ -> assert false)
let match_expr_ref : (typer -> Ast.expr -> (Ast.expr list * Ast.expr option * Ast.expr option) list -> Ast.expr option option -> with_type -> Ast.pos -> texpr) ref = ref (fun _ _ _ _ _ _ -> assert false)
let get_pattern_locals_ref : (typer -> Ast.expr -> Type.t -> (string, tvar * pos) PMap.t) ref = ref (fun _ _ _ -> assert false)
let get_constructor_ref : (typer -> tclass -> t list -> Ast.pos -> (t * tclass_field)) ref = ref (fun _ _ _ _ -> assert false)
let cast_or_unify_ref : (typer -> t -> texpr -> Ast.pos -> texpr) ref = ref (fun _ _ _ _ -> assert false)
let find_array_access_raise_ref : (typer -> tabstract -> tparams -> texpr -> texpr option -> pos -> (tclass_field * t * t * texpr * texpr option)) ref = ref (fun _ _ _ _ _ _ -> assert false)
let analyzer_run_on_expr_ref : (Common.context -> texpr -> texpr) ref = ref (fun _ _ -> assert false)

let string_source t = match follow t with
	| TInst(c,_) -> List.map (fun cf -> cf.cf_name) c.cl_ordered_fields
	| TAnon a -> PMap.fold (fun cf acc -> cf.cf_name :: acc) a.a_fields []
	| TAbstract({a_impl = Some c},_) -> List.map (fun cf -> cf.cf_name) c.cl_ordered_statics
	| _ -> []

let short_type ctx t =
	let tstr = s_type ctx t in
	if String.length tstr > 150 then String.sub tstr 0 147 ^ "..." else tstr

let unify_error_msg ctx = function
	| Cannot_unify (t1,t2) ->
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Invalid_field_type s ->
		"Invalid type for field " ^ s ^ " :"
	| Has_no_field (t,n) ->
		StringError.string_error n (string_source t) (short_type ctx t ^ " has no field " ^ n)
	| Has_no_runtime_field (t,n) ->
		s_type ctx t ^ "." ^ n ^ " is not accessible at runtime"
	| Has_extra_field (t,n) ->
		short_type ctx t ^ " has extra field " ^ n
	| Invalid_kind (f,a,b) ->
		(match a, b with
		| Var va, Var vb ->
			let name, stra, strb = if va.v_read = vb.v_read then
				"setter", s_access false va.v_write, s_access false vb.v_write
			else if va.v_write = vb.v_write then
				"getter", s_access true va.v_read, s_access true vb.v_read
			else
				"access", "(" ^ s_access true va.v_read ^ "," ^ s_access false va.v_write ^ ")", "(" ^ s_access true vb.v_read ^ "," ^ s_access false vb.v_write ^ ")"
			in
			"Inconsistent " ^ name ^ " for field " ^ f ^ " : " ^ stra ^ " should be " ^ strb
		| _ ->
			"Field " ^ f ^ " is " ^ s_kind a ^ " but should be " ^ s_kind b)
	| Invalid_visibility n ->
		"The field " ^ n ^ " is not public"
	| Not_matching_optional n ->
		"Optional attribute of parameter " ^ n ^ " differs"
	| Cant_force_optional ->
		"Optional parameters can't be forced"
	| Invariant_parameter _ ->
		"Type parameters are invariant"
	| Constraint_failure name ->
		"Constraint check failure for " ^ name
	| Missing_overload (cf, t) ->
		cf.cf_name ^ " has no overload for " ^ s_type ctx t
	| Unify_custom msg ->
		msg

let rec error_msg = function
	| Module_not_found m -> "Type not found : " ^ Ast.s_type_path m
	| Type_not_found (m,t) -> "Module " ^ Ast.s_type_path m ^ " does not define type " ^ t
	| Unify l ->
		let ctx = print_context() in
		String.concat "\n" (List.map (unify_error_msg ctx) l)
	| Unknown_ident s -> "Unknown identifier : " ^ s
	| Custom s -> s
	| Stack (m1,m2) -> error_msg m1 ^ "\n" ^ error_msg m2
	| Call_error err -> s_call_error err

and s_call_error = function
	| Not_enough_arguments tl ->
		let pctx = print_context() in
		"Not enough arguments, expected " ^ (String.concat ", " (List.map (fun (n,_,t) -> n ^ ":" ^ (short_type pctx t)) tl))
	| Too_many_arguments -> "Too many arguments"
	| Could_not_unify err -> error_msg err
	| Cannot_skip_non_nullable s -> "Cannot skip non-nullable argument " ^ s

let pass_name = function
	| PBuildModule -> "build-module"
	| PBuildClass -> "build-class"
	| PTypeField -> "type-field"
	| PCheckConstraint -> "check-constraint"
	| PForce -> "force"
	| PFinal -> "final"

let display_error ctx msg p = match ctx.com.display.DisplayMode.dms_error_policy with
	| DisplayMode.EPShow | DisplayMode.EPIgnore -> ctx.on_error ctx msg p
	| DisplayMode.EPCollect -> add_diagnostics_message ctx.com msg p DiagnosticsSeverity.Error

let error msg p = raise (Error (Custom msg,p))

let make_call ctx e el t p = (!make_call_ref) ctx e el t p

let type_expr ctx e with_type = (!type_expr_ref) ctx e with_type

let unify_min ctx el = (!unify_min_ref) ctx el

let match_expr ctx e cases def with_type p = !match_expr_ref ctx e cases def with_type p

let make_static_this c p =
	let ta = TAnon { a_fields = c.cl_statics; a_status = ref (Statics c) } in
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

let add_local ctx n t p =
	let v = alloc_var n t p in
	ctx.locals <- PMap.add n v ctx.locals;
	v

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
	add_local ctx (loop 0) t p

let is_gen_local v =
	String.unsafe_get v.v_name 0 = String.unsafe_get gen_local_prefix 0

let not_opened = ref Closed
let mk_anon fl = TAnon { a_fields = fl; a_status = not_opened; }

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

let exc_protect ctx f (where:string) =
	let rec r = ref (fun() ->
		try
			f r
		with
			| Error (m,p) ->
				raise (Fatal_error ((error_msg m),p))
	) in
	r

let fake_modules = Hashtbl.create 0
let create_fake_module ctx file =
	let file = Common.unique_full_path file in
	let mdep = (try Hashtbl.find fake_modules file with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = (["$DEP"],file);
			m_types = [];
			m_extra = module_extra file (Common.get_signature ctx.com) (file_time file) MFake;
		} in
		Hashtbl.add fake_modules file mdep;
		mdep
	) in
	Hashtbl.replace ctx.g.modules mdep.m_path mdep;
	mdep

let push_this ctx e = match e.eexpr with
	| TConst ((TInt _ | TFloat _ | TString _ | TBool _) as ct) ->
		(Ast.EConst (tconst_to_const ct),e.epos),fun () -> ()
	| _ ->
		ctx.this_stack <- e :: ctx.this_stack;
		let er = Ast.EMeta((Ast.Meta.This,[],e.epos), (Ast.EConst(Ast.Ident "this"),e.epos)),e.epos in
		er,fun () -> ctx.this_stack <- List.tl ctx.this_stack

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
	if Common.raw_defined ctx.com "cdebug" then prerr_endline (context_ident ctx ^ !delay_tabs ^ str)

let init_class_done ctx =
	debug ctx ("init_class_done " ^ Ast.s_type_path ctx.curclass.cl_path);
	init_class_done ctx

let ctx_pos ctx =
	let inf = Ast.s_type_path ctx.m.curmod.m_path in
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

let exc_protect ctx f (where:string) =
	let f = make_pass ~inf:(make_where ctx where) ctx f in
	let rec r = ref (fun() ->
		try
			f r
		with
			| Error (m,p) ->
				raise (Fatal_error (error_msg m,p))
	) in
	r

*/*)
(* --------------------------------------------------- *)


