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
open ProgramStage
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

type typer_module = {
	curmod : module_def;
	import_resolution : resolution_list;
	mutable own_resolution : resolution_list option;
	mutable enum_with_type : module_type option;
	mutable module_using : (tclass * pos) list;
	(* `using` statements to resolve, newest first (see ImportHandling.init_using) *)
	mutable pending_usings : (unit -> (tclass * pos) list) list;
	mutable import_statements : import list;
	mutable is_display_file : bool;
}

type typer_class = {
	curclass : tclass;
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

type function_mode =
	| FunFunction
	| FunCoroutine
	| FunNotFunction

type call_arg_context = {
	cac_in_overload : bool;
	mutable cac_body_capture : Message.t list ref;
}

(* A stage of [program] (docs/incremental-compilation.md, phase 3.1): its
   units, run in the order they were added, then its late
   units (those that must run once the others are done), in the order they
   were added. *)
type program_stage = {
	ps_units : (unit -> unit) Queue.t;
	ps_late : (unit -> unit) Queue.t;
}

type typer_globals = {
	(* the stages of [program], in [ProgramStage] order *)
	program : program_stage array;
	(* stages before this one have no unit left *)
	mutable program_min_stage : int;
	retain_meta : bool;
	mutable core_api : typer option;
	mutable macros : ((unit -> unit) * typer) option;
	mutable module_check_policies : (string list * module_check_policy list * bool) list;
	mutable global_using : (tclass * pos) list;
	(* Indicates that Typer.create() finished building this instance *)
	mutable complete : bool;
	mutable type_hints : (module_def_display * pos * t) list;
	mutable load_only_cached_modules : bool;
	mutable return_partial_type : bool;
	mutable t_dynamic_def : Type.t;
	mutable delayed_display : DisplayTypes.display_exception_kind option;
	root_typer : typer;
	(* api *)
	mutable continuation_api : ContTypes.continuation_api option;
	do_macro : typer -> macro_mode -> path -> string -> expr list -> pos -> macro_result;
	do_load_macro : typer -> bool -> path -> string -> pos -> ((string * bool * t) list * t * tclass * Type.tclass_field);
	do_load_module : ?origin:module_dep_origin -> typer -> path -> pos -> module_def;
	do_load_type_def : typer -> pos -> type_path -> module_type;
	get_build_info : typer -> module_type -> pos -> build_info;
	do_format_string : typer -> string -> pos -> Ast.expr;
	do_load_core_class : typer -> tclass -> tclass;
}

(* typer_expr holds information that is specific to a (function) expresssion, whereas typer_field
   is shared by local TFunctions. *)
and typer_expr = {
	curfun : current_fun;
	function_mode : function_mode;
	mutable ret : t;
	mutable opened : anon_status ref list;
	mutable monomorphs : (tmono * pos) list;
	mutable in_loop : bool;
	mutable bypass_accessor : int;
	mutable with_type_stack : WithType.t list;
	mutable call_argument_stack : expr list list;
	mutable macro_depth : int;
}

and typer_field = {
	curfield : tclass_field;
	mutable locals : (string, tvar) PMap.t;
	mutable vthis : tvar option;
	mutable untyped : bool;
	mutable meta : metadata;
	mutable in_display : bool;
	mutable in_call_args : bool;
}

and typer = {
	(* shared *)
	com : context;
	t : basic_types;
	g : typer_globals;
	m : typer_module;
	c : typer_class;
	f : typer_field;
	e : typer_expr;
	stage : stage;
	mutable type_params : type_params;
	mutable allow_inline : bool;
	mutable allow_transform : bool;
	(* events *)
	memory_marker : float array;
}

let stage_name = function
	| StageModules -> "modules"
	| StageTypes -> "types"
	| StageMembers -> "members"
	| StageSigs -> "sigs"
	| StageBodies -> "bodies"
	| StageConstraints -> "constraints"
	| StageChecks -> "checks"
	| StageFinal -> "final"

module TyperManager = struct
	let create ctx m c f e stage params =
		if stage < ctx.stage then die (Printf.sprintf "Bad context clone from %s(%s) to %s(%s)" (s_type_path ctx.m.curmod.m_path) (stage_name ctx.stage) (s_type_path m.curmod.m_path) (stage_name stage)) __LOC__;
		let new_ctx = {
			com = ctx.com;
			g = ctx.g;
			t = ctx.com.basic;
			m = m;
			c = c;
			f = f;
			e = e;
			stage = stage;
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
			in_call_args = false;
		}

	let create_ctx_e curfun function_mode =
		{
			curfun;
			function_mode;
			ret = t_dynamic;
			opened = [];
			monomorphs = [];
			in_loop = false;
			bypass_accessor = 0;
			with_type_stack = [];
			call_argument_stack = [];
			macro_depth = 0;
		}

	let clone_for_module ctx m =
		let ctx = create ctx m ctx.c ctx.f ctx.e StageModules [] in
		ctx.allow_transform <- true;
		ctx.allow_inline <- true;
		ctx

	let clone_for_class ctx c =
		let c = create_ctx_c c in
		let params = match c.curclass.cl_kind with KAbstractImpl a -> a.a_params | _ -> c.curclass.cl_params in
		create ctx ctx.m c ctx.f ctx.e StageTypes params

	let clone_for_enum ctx en =
		let c = create_ctx_c null_class in
		create ctx ctx.m c ctx.f ctx.e StageTypes en.e_params

	let clone_for_typedef ctx td =
		let c = create_ctx_c null_class in
		create ctx ctx.m c ctx.f ctx.e StageTypes td.t_params

	let clone_for_abstract ctx a =
		let c = create_ctx_c null_class in
		create ctx ctx.m c ctx.f ctx.e StageTypes a.a_params

	let clone_for_field ctx cf params =
		let f = create_ctx_f cf in
		create ctx ctx.m ctx.c f ctx.e StageTypes params

	let clone_for_enum_field ctx params =
		let f = create_ctx_f null_field in
		create ctx ctx.m ctx.c f ctx.e StageTypes params

	let clone_for_expr ctx curfun in_function =
		let e = create_ctx_e curfun in_function in
		begin match curfun with
		| FunMember | FunMemberAbstract | FunStatic | FunConstructor ->
			(* Monomorphs from field arguments and return types are created before
			   ctx.e is cloned, so they have to be absorbed here. A better fix might
			   be to clone ctx.e earlier, but that comes with its own challenges. *)
			e.monomorphs <- ctx.e.monomorphs;
			ctx.e.monomorphs <- []
		| FunMemberAbstractLocal | FunMemberClassLocal ->
			(* We don't need to do this for local functions because the cloning happens
			   earlier there. *)
			()
		end;
		create ctx ctx.m ctx.c ctx.f e StageBodies ctx.type_params

	let clone_for_type_params ctx params =
		create ctx ctx.m ctx.c ctx.f ctx.e ctx.stage params

	let clone_for_type_parameter_expression ctx =
		let f = create_ctx_f ctx.f.curfield in
		let e = create_ctx_e ctx.e.curfun FunNotFunction in
		create ctx ctx.m ctx.c f e StageBodies ctx.type_params

	let is_coroutine_context ctx =
		ctx.e.function_mode = FunCoroutine

	let is_function_context ctx = match ctx.e.function_mode with
		| FunFunction | FunCoroutine ->
			true
		| FunNotFunction ->
			false
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
	(* The position of the field name accessed on `fa_on`. *)
	fa_field_pos : pos option;
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
	let options = (Warning.from_meta ctx.f.curfield.cf_meta) @ (Warning.from_meta ctx.c.curclass.cl_meta) in
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
	ctx.e.monomorphs <- (mono,p) :: ctx.e.monomorphs;
	mono

let spawn_monomorph ctx p =
	TMono (spawn_monomorph' ctx p)

let monomorph_transaction ctx =
	let current = ctx.e.monomorphs in
	let known = List.fold_left (fun acc (m,_) -> match m.tm_type with
		| None -> (m,m.tm_down_constraints) :: acc
		| Some _ -> acc
	) [] current in
	(fun () ->
		List.iter (fun (m,constr) ->
			(match m.tm_type with Some _ -> m.tm_type <- None | None -> ());
			if constr != m.tm_down_constraints then m.tm_down_constraints <- constr;
		) known;
		ctx.e.monomorphs <- current
	)

let make_static_field_access c cf t p =
	let ethis = Texpr.Builder.make_static_this c p in
	mk (TField (ethis,(FStatic (c,cf)))) t p

(* The call arguments being typed, for the dynamic extent of their typing in the
   current domain. *)
let call_arg_contexts : (typer_globals,call_arg_context) DomainLocal.Scoped.t = DomainLocal.Scoped.create ()

let call_arg_context ctx = DomainLocal.Scoped.find call_arg_contexts ctx.g

let enter_call_args ctx ~in_overload =
	let old_in_call_args = ctx.f.in_call_args in
	ctx.f.in_call_args <- true;
	let restore = DomainLocal.Scoped.push call_arg_contexts ctx.g { cac_in_overload = in_overload; cac_body_capture = ref [] } in
	(fun () ->
		ctx.f.in_call_args <- old_in_call_args;
		restore ()
	)

let in_overload_call_args ctx = match call_arg_context ctx with
	| Some cac -> cac.cac_in_overload
	| None -> false

let reset_call_arg_body_capture ctx = match call_arg_context ctx with
	| Some cac -> let buf = ref [] in cac.cac_body_capture <- buf; buf
	| None -> ref []

let call_arg_body_capture ctx = match call_arg_context ctx with
	| Some cac -> Some cac.cac_body_capture
	| None -> None

let raise_with_type_error msg p =
	raise (WithTypeError (make_error (Custom msg) p))

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

let unify_raise a b = unify_raise_custom (default_unification_context()) a b

let save_locals ctx =
	let locals = ctx.f.locals in
	(fun() -> ctx.f.locals <- locals)

let add_local ctx k n t p =
	let v = alloc_var k n t p in
	if n <> "_" then begin
		match k with
		| VUser _ ->
			begin try
				let v' = PMap.find n ctx.f.locals in
				(* ignore std lib *)
				if not (List.exists (fun path -> ExtLib.String.starts_with (Pos.file p) (path#path)) ctx.com.class_paths#get_std_paths) then begin
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

let gen_local ctx t p =
	add_local ctx VGenerated gen_local_prefix t p

let is_gen_local v = match v.v_kind with
	| VGenerated ->
		true
	| _ ->
		false

let make_program_stage () = {
	ps_units = Queue.create ();
	ps_late = Queue.create ();
}

(* [program] (docs/incremental-compilation.md, phase 3.1) types the program by
   stages ([ProgramStage]): the structure of modules and types
   ([StageModules], [StageTypes], [StageMembers]), the bodies of fields
   ([StageBodies]), the type parameter constraint checks ([StageConstraints]),
   then the per-type checks and the forcing of lazy types ([StageChecks]). Each
   stage runs its units in the order they were added. A unit can add units to
   any stage, including earlier ones, which then run first: [run_stages]
   always runs the earliest stage with units left. *)

(* Adds a unit to stage [p]. *)
let schedule g (p : stage) f =
	let i = Obj.magic p in
	Queue.push f g.program.(i).ps_units;
	if i < g.program_min_stage then
		g.program_min_stage <- i

(* Adds a late unit to stage [p]: it runs once the stage has no
   other unit left. *)
let schedule_late g (p : stage) f =
	let i = Obj.magic p in
	Queue.push f g.program.(i).ps_late;
	if i < g.program_min_stage then
		g.program_min_stage <- i

(* -D typer-shuffle=<seed>: the units of a stage run in a random order, to check
   that typing does not depend on it (docs/incremental-compilation.md, phase 3). *)
let typer_shuffle : Random.State.t option ref = ref None

let pop_unit q = match !typer_shuffle with
	| Some st when Queue.length q > 1 ->
		let l = List.of_seq (Queue.to_seq q) in
		let n = Random.State.int st (List.length l) in
		Queue.clear q;
		List.iteri (fun i f -> if i <> n then Queue.push f q) l;
		List.nth l n
	| _ ->
		Queue.pop q

(* Runs the stages of [program] up to [p], until none of them has a unit left. *)
let rec run_stages g (p : stage) where =
	let last = Obj.magic p in
	let rec loop i =
		if i <= last then begin
			let s = g.program.(i) in
			if not (Queue.is_empty s.ps_units) then begin
				(pop_unit s.ps_units) ();
				loop g.program_min_stage
			end else if not (Queue.is_empty s.ps_late) then begin
				Queue.transfer s.ps_late s.ps_units;
				loop i
			end else begin
				(* Done with this stage (for now) *)
				g.program_min_stage <- i + 1;
				loop (i + 1)
			end
		end
	in
	loop g.program_min_stage

(* Adds the typing of the fields of a class whose type is inferred from their
   body to [program]: all of them are typed before any field body whose type
   is declared, which then cannot change the type of another field. *)
let add_program_sig_unit g f =
	schedule g StageSigs f

(* Adds the typing of the bodies of a class, or another field typing unit, to
   [program]. *)
let add_program_unit g f =
	schedule g StageBodies f

(* Adds a type parameter constraint check to [program]. *)
let add_program_constraint g f =
	schedule g StageConstraints f

(* Adds a check to [program]. *)
let add_program_check g f =
	schedule g StageChecks f

(* Completes the structure of the types loaded so far: what loading a module
   while typing a field demands. *)
let complete_structure g info =
	run_stages g StageMembers info

let make_lazy ctx t_proc f where =
	let r = make_unforced_lazy t_proc f where in
	add_program_check ctx (fun () -> ignore(lazy_type r));
	r

let is_forced_inline c cf =
	match c with
	| Some { cl_kind = KAbstractImpl _ } -> true
	| Some c when has_class_flag c CExtern -> true
	| _ when has_class_field_flag cf CfExtern -> true
	| _ -> false

let needs_inline ctx c cf =
	cf.cf_kind = Method MethInline && ctx.allow_inline && (ctx.com.doinline || is_forced_inline c cf)

(** checks if we can access to a given class field using current context *)
let can_access ctx c cf ?(check_prop=false) ?(is_setter=false) stat =
	let is_private_prop = check_prop && match cf.cf_kind with
		| Var { v_read = AccPrivateCall } when not is_setter -> true
		| Var { v_write = AccPrivateCall } when is_setter -> true
		| _ -> false in
	if (not is_private_prop && has_class_field_flag cf CfPublic) then
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
		begin match (TAccess.Header.super c) with
			| Some (csup,_) -> loop csup false
			| None -> ()
		end;
		List.iter (fun (c,_) -> loop c false) (TAccess.Header.implements c);
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
					match (TAccess.Members.constructor c) with
					| Some cf ->
						if allowed cf then true
						else if has_class_field_flag cf CfInheritedConstructor then false (* the access of the inherited one applies *)
						else raise Not_found
					| _ -> false
				) else
					try allowed (PMap.find cf.cf_name (if stat then (TAccess.Members.statics c) else (TAccess.Members.fields c)))
					with Not_found -> false
			)
			|| (match (TAccess.Header.super c) with
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
		List.iter (maybe_merge (TAccess.Members.fields c_core)) (TAccess.Members.ordered_fields c);
		List.iter (maybe_merge (TAccess.Members.statics c_core)) (TAccess.Members.ordered_statics c);
		begin match (TAccess.Members.constructor c),(TAccess.Members.constructor c_core) with
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

let relative_path ctx file =
	ctx.com.class_paths#relative_path file

let mk_infos_t =
	let fileName = ("fileName",null_pos,NoQuotes) in
	let lineNumber = ("lineNumber",null_pos,NoQuotes) in
	let className = ("className",null_pos,NoQuotes) in
	let methodName = ("methodName",null_pos,NoQuotes) in
	(fun ctx p params t ->
		let file = if ctx.com.is_macro_context then (Pos.file p) else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path (Pos.file p) else relative_path ctx (Pos.file p) in
		let line = Lexer.get_error_line p in
		let class_name = s_type_path ctx.c.curclass.cl_path in
		let fields =
			(fileName,Texpr.Builder.make_string ctx.com.basic file p) ::
			(lineNumber,Texpr.Builder.make_int ctx.com.basic line p) ::
			(className,Texpr.Builder.make_string ctx.com.basic class_name p) ::
			if ctx.f.curfield.cf_name = "" then
				params
			else
				(methodName,Texpr.Builder.make_string ctx.com.basic ctx.f.curfield.cf_name p) ::
				params
		in
		mk (TObjectDecl fields) t p
	)

let mk_infos ctx p params =
	let file = if ctx.com.is_macro_context then (Pos.file p) else if Common.defined ctx.com Define.AbsolutePath then Path.get_full_path (Pos.file p) else relative_path ctx (Pos.file p) in
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
	let uid = Atomic.make 0 in
	(fun() -> Atomic.fetch_and_add uid 1 + 1)

let make_stored_id_expr id p =
	(EConst (Int (string_of_int id, None))), p

let store_typed_expr com te p =
	let id = get_next_stored_typed_expr_id() in
	com.part_scope.stored_typed_exprs#add id te;
	let eid = make_stored_id_expr id p in
	id,((EMeta ((Meta.StoredTypedExpr,[],null_pos), eid)),p)

let push_this ctx e = match e.eexpr with
| TConst ((TInt _ | TFloat _ | TString _ | TBool _) as ct) ->
	(EConst (tconst_to_const ct),e.epos),fun () -> ()
| _ ->
	let id,er = store_typed_expr ctx.com e e.epos in
	er,fun () -> ctx.com.part_scope.stored_typed_exprs#remove id

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



