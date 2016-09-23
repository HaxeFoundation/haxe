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

open Globals
open Ast
open Common
open Type
open Error

type with_type =
	| NoValue
	| Value
	| WithType of t

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
	mutable get_build_infos : unit -> (module_type * t list * class_field list) option;
	delayed_macros : (unit -> unit) DynArray.t;
	mutable global_using : (tclass * pos) list;
	(* api *)
	do_inherit : typer -> Type.tclass -> pos -> (bool * placed_type_path) -> bool;
	do_create : Common.context -> typer;
	do_macro : typer -> macro_mode -> path -> string -> expr list -> pos -> expr option;
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
exception Forbid_package of (string * path * pos) * pos list * string

exception WithTypeError of error_msg * pos

let make_call_ref : (typer -> texpr -> texpr list -> t -> pos -> texpr) ref = ref (fun _ _ _ _ _ -> assert false)
let type_expr_ref : (typer -> expr -> with_type -> texpr) ref = ref (fun _ _ _ -> assert false)
let type_module_type_ref : (typer -> module_type -> t list option -> pos -> texpr) ref = ref (fun _ _ _ _ -> assert false)
let unify_min_ref : (typer -> texpr list -> t) ref = ref (fun _ _ -> assert false)
let match_expr_ref : (typer -> expr -> (expr list * expr option * expr option * pos) list -> (expr option * pos) option -> with_type -> pos -> texpr) ref = ref (fun _ _ _ _ _ _ -> assert false)
let get_pattern_locals_ref : (typer -> expr -> Type.t -> (string, tvar * pos) PMap.t) ref = ref (fun _ _ _ -> assert false)
let get_constructor_ref : (typer -> tclass -> t list -> pos -> (t * tclass_field)) ref = ref (fun _ _ _ _ -> assert false)
let cast_or_unify_ref : (typer -> t -> texpr -> pos -> texpr) ref = ref (fun _ _ _ _ -> assert false)
let find_array_access_raise_ref : (typer -> tabstract -> tparams -> texpr -> texpr option -> pos -> (tclass_field * t * t * texpr * texpr option)) ref = ref (fun _ _ _ _ _ _ -> assert false)
let analyzer_run_on_expr_ref : (Common.context -> texpr -> texpr) ref = ref (fun _ _ -> assert false)
let merge_core_doc_ref : (typer -> tclass -> unit) ref = ref (fun _ _ -> assert false)

let pass_name = function
	| PBuildModule -> "build-module"
	| PBuildClass -> "build-class"
	| PTypeField -> "type-field"
	| PCheckConstraint -> "check-constraint"
	| PForce -> "force"
	| PFinal -> "final"

let display_error ctx msg p = match ctx.com.display.DisplayMode.dms_error_policy with
	| DisplayMode.EPShow | DisplayMode.EPIgnore -> ctx.on_error ctx msg p
	| DisplayMode.EPCollect -> add_diagnostics_message ctx.com msg p DisplayTypes.DiagnosticsSeverity.Error

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

let add_unbound_local ctx n t p =
	let v = add_local ctx n t p in
	v.v_meta <- (Meta.Unbound,[],null_pos) :: v.v_meta;
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
	let file = Path.unique_full_path file in
	let mdep = (try Hashtbl.find fake_modules file with Not_found ->
		let mdep = {
			m_id = alloc_mid();
			m_path = (["$DEP"],file);
			m_types = [];
			m_extra = module_extra file (Common.get_signature ctx.com) (file_time file) MFake [];
		} in
		Hashtbl.add fake_modules file mdep;
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
	Meta.has Meta.Extern f.cf_meta || Meta.has Meta.Generic f.cf_meta
	|| (match f.cf_kind with
		| Var {v_read = AccRequire (s,_)} -> true
		| Method MethMacro -> not ctx.in_macro
		| _ -> false)

(* -------------------------------------------------------------------------- *)
(* ABSTRACT CASTS *)

module AbstractCast = struct

	let cast_stack = ref []

	let make_static_call ctx c cf a pl args t p =
		if cf.cf_kind = Method MethMacro then begin
			match args with
				| [e] ->
					let e,f = push_this ctx e in
					ctx.with_type_stack <- (WithType t) :: ctx.with_type_stack;
					let e = match ctx.g.do_macro ctx MExpr c.cl_path cf.cf_name [e] p with
						| Some e -> type_expr ctx e Value
						| None ->  type_expr ctx (EConst (Ident "null"),p) Value
					in
					ctx.with_type_stack <- List.tl ctx.with_type_stack;
					f();
					e
				| _ -> assert false
		end else
			make_static_call ctx c cf (apply_params a.a_params pl) args t p

	let do_check_cast ctx tleft eright p =
		let recurse cf f =
			if cf == ctx.curfield || List.mem cf !cast_stack then error "Recursive implicit cast" p;
			cast_stack := cf :: !cast_stack;
			let r = f() in
			cast_stack := List.tl !cast_stack;
			r
		in
		let find a tl f =
			let tcf,cf = f() in
			if (Meta.has Meta.MultiType a.a_meta) then
				mk_cast eright tleft p
			else match a.a_impl with
				| Some c -> recurse cf (fun () ->
					let ret = make_static_call ctx c cf a tl [eright] tleft p in
					{ ret with eexpr = TMeta( (Meta.ImplicitCast,[],ret.epos), ret) }
				)
				| None -> assert false
		in
		if type_iseq tleft eright.etype then
			eright
		else begin
			let rec loop tleft tright = match follow tleft,follow tright with
			| TAbstract(a1,tl1),TAbstract(a2,tl2) ->
				begin try find a2 tl2 (fun () -> Abstract.find_to a2 tl2 tleft)
				with Not_found -> try find a1 tl1 (fun () -> Abstract.find_from a1 tl1 eright.etype tleft)
				with Not_found -> raise Not_found
				end
			| TAbstract(a,tl),_ ->
				begin try find a tl (fun () -> Abstract.find_from a tl eright.etype tleft)
				with Not_found ->
					let rec loop2 tcl = match tcl with
						| tc :: tcl ->
							if not (type_iseq tc tleft) then loop (apply_params a.a_params tl tc) tright
							else loop2 tcl
						| [] -> raise Not_found
					in
					loop2 a.a_from
				end
			| _,TAbstract(a,tl) ->
				begin try find a tl (fun () -> Abstract.find_to a tl tleft)
				with Not_found ->
					let rec loop2 tcl = match tcl with
						| tc :: tcl ->
							if not (type_iseq tc tright) then loop tleft (apply_params a.a_params tl tc)
							else loop2 tcl
						| [] -> raise Not_found
					in
					loop2 a.a_to
				end
			| _ ->
				raise Not_found
			in
			loop tleft eright.etype
		end

	let cast_or_unify_raise ctx tleft eright p =
		try
			(* can't do that anymore because this might miss macro calls (#4315) *)
			(* if ctx.com.display <> DMNone then raise Not_found; *)
			do_check_cast ctx tleft eright p
		with Not_found ->
			unify_raise ctx eright.etype tleft p;
			eright

	let cast_or_unify ctx tleft eright p =
		try
			cast_or_unify_raise ctx tleft eright p
		with Error (Unify l,p) ->
			raise_or_display ctx l p;
			eright

	let find_array_access_raise ctx a pl e1 e2o p =
		let is_set = e2o <> None in
		let ta = apply_params a.a_params pl a.a_this in
		let rec loop cfl = match cfl with
			| [] -> raise Not_found
			| cf :: cfl ->
				let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
				let map t = apply_params a.a_params pl (apply_params cf.cf_params monos t) in
				let check_constraints () =
					List.iter2 (fun m (name,t) -> match follow t with
						| TInst ({ cl_kind = KTypeParameter constr },_) when constr <> [] ->
							List.iter (fun tc -> match follow m with TMono _ -> raise (Unify_error []) | _ -> Type.unify m (map tc) ) constr
						| _ -> ()
					) monos cf.cf_params;
				in
				match follow (map cf.cf_type) with
				| TFun([(_,_,tab);(_,_,ta1);(_,_,ta2)],r) as tf when is_set ->
					begin try
						Type.unify tab ta;
						let e1 = cast_or_unify_raise ctx ta1 e1 p in
						let e2o = match e2o with None -> None | Some e2 -> Some (cast_or_unify_raise ctx ta2 e2 p) in
						check_constraints();
						cf,tf,r,e1,e2o
					with Unify_error _ | Error (Unify _,_) ->
						loop cfl
					end
				| TFun([(_,_,tab);(_,_,ta1)],r) as tf when not is_set ->
					begin try
						Type.unify tab ta;
						let e1 = cast_or_unify_raise ctx ta1 e1 p in
						check_constraints();
						cf,tf,r,e1,None
					with Unify_error _ | Error (Unify _,_) ->
						loop cfl
					end
				| _ -> loop cfl
		in
		loop a.a_array

	let find_array_access ctx a tl e1 e2o p =
		try find_array_access_raise ctx a tl e1 e2o p
		with Not_found -> match e2o with
			| None ->
				error (Printf.sprintf "No @:arrayAccess function accepts argument of %s" (s_type (print_context()) e1.etype)) p
			| Some e2 ->
				error (Printf.sprintf "No @:arrayAccess function accepts arguments of %s and %s" (s_type (print_context()) e1.etype) (s_type (print_context()) e2.etype)) p

	let find_multitype_specialization com a pl p =
		let m = mk_mono() in
		let tl = match Meta.get Meta.MultiType a.a_meta with
			| _,[],_ -> pl
			| _,el,_ ->
				let relevant = Hashtbl.create 0 in
				List.iter (fun e ->
					let rec loop f e = match fst e with
						| EConst(Ident s) ->
							Hashtbl.replace relevant s f
						| EMeta((Meta.Custom ":followWithAbstracts",_,_),e1) ->
							loop Abstract.follow_with_abstracts e1;
						| _ ->
							error "Type parameter expected" (pos e)
					in
					loop (fun t -> t) e
				) el;
				let tl = List.map2 (fun (n,_) t ->
					try
						(Hashtbl.find relevant n) t
					with Not_found ->
						if not (has_mono t) then t
						else t_dynamic
				) a.a_params pl in
				if com.platform = Globals.Js && a.a_path = ([],"Map") then begin match tl with
					| t1 :: _ ->
						let rec loop stack t =
							if List.exists (fun t2 -> fast_eq t t2) stack then
								t
							else begin
								let stack = t :: stack in
								match follow t with
								| TAbstract ({ a_path = [],"Class" },_) ->
									error (Printf.sprintf "Cannot use %s as key type to Map because Class<T> is not comparable" (s_type (print_context()) t1)) p;
								| TEnum(en,tl) ->
									PMap.iter (fun _ ef -> ignore(loop stack ef.ef_type)) en.e_constrs;
									Type.map (loop stack) t
								| t ->
									Type.map (loop stack) t
							end
						in
						ignore(loop [] t1)
					| _ -> assert false
				end;
				tl
		in
		let _,cf =
			try
				Abstract.find_to a tl m
			with Not_found ->
				let at = apply_params a.a_params pl a.a_this in
				let st = s_type (print_context()) at in
				if has_mono at then
					error ("Type parameters of multi type abstracts must be known (for " ^ st ^ ")") p
				else
					error ("Abstract " ^ (s_type_path a.a_path) ^ " has no @:to function that accepts " ^ st) p;
		in
		cf, follow m

	let handle_abstract_casts ctx e =
		let rec loop ctx e = match e.eexpr with
			| TNew({cl_kind = KAbstractImpl a} as c,pl,el) ->
				if not (Meta.has Meta.MultiType a.a_meta) then begin
					(* This must have been a @:generic expansion with a { new } constraint (issue #4364). In this case
					   let's construct the underlying type. *)
					match Abstract.get_underlying_type a pl with
					| TInst(c,tl) as t -> {e with eexpr = TNew(c,tl,el); etype = t}
					| _ -> error ("Cannot construct " ^ (s_type (print_context()) (TAbstract(a,pl)))) e.epos
				end else begin
					(* a TNew of an abstract implementation is only generated if it is a multi type abstract *)
					let cf,m = find_multitype_specialization ctx.com a pl e.epos in
					let e = make_static_call ctx c cf a pl ((mk (TConst TNull) (TAbstract(a,pl)) e.epos) :: el) m e.epos in
					{e with etype = m}
				end
			| TCall({eexpr = TField(_,FStatic({cl_path=[],"Std"},{cf_name = "string"}))},[e1]) when (match follow e1.etype with TAbstract({a_impl = Some _},_) -> true | _ -> false) ->
				begin match follow e1.etype with
					| TAbstract({a_impl = Some c} as a,tl) ->
						begin try
							let cf = PMap.find "toString" c.cl_statics in
							make_static_call ctx c cf a tl [e1] ctx.t.tstring e.epos
						with Not_found ->
							e
						end
					| _ ->
						assert false
				end
			| TCall(e1, el) ->
				begin try
					let rec find_abstract e t = match follow t,e.eexpr with
						| TAbstract(a,pl),_ when Meta.has Meta.MultiType a.a_meta -> a,pl,e
						| _,TCast(e1,None) -> find_abstract e1 e1.etype
						| _,TLocal {v_extra = Some(_,Some e')} ->
							begin match follow e'.etype with
							| TAbstract(a,pl) when Meta.has Meta.MultiType a.a_meta -> a,pl,mk (TCast(e,None)) e'.etype e.epos
							| _ -> raise Not_found
							end
						| _ -> raise Not_found
					in
					let rec find_field e1 =
						match e1.eexpr with
						| TCast(e2,None) ->
							{e1 with eexpr = TCast(find_field e2,None)}
						| TField(e2,fa) ->
							let a,pl,e2 = find_abstract e2 e2.etype in
							let m = Abstract.get_underlying_type a pl in
							let fname = field_name fa in
							let el = List.map (loop ctx) el in
							begin try
								let fa = quick_field m fname in
								let get_fun_type t = match follow t with
									| TFun(_,tr) as tf -> tf,tr
									| _ -> raise Not_found
								in
								let tf,tr = match fa with
									| FStatic(_,cf) -> get_fun_type cf.cf_type
									| FInstance(c,tl,cf) -> get_fun_type (apply_params c.cl_params tl cf.cf_type)
									| FAnon cf -> get_fun_type cf.cf_type
									| _ -> raise Not_found
								in
								let ef = mk (TField({e2 with etype = m},fa)) tf e2.epos in
								let ecall = make_call ctx ef el tr e.epos in
								if not (type_iseq ecall.etype e.etype) then
									mk (TCast(ecall,None)) e.etype e.epos
								else
									ecall
							with Not_found ->
								(* quick_field raises Not_found if m is an abstract, we have to replicate the 'using' call here *)
								match follow m with
								| TAbstract({a_impl = Some c} as a,pl) ->
									let cf = PMap.find fname c.cl_statics in
									make_static_call ctx c cf a pl (e2 :: el) e.etype e.epos
								| _ -> raise Not_found
							end
						| _ ->
							raise Not_found
					in
					find_field e1
				with Not_found ->
					Type.map_expr (loop ctx) e
				end
			| _ ->
				Type.map_expr (loop ctx) e
		in
		loop ctx e
end

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


