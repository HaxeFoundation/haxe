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
open Type
open EvalValue
open EvalHash
open EvalString

type var_info = string

type scope = {
	(* The position of the current scope. *)
	pos : pos;
	(* The local start offset of the current scope. *)
	local_offset : int;
	(* The locals declared in the current scope. Maps variable IDs to local slots. *)
	locals : (int,int) Hashtbl.t;
	(* The name of local variables. Maps local slots to variable names. Only filled in debug mode. *)
	local_infos : (int,var_info) Hashtbl.t;
	(* The IDs of local variables. Maps variable names to variable IDs. *)
	local_ids : (string,int) Hashtbl.t;
}

type env_kind =
	| EKLocalFunction of int
	| EKMethod of int * int
	| EKToplevel

(* Compile-time information for environments. This information is static for all
   environments of the same kind, e.g. all environments of a specific method. *)
type env_info = {
	(* If false, the environment has a this-context. *)
	static : bool;
	(* Hash of the source file of this environment. *)
	pfile : int;
	(* The environment kind. *)
	kind : env_kind;
	(* The name of capture variables. Maps local slots to variable names. Only filled in debug mode. *)
	capture_infos : (int,var_info) Hashtbl.t;
}

(* Per-environment debug information. These values are only modified while debugging. *)
type env_debug = {
	(* The timer function to execute when the environment finishes executing *)
	timer : unit -> unit;
	(* The current scope stack. *)
	mutable scopes : scope list;
	(* The current line being executed. This in conjunction with `env_info.pfile` is used to find breakpoints. *)
	mutable line : int;
	(* The current expression being executed *)
	mutable expr : texpr;
}

(* An environment in which code is executed. Environments are created whenever a function is called and when
   evaluating static inits. *)
type env = {
	(* The compile-time information for the current environment *)
	env_info : env_info;
	(* The debug information for the current environment *)
	env_debug : env_debug;
	(* The position at which the current environment was left, e.g. by a call. *)
	mutable env_leave_pmin : int;
	(* The position at which the current environment was left, e.g. by a call. *)
	mutable env_leave_pmax : int;
	(* The environment's local variables. Indices are determined during compile-time, or can be obtained
	   through `scope.locals` when debugging. *)
	env_locals : value array;
	(* The reference to the environment's captured local variables. Indices are determined during compile-time,
	   or can be obtained through `env_info.capture_infos`. *)
	env_captures : value ref array;
	(* Map of extra variables added while debugging. Keys are hashed variable names. *)
	mutable env_extra_locals : value IntMap.t;
	(* The parent of the current environment, if exists. All environments except EKToplevel have a parent. *)
	env_parent : env option;
}

type breakpoint_state =
	| BPEnabled
	| BPDisabled
	| BPHit

type breakpoint_column =
	| BPAny
	| BPColumn of int

type breakpoint = {
	bpid : int;
	bpfile : int;
	bpline : int;
	bpcolumn : breakpoint_column;
	bpcondition : Ast.expr option;
	mutable bpstate : breakpoint_state;
}

type function_breakpoint = {
	fbpid : int;
	mutable fbpstate : breakpoint_state;
}

type debug_state =
	| DbgStart
	| DbgRunning
	| DbgWaiting
	| DbgContinue
	| DbgNext of env * pos
	| DbgFinish of env (* parent env *)

type builtins = {
	mutable instance_builtins : (int * value) list IntMap.t;
	mutable static_builtins : (int * value) list IntMap.t;
	constructor_builtins : (int,value list -> value) Hashtbl.t;
	empty_constructor_builtins : (int,unit -> value) Hashtbl.t;
}

type exception_mode =
	| CatchAll
	| CatchUncaught
	| CatchNone

type debug_connection = {
	wait : context -> (env -> value) -> env -> value;
	bp_stop : context -> env -> unit;
	exc_stop : context -> value -> pos -> unit;
}

and debug_socket = {
	socket : Socket.t;
	connection : debug_connection;
}

(* Per-context debug information *)
and debug = {
	(* The registered breakpoints *)
	breakpoints : (int,(int,breakpoint) Hashtbl.t) Hashtbl.t;
	(* The registered function breakpoints *)
	function_breakpoints : ((int * int),function_breakpoint) Hashtbl.t;
	(* Whether or not debugging is supported. Has various effects on the amount of
	   data being retained at run-time. *)
	mutable support_debugger : bool;
	(* The current debug state. Managed by the debugger. *)
	mutable debug_state : debug_state;
	(* The currently active breakpoint. Set to a dummy value initially. *)
	mutable breakpoint : breakpoint;
	(* Map of all types that are currently being caught. Updated by `emit_try`. *)
	caught_types : (int,bool) Hashtbl.t;
	(* The debugger socket *)
	mutable debug_socket : debug_socket option;
	(* The current exception mode *)
	mutable exception_mode : exception_mode;
	(* The most recently caught exception. Used by `debug_loop` to avoid getting stuck. *)
	mutable caught_exception : value;
}

and eval = {
	mutable env : env;
}

and context = {
	ctx_id : int;
	is_macro : bool;
	detail_times : bool;
	builtins : builtins;
	debug : debug;
	mutable had_error : bool;
	mutable curapi : value MacroApi.compiler_api;
	mutable type_cache : Type.module_type IntMap.t;
	overrides : (Globals.path * string,bool) Hashtbl.t;
	(* prototypes *)
	mutable array_prototype : vprototype;
	mutable string_prototype : vprototype;
	mutable vector_prototype : vprototype;
	mutable instance_prototypes : vprototype IntMap.t;
	mutable static_prototypes : vprototype IntMap.t;
	mutable constructors : value Lazy.t IntMap.t;
	get_object_prototype : 'a . context -> (int * 'a) list -> vprototype * (int * 'a) list;
	mutable static_inits : (vprototype * (vprototype -> unit) list) IntMap.t;
	(* eval *)
	toplevel : value;
	eval : eval;
	evals : eval DynArray.t;
	mutable exception_stack : (pos * env_kind) list;
}

let get_ctx_ref : (unit -> context) ref = ref (fun() -> assert false)
let get_ctx () = (!get_ctx_ref)()
let select ctx = get_ctx_ref := (fun() -> ctx)

(* Misc *)

let get_eval ctx =
    let id = Thread.id (Thread.self()) in
    if id = 0 then ctx.eval else DynArray.unsafe_get ctx.evals id

let rec kind_name eval kind =
	let rec loop kind env = match kind with
		| EKMethod(i1,i2) ->
			Printf.sprintf "%s.%s" (rev_hash i1) (rev_hash i2)
		| EKLocalFunction i ->
			begin match env with
			| None -> Printf.sprintf "localFunction%i" i
			| Some env -> Printf.sprintf "%s.localFunction%i" (loop env.env_info.kind env.env_parent) i
			end
		| EKToplevel ->
			"toplevel"
	in
	loop kind (Some eval.env)

let call_function f vl = f vl

let object_fields o =
	IntMap.fold (fun key index acc ->
		(key,(o.ofields.(index))) :: acc
	) o.oproto.pinstance_names []

let instance_fields i =
	IntMap.fold (fun name key acc ->
		(name,i.ifields.(key)) :: acc
	) i.iproto.pinstance_names []

let proto_fields proto =
	IntMap.fold (fun name key acc ->
		(name,proto.pfields.(key)) :: acc
	) proto.pnames []

(* Exceptions *)

exception RunTimeException of value * env list * pos

let call_stack ctx =
	let eval = get_eval ctx in
	let rec loop acc env =
		let acc = env :: acc in
		match env.env_parent with
		| Some env when env.env_info.kind <> EKToplevel -> loop acc env
		| _ -> List.rev acc
	in
	loop [] eval.env

let throw v p =
	let ctx = get_ctx() in
	let eval = get_eval ctx in
	let env = eval.env in
	env.env_leave_pmin <- p.pmin;
	env.env_leave_pmax <- p.pmax;
	raise_notrace (RunTimeException(v,call_stack ctx,p))

let exc v = throw v null_pos

let exc_string str = exc (vstring (EvalString.create_ascii str))

let error_message = exc_string

let flush_core_context f =
	let ctx = get_ctx() in
	ctx.curapi.MacroApi.flush_context f

(* Environment handling *)

let no_timer = fun () -> ()
let empty_array = [||]
let no_expr = mk (TConst TNull) t_dynamic null_pos

let no_debug = {
	timer = no_timer;
	scopes = [];
	line = 0;
	expr = no_expr;
}

let null_env = {
	env_info = {
		static = true;
		pfile = EvalHash.hash "null-env";
		kind = EKToplevel;
		capture_infos = Hashtbl.create 0;
	};
	env_debug = no_debug;
	env_leave_pmin = 0;
	env_leave_pmax = 0;
	env_locals = [||];
	env_captures = [||];
	env_extra_locals = IntMap.empty;
	env_parent = None;
}

let create_env_info static pfile kind capture_infos =
	let info = {
		static = static;
		kind = kind;
		pfile = pfile;
		capture_infos = capture_infos;
	} in
	info

let push_environment ctx info num_locals num_captures =
	let eval = get_eval ctx in
	let timer = if ctx.detail_times then
		Timer.timer ["macro";"execution";kind_name eval info.kind]
	else
		no_timer
	in
	let debug = if ctx.debug.support_debugger || ctx.detail_times then
		{ no_debug with timer = timer }
	else
		no_debug
	in
	let locals = if num_locals = 0 then
		empty_array
	else
		Array.make num_locals vnull
	in
	let captures = if num_captures = 0 then
		empty_array
	else
		Array.make num_captures (ref vnull)
	in
	let env = {
		env_info = info;
		env_leave_pmin = 0;
		env_leave_pmax = 0;
		env_debug = debug;
		env_locals = locals;
		env_captures = captures;
		env_extra_locals = IntMap.empty;
		env_parent = Some eval.env;
	} in
	eval.env <- env;
	begin match ctx.debug.debug_socket,env.env_info.kind with
		| Some socket,EKMethod(key_type,key_field) ->
			begin try
				let bp = Hashtbl.find ctx.debug.function_breakpoints (key_type,key_field) in
				if bp.fbpstate <> BPEnabled then raise Not_found;
				socket.connection.bp_stop ctx env;
				ctx.debug.debug_state <- DbgWaiting;
			with Not_found ->
				()
			end
		| _ ->
			()
	end;
	env

let pop_environment ctx env =
	let eval = get_eval ctx in
	begin match env.env_parent with
		| Some env -> eval.env <- env
		| None -> assert false
	end;
	env.env_debug.timer();
	()

(* Prototypes *)

let get_static_prototype_raise ctx path =
	IntMap.find path ctx.static_prototypes

let get_static_prototype ctx path p =
	try get_static_prototype_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Type not found: %s" ctx.ctx_id (rev_hash path)) p

let get_static_prototype_as_value ctx path p =
	(get_static_prototype ctx path p).pvalue

let get_instance_prototype_raise ctx path =
	IntMap.find path ctx.instance_prototypes

let get_instance_prototype ctx path p =
	try get_instance_prototype_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Instance prototype not found: %s" ctx.ctx_id (rev_hash path)) p

let get_instance_constructor_raise ctx path =
	IntMap.find path ctx.constructors

let get_instance_constructor ctx path p =
	try get_instance_constructor_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Instance constructor not found: %s" ctx.ctx_id (rev_hash path)) p

let get_special_instance_constructor_raise ctx path =
	Hashtbl.find (get_ctx()).builtins.constructor_builtins path

let get_proto_field_index_raise proto name =
	IntMap.find name proto.pnames

let get_proto_field_index proto name =
	try get_proto_field_index_raise proto name
	with Not_found -> Error.error (Printf.sprintf "Field index for %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) null_pos

let get_instance_field_index_raise proto name =
	IntMap.find name proto.pinstance_names

let get_instance_field_index proto name p =
	try get_instance_field_index_raise proto name
	with Not_found -> Error.error (Printf.sprintf "Field index for %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) p
