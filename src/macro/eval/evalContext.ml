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
open EvalValue
open EvalHash
open EvalString

type var_info = {
	vi_name : string;
	vi_pos : pos;
	vi_generated : bool;
}

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
	| EKEntrypoint

(* Compile-time information for environments. This information is static for all
   environments of the same kind, e.g. all environments of a specific method. *)
type env_info = {
	(* If false, the environment has a this-context. *)
	static : bool;
	(* Hash of the source file of this environment. *)
	pfile : int;
	(* Hash of the unique source file of this environment. *)
	pfile_unique : int;
	(* The environment kind. *)
	kind : env_kind;
	(* The name of capture variables. Maps local slots to variable names. Only filled in debug mode. *)
	capture_infos : (int,var_info) Hashtbl.t;
	(* The number of local variables. *)
	num_locals : int;
	(* The number of capture variables. *)
	num_captures : int;
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
	mutable debug_expr : string;
	(* The current expression position being executed *)
	mutable debug_pos : pos;
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
	env_captures : value array;
	(* Map of extra variables added while debugging. Keys are hashed variable names. *)
	mutable env_extra_locals : value IntMap.t;
	(* The parent of the current environment, if exists. *)
	env_parent : env option;
	(** Exeucution stack depth *)
	env_stack_depth : int;
	env_eval : eval;
}

and eval = {
	mutable env : env option;
	thread : vthread;
	(* The threads current debug state *)
	mutable debug_state : debug_state;
	(* The currently active breakpoint. Set to a dummy value initially. *)
	mutable breakpoint : breakpoint;
	(* Map of all types that are currently being caught. Updated by `emit_try`. *)
	caught_types : (int,bool) Hashtbl.t;
	(* The most recently caught exception. Used by `debug_loop` to avoid getting stuck. *)
	mutable caught_exception : value;
	(* The value which was last returned. *)
	mutable last_return : value option;
	(* The debug channel used to synchronize with the debugger. *)
	debug_channel : unit Event.channel;
}

and debug_state =
	| DbgRunning
	| DbgWaiting
	| DbgStep
	| DbgNext of env * pos
	| DbgFinish of env (* parent env *)

and breakpoint_state =
	| BPEnabled
	| BPDisabled
	| BPHit

and breakpoint_column =
	| BPAny
	| BPColumn of int

and breakpoint = {
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

type builtins = {
	mutable instance_builtins : (int * value) list IntMap.t;
	mutable static_builtins : (int * value) list IntMap.t;
	constructor_builtins : (int,value list -> value) Hashtbl.t;
	empty_constructor_builtins : (int,unit -> value) Hashtbl.t;
}

type debug_scope_info = {
	ds_expr : string;
	ds_return : value option;
}

type context_reference =
	| StackFrame of env
	| Scope of scope * env
	| CaptureScope of (int,var_info) Hashtbl.t * env
	| DebugScope of debug_scope_info * env
	| Value of value * env
	| Toplevel
	| NoSuchReference

class eval_debug_context = object(self)
	val lut =
		let d = DynArray.create() in
		DynArray.add d Toplevel;
		d

	val mutex = Mutex.create()

	method private add reference =
		Mutex.lock mutex;
		DynArray.add lut reference;
		let i = DynArray.length lut - 1 in
		Mutex.unlock mutex;
		i

	method add_stack_frame env =
		self#add (StackFrame env)

	method add_scope scope env =
		self#add (Scope(scope,env))

	method add_capture_scope h env =
		self#add (CaptureScope(h,env))

	method add_value v env =
		self#add (Value(v,env))

	method add_debug_scope scope env =
		self#add (DebugScope(scope,env))

	method get id =
		try DynArray.get lut id with _ -> NoSuchReference

end

class static_prototypes = object(self)
	val mutable prototypes : vprototype IntMap.t = IntMap.empty
	val mutable inits : (vprototype * (vprototype -> unit) list) IntMap.t = IntMap.empty

	method add proto =
		prototypes <- IntMap.add proto.ppath proto prototypes

	method remove path =
		inits <- IntMap.remove path inits;
		prototypes <- IntMap.remove path prototypes

	method reset =
		IntMap.iter (fun _ (proto, delays) -> List.iter (fun f -> f proto) delays) inits

	method add_init proto delays =
		inits <- IntMap.add proto.ppath (proto, delays) inits

	method get path =
		IntMap.find path prototypes
end

type exception_mode =
	| CatchAll
	| CatchUncaught
	| CatchNone

type debug_connection = {
	bp_stop : debug -> unit;
	exc_stop : debug -> value -> pos -> unit;
	send_thread_event : int -> string -> unit;
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
	(* The debugger socket *)
	mutable debug_socket : debug_socket option;
	(* The current exception mode *)
	mutable exception_mode : exception_mode;
	(* The debug context which manages scopes and variables. *)
	mutable debug_context : eval_debug_context;
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
	mutable static_prototypes : static_prototypes;
	mutable constructors : value Lazy.t IntMap.t;
	file_keys : Common.file_keys;
	get_object_prototype : 'a . context -> (int * 'a) list -> vprototype * (int * 'a) list;
	(* eval *)
	toplevel : value;
	eval : eval;
	mutable evals : eval IntMap.t;
	mutable exception_stack : (pos * env_kind) list;
	max_stack_depth : int;
}

module GlobalState = struct
	let get_ctx_ref : (unit -> context) ref = ref (fun() -> die "GlobalState.get_ctx_ref called before initialization" __LOC__)
	let initialized = ref false

	let sid : int ref = ref (-1)

	let debug : debug option ref = ref None
	let debugger_initialized : bool ref = ref false

	let stdlib : builtins option ref = ref None
	let macro_lib : (string,value) Hashtbl.t = Hashtbl.create 0

	let cleanup ctx =
		(* curapi holds a reference to the typing context which we don't want to persist. Let's unset it so the
		   context can be collected. *)
		ctx.curapi <- Obj.magic ""
end

let get_ctx () = (!GlobalState.get_ctx_ref)()
let select ctx =
	GlobalState.initialized := true;
	GlobalState.get_ctx_ref := (fun() -> ctx)

let s_debug_state = function
	| DbgRunning -> "DbgRunning"
	| DbgWaiting -> "DbgWaiting"
	| DbgStep -> "DbgStep"
	| DbgNext _ -> "DbgNext"
	| DbgFinish _ -> "DbgFinish"

(* Misc *)

let get_eval ctx =
	let id = Thread.id (Thread.self()) in
	if id = 0 then
		ctx.eval
	else
		try
			IntMap.find id ctx.evals
		with Not_found ->
			die "Cannot run Haxe code in a non-Haxe thread" __LOC__


let rec kind_name eval kind =
	let rec loop kind env = match kind with
		| EKMethod(i1,i2) ->
			Printf.sprintf "%s.%s" (rev_hash i1) (rev_hash i2)
		| EKLocalFunction i ->
			begin match env with
			| None -> Printf.sprintf "localFunction%i" i
			| Some env -> Printf.sprintf "%s.localFunction%i" (loop env.env_info.kind env.env_parent) i
			end
		| EKEntrypoint ->
			begin match env with
			| None -> "entrypoint"
			| Some env -> rev_hash env.env_info.pfile
			end
	in
	match eval.env with
	| None -> "toplevel"
	| Some env -> loop kind (Some env)

let call_function f vl = f vl

let object_fields o = match o.oproto with
	| OProto proto ->
		IntMap.fold (fun key index acc ->
			(key,(o.ofields.(index))) :: acc
		) proto.pinstance_names []
	| ODictionary d ->
		IntMap.fold (fun k v acc -> (k,v) :: acc) d []

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

let call_stack eval =
	let rec loop acc env =
		let acc = env :: acc in
		match env.env_parent with
		| Some env -> loop acc env
		| _ -> List.rev acc
	in
	match eval.env with
	| None -> []
	| Some env -> loop [] env

let throw v p =
	let ctx = get_ctx() in
	let eval = get_eval ctx in
	match eval.env with
	| Some env ->
		if p <> null_pos then begin
			env.env_leave_pmin <- p.pmin;
			env.env_leave_pmax <- p.pmax;
		end;
		raise_notrace (RunTimeException(v,call_stack eval,p))
	| None ->
		raise_notrace (RunTimeException(v,[],p))

let exc v = throw v null_pos

let exc_string str = exc (vstring (EvalString.create_ascii str))

let exc_string_p str p = throw (vstring (EvalString.create_ascii str)) p

let error_message = exc_string

let flush_core_context f =
	let ctx = get_ctx() in
	ctx.curapi.MacroApi.flush_context f

(* Environment handling *)

let no_timer = fun () -> ()
let empty_array = [||]
let no_expr = ""

let no_debug = {
	timer = no_timer;
	scopes = [];
	line = 0;
	debug_expr = no_expr;
	debug_pos = null_pos;
}

let create_env_info static pfile pfile_key kind capture_infos num_locals num_captures =
	let info = {
		static = static;
		kind = kind;
		pfile = hash pfile;
		pfile_unique = hash (Path.UniqueKey.to_string pfile_key);
		capture_infos = capture_infos;
		num_locals = num_locals;
		num_captures = num_captures;
	} in
	info

let push_environment ctx info =
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
	let locals = if info.num_locals = 0 then
		empty_array
	else
		Array.make info.num_locals vnull
	in
	let captures = if info.num_captures = 0 then
		empty_array
	else
		Array.make info.num_captures vnull
	in
	let stack_depth = match eval.env with
		| None -> 1;
		| Some env -> env.env_stack_depth + 1
	in
	let env = {
		env_info = info;
		env_leave_pmin = 0;
		env_leave_pmax = 0;
		env_debug = debug;
		env_locals = locals;
		env_captures = captures;
		env_extra_locals = IntMap.empty;
		env_parent = eval.env;
		env_eval = eval;
		env_stack_depth = stack_depth;
	} in
	eval.env <- Some env;
	begin match ctx.debug.debug_socket,env.env_info.kind with
		| Some socket,EKMethod(key_type,key_field) ->
			begin try
				let bp = Hashtbl.find ctx.debug.function_breakpoints (key_type,key_field) in
				if bp.fbpstate <> BPEnabled then raise Not_found;
				socket.connection.bp_stop ctx.debug;
				eval.debug_state <- DbgWaiting;
			with Not_found ->
				()
			end
		| _ ->
			()
	end;
	env

let pop_environment ctx env =
	let eval = env.env_eval in
	eval.env <- env.env_parent;
	env.env_debug.timer();
	()

(* Prototypes *)

let get_static_prototype_raise ctx path =
	ctx.static_prototypes#get path

let get_static_prototype ctx path p =
	try get_static_prototype_raise ctx path
	with Not_found -> Error.typing_error (Printf.sprintf "[%i] Type not found: %s" ctx.ctx_id (rev_hash path)) p

let get_static_prototype_as_value ctx path p =
	(get_static_prototype ctx path p).pvalue

let get_instance_prototype_raise ctx path =
	IntMap.find path ctx.instance_prototypes

let get_instance_prototype ctx path p =
	try get_instance_prototype_raise ctx path
	with Not_found -> Error.typing_error (Printf.sprintf "[%i] Instance prototype not found: %s" ctx.ctx_id (rev_hash path)) p

let get_instance_constructor_raise ctx path =
	IntMap.find path ctx.constructors

let get_instance_constructor ctx path p =
	try get_instance_constructor_raise ctx path
	with Not_found -> Error.typing_error (Printf.sprintf "[%i] Instance constructor not found: %s" ctx.ctx_id (rev_hash path)) p

let get_special_instance_constructor_raise ctx path =
	Hashtbl.find (get_ctx()).builtins.constructor_builtins path

let get_proto_field_index_raise proto name =
	IntMap.find name proto.pnames

let get_proto_field_index proto name =
	try get_proto_field_index_raise proto name
	with Not_found -> Error.typing_error (Printf.sprintf "Field index for %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) null_pos

let get_instance_field_index_raise proto name =
	IntMap.find name proto.pinstance_names

let get_instance_field_index proto name p =
	try get_instance_field_index_raise proto name
	with Not_found -> Error.typing_error (Printf.sprintf "Field index for %s not found on prototype %s" (rev_hash name) (rev_hash proto.ppath)) p

let is v path =
	if path = key_Dynamic then
		v <> vnull
	else match v with
	| VInt32 _ -> path = key_Int || path = key_Float
	| VFloat f -> path = key_Float || (path = key_Int && f = (float_of_int (int_of_float f)) && f <= 2147483647. && f >= -2147483648.)
	| VTrue | VFalse -> path = key_Bool
	| VPrototype {pkind = PClass _} -> path = key_Class
	| VPrototype {pkind = PEnum _} -> path = key_Enum
	| VEnumValue ve -> path = key_EnumValue || path = ve.epath
	| VString _ -> path = key_String
	| VArray _ -> path = key_Array
	| VVector _ -> path = key_eval_Vector
	| VInstance vi ->
		let has_interface path' =
			try begin match (get_static_prototype_raise (get_ctx()) path').pkind with
				| PClass interfaces -> List.mem path interfaces
				| _ -> false
			end with Not_found ->
				false
		in
		let rec loop proto =
			if path = proto.ppath || has_interface proto.ppath then true
			else begin match proto.pparent with
				| Some proto -> loop proto
				| None -> false
			end
		in
		loop vi.iproto
	| _ -> false