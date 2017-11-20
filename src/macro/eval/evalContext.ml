(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

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

type var_info = string

type scope = {
	pos : pos;
	(* The local start offset of the current scope. *)
	local_offset : int;
	(* The locals declared in the current scope. Maps variable IDs to local slots. *)
	mutable locals : (int,int) Hashtbl.t;
	(* The name of local variables. Maps local slots to variable names. Only filled in debug mode. *)
	mutable local_infos : (int,var_info) Hashtbl.t;
	(* The IDs of local variables. Maps variable names to variable IDs. *)
	mutable local_ids : (string,int) Hashtbl.t;
}

type env_kind =
	| EKLocalFunction of int
	| EKMethod of int * int
	| EKDelayed

type env_info = {
	static : bool;
	pfile : int;
	kind : env_kind;
	capture_infos : (int,var_info) Hashtbl.t;
}

type env_debug = {
	timer : unit -> unit;
	mutable scopes : scope list;
	mutable line : int;
	mutable expr : texpr;
}

type env = {
	env_info : env_info;
	env_debug : env_debug;
	mutable env_leave_pmin : int;
	mutable env_leave_pmax : int;
	mutable env_in_use : bool;
	env_locals : value array;
	env_captures : value ref array;
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
	mutable bpstate : breakpoint_state;
}

type debug_state =
	| DbgStart
	| DbgRunning
	| DbgWaiting
	| DbgContinue
	| DbgNext of int
	| DbgFinish of int

type builtins = {
	mutable instance_builtins : (int * value) list IntMap.t;
	mutable static_builtins : (int * value) list IntMap.t;
	constructor_builtins : (int,value list -> value) Hashtbl.t;
	empty_constructor_builtins : (int,unit -> value) Hashtbl.t;
}

type debug_socket = {
	addr : Unix.inet_addr;
	port : int;
	mutable socket : Unix.file_descr option;
}

type debug = {
	debug : bool;
	breakpoints : (int,(int,breakpoint) Hashtbl.t) Hashtbl.t;
	mutable support_debugger : bool;
	mutable debug_state : debug_state;
	mutable breakpoint : breakpoint;
	caught_types : (int,bool) Hashtbl.t;
	mutable environment_offset_delta : int;
	mutable debug_socket : debug_socket option;
}

type eval = {
	environments : env DynArray.t;
	mutable environment_offset : int;
}

type context = {
	ctx_id : int;
	is_macro : bool;
	record_stack : bool;
	detail_times : bool;
	builtins : builtins;
	debug : debug;
	mutable had_error : bool;
	mutable curapi : value MacroApi.compiler_api;
	mutable type_cache : Type.module_type IntMap.t;
	overrides : (Type.path * string,bool) Hashtbl.t;
	(* prototypes *)
	mutable string_prototype : vprototype;
	mutable instance_prototypes : vprototype IntMap.t;
	mutable static_prototypes : vprototype IntMap.t;
	mutable constructors : value Lazy.t IntMap.t;
	get_object_prototype : 'a . context -> (int * 'a) list -> vprototype * (int * 'a) list;
	(* eval *)
	eval : eval;
	mutable exception_stack : (pos * env_kind) list;
}

let get_ctx_ref : (unit -> context) ref = ref (fun() -> assert false)
let get_ctx () = (!get_ctx_ref)()
let select ctx = get_ctx_ref := (fun() -> ctx)

(* Misc *)

let get_eval ctx =
	ctx.eval

let rec kind_name ctx kind =
	let rec loop kind env_id = match kind, env_id with
		| EKLocalFunction i, 0 ->
			Printf.sprintf "localFunction%i" i
		| EKLocalFunction i, env_id ->
			let parent_id = env_id - 1 in
			let env = DynArray.get ctx.environments parent_id in
			Printf.sprintf "%s.localFunction%i" (loop env.env_info.kind parent_id) i
		| EKMethod(i1,i2),_ -> Printf.sprintf "%s.%s" (rev_hash_s i1) (rev_hash_s i2)
		| EKDelayed,_ -> "delayed"
	in
	loop kind ctx.environment_offset

let vstring s =
	VString (s,lazy (Rope.to_string s))

let vstring_direct (r,s) =
	VString(r,s)

let call_function f vl = match f,vl with
	| Fun0 f,_ -> f()
	| Fun1 f,[] -> f vnull
	| Fun1 f,(a :: _) -> f a
	| Fun2 f,[] -> f vnull vnull
	| Fun2 f,[a] -> f a vnull
	| Fun2 f,(a :: b :: _) -> f a b
	| Fun3 f,[] -> f vnull vnull vnull
	| Fun3 f,[a] -> f a vnull vnull
	| Fun3 f,[a;b] -> f a b vnull
	| Fun3 f,(a :: b :: c :: _) -> f a b c
	| Fun4 f,[] -> f vnull vnull vnull vnull
	| Fun4 f,[a] -> f a vnull vnull vnull
	| Fun4 f,[a;b] -> f a b vnull vnull
	| Fun4 f,[a;b;c] -> f a b c vnull
	| Fun4 f,(a :: b :: c :: d :: _) -> f a b c d
	| Fun5 f,[] -> f vnull vnull vnull vnull vnull
	| Fun5 f,[a] -> f a vnull vnull vnull vnull
	| Fun5 f,[a;b] -> f a b vnull vnull vnull
	| Fun5 f,[a;b;c] -> f a b c vnull vnull
	| Fun5 f,[a;b;c;d] -> f a b c d vnull
	| Fun5 f,(a :: b :: c :: d :: e :: _) -> f a b c d e
	| FunN f,_ -> f vl

let object_fields o =
	let fields = IntMap.fold (fun key vvalue acc -> (key,vvalue) :: acc) o.oextra [] in
	IntMap.fold (fun key index acc ->
		if IntMap.mem key o.oremoved then acc
		else (key,(o.ofields.(index))) :: acc
	) o.oproto.pinstance_names fields

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
	if not ctx.record_stack then
		[]
	else
		List.rev (DynArray.to_list (DynArray.sub ctx.eval.environments 0 ctx.eval.environment_offset))

let throw v p =
	let ctx = get_ctx() in
	let eval = get_eval ctx in
	if ctx.record_stack && eval.environment_offset > 0 then begin
		let env = DynArray.get eval.environments (eval.environment_offset - 1) in
		env.env_leave_pmin <- p.pmin;
		env.env_leave_pmax <- p.pmax;
	end;
	raise (RunTimeException(v,call_stack ctx,p))

let exc v = throw v null_pos

let exc_string str = exc (vstring (Rope.of_string str))

(* Environment handling *)

let no_timer = fun () -> ()
let empty_array = [||]
let no_expr = mk (TConst TNull) t_dynamic null_pos

let no_debug = {
	timer = no_timer;
	scopes = [];
	line = 0;
	expr = no_expr
}

let create_env_info static pfile kind capture_infos =
	let info = {
		static = static;
		kind = kind;
		pfile = pfile;
		capture_infos = capture_infos;
	} in
	info

let push_environment_debug ctx info num_locals num_captures =
	let eval = get_eval ctx in
	let timer = if ctx.detail_times then
		Common.timer ["macro";"execution";kind_name eval info.kind]
	else
		no_timer
	in
	let env = {
		env_info = info;
		env_leave_pmin = 0;
		env_leave_pmax = 0;
		env_in_use = false;
		env_debug = {
			timer = timer;
			scopes = [];
			line = 0;
			expr = no_expr;
		};
		env_locals = Array.make num_locals vnull;
		env_captures = Array.make num_captures (ref vnull);
	} in
	if eval.environment_offset = DynArray.length eval.environments then
		DynArray.add eval.environments env
	else
		DynArray.unsafe_set eval.environments eval.environment_offset env;
	eval.environment_offset <- eval.environment_offset + 1;
	env

let create_default_environment ctx info num_locals =
	{
		env_info = info;
		env_leave_pmin = 0;
		env_leave_pmax = 0;
		env_in_use = false;
		env_debug = no_debug;
		env_locals = Array.make num_locals vnull;
		env_captures = empty_array;
	}

let pop_environment_debug ctx env =
	let eval = get_eval ctx in
	eval.environment_offset <- eval.environment_offset - 1;
	env.env_debug.timer();
	()

let push_environment ctx info num_locals num_captures =
	if ctx.record_stack then
		push_environment_debug ctx info num_locals num_captures
	else {
		env_info = info;
		env_leave_pmin = 0;
		env_leave_pmax = 0;
		env_in_use = false;
		env_debug = no_debug;
		env_locals = Array.make num_locals vnull;
		env_captures = Array.make num_captures (ref vnull);
	}
[@@inline]

let pop_environment ctx env =
	if ctx.record_stack then pop_environment_debug ctx env else ()
[@@inline]

(* Prototypes *)

let get_static_prototype_raise ctx path =
	IntMap.find path ctx.static_prototypes

let get_static_prototype ctx path p =
	try get_static_prototype_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Type not found: %s" ctx.ctx_id (rev_hash_s path)) p

let get_static_prototype_as_value ctx path p =
	(get_static_prototype ctx path p).pvalue

let get_instance_prototype_raise ctx path =
	IntMap.find path ctx.instance_prototypes

let get_instance_prototype ctx path p =
	try get_instance_prototype_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Instance prototype not found: %s" ctx.ctx_id (rev_hash_s path)) p

let get_instance_constructor_raise ctx path =
	IntMap.find path ctx.constructors

let get_instance_constructor ctx path p =
	try get_instance_constructor_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Instance constructor not found: %s" ctx.ctx_id (rev_hash_s path)) p

let get_special_instance_constructor_raise ctx path =
	Hashtbl.find (get_ctx()).builtins.constructor_builtins path

let get_proto_field_index_raise proto name =
	IntMap.find name proto.pnames

let get_proto_field_index proto name =
	try get_proto_field_index_raise proto name
	with Not_found -> Error.error (Printf.sprintf "Field index for %s not found on prototype %s" (rev_hash_s name) (rev_hash_s proto.ppath)) null_pos

let get_instance_field_index_raise proto name =
	IntMap.find name proto.pinstance_names

let get_instance_field_index proto name =
	try get_instance_field_index_raise proto name
	with Not_found -> Error.error (Printf.sprintf "Field index for %s not found on prototype %s" (rev_hash_s name) (rev_hash_s proto.ppath)) null_pos
