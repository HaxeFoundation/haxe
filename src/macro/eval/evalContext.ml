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
open EvalValue
open EvalHash

type env_kind =
	| EKLocalFunction of int
	| EKMethod of int * int
	| EKDelayed

type env = {
	mutable leave_pos : pos;
	kind : env_kind;
	locals : value array;
	captures : value ref array;
}

type builtins = {
	mutable instance_builtins : (int * value) list IntMap.t;
	mutable static_builtins : (int * value) list IntMap.t;
	constructor_builtins : (int,value list -> value) Hashtbl.t;
	empty_constructor_builtins : (int,unit -> value) Hashtbl.t;
}

type context = {
	ctx_id : int;
	is_macro : bool;
	builtins : builtins;
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
	environments : env Stack.t;
	exception_stack : env Stack.t;
}

let get_ctx_ref : (unit -> context) ref = ref (fun() -> assert false)
let get_ctx () = (!get_ctx_ref)()
let select ctx = get_ctx_ref := (fun() -> ctx)

(* Exceptions *)

exception RunTimeException of value * env list * pos

let vstring s =
	let proto = (get_ctx()).string_prototype in
	vinstance {
		ifields = [|vint (Rope.length s)|];
		iproto = proto;
		ikind = IString(s,lazy (Rope.to_string s))
	}

let stack_to_list stack =
	let l = DynArray.create () in
	Stack.iter (fun env -> DynArray.add l env) stack;
	DynArray.to_list l

let throw v p =
	let ctx = get_ctx() in
	Stack.clear ctx.exception_stack;
	if not (Stack.is_empty ctx.environments) then (Stack.top ctx.environments).leave_pos <- p;
	raise (RunTimeException(v,stack_to_list ctx.environments,p))

let exc v = throw v null_pos

let exc_string str = exc (vstring (Rope.of_string str))

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

(* Environment handling *)

let create_environment kind num_locals num_captures = {
	leave_pos = null_pos;
	kind = kind;
	locals = Array.make num_locals vnull;
	captures = Array.make num_captures (ref vnull);
}

let push_environment ctx kind num_locals num_captures =
	let env = create_environment kind num_locals num_captures in
	Stack.push env ctx.environments;
	env

let pop_environment ctx =
	Stack.pop ctx.environments

(* Prototypes *)

let get_static_prototype_raise ctx path =
	IntMap.find path ctx.static_prototypes

let get_static_prototype ctx path p =
	try get_static_prototype_raise ctx path
	with Not_found -> Error.error (Printf.sprintf "[%i] Type not found: %s" ctx.ctx_id (rev_hash_s path)) p

let get_static_prototype_as_value ctx path p =
	vprototype (get_static_prototype ctx path p)

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