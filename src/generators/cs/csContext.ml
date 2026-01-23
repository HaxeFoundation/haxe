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

(* Context types for C# code generation.
   These record types hold state during the code generation pass and are
   shared across multiple generator modules.

   Main types:
   - gen_context: Global state for the entire generation pass
   - expr_context: Local state for expression translation
   - cs_expr_result: Expression result with optional prefix statements

   Also provides context creation and utility functions. *)

open Globals
open CsAst
open CsTypeMapping
open Genshared

(* Generation context - holds state for the entire code generation pass *)
type gen_context = {
	com : Gctx.t;
	mutable generated_types : cs_type_def list;
	mutable closures_by_class : (path * cs_type_def list) list;
	mutable closure_count : int;
	mutable temp_count : int;
	invoke_signatures : (cs_type list * cs_type, unit) Hashtbl.t;
	mutable preprocessor : cs_type preprocessor;
}

(* Expression generation context - holds state for translating expressions *)
type expr_context = {
	gctx : gen_context;
	mutable local_vars : (int * string) list;
	mutable used_names : string list;
	mutable temp_count : int;
	mutable return_type : Type.t option;
	mutable current_class_path : path option;
	mutable current_method_name : string option;
	mutable origin_class_path : path option;
	mutable captured_vars : int list;
	mutable captures_this : bool;
	mutable type_params_in_scope : string list;
	mutable type_param_constraints : (string * cs_type list) list;
	mutable in_switch : bool;
	mutable loop_break_label : string option;
}

(* Result type for expressions that may need prefix statements *)
type cs_expr_result = {
	er_stmts : cs_stmt list;
	er_expr : cs_expr;
}

(* Create a new generation context *)
let create_context com = {
	com = com;
	generated_types = [];
	closures_by_class = [];
	closure_count = 0;
	temp_count = 0;
	invoke_signatures = Hashtbl.create 32;
	preprocessor = Obj.magic ();
}

(* Create a new expression context *)
let create_expr_context gctx = {
	gctx = gctx;
	local_vars = [];
	used_names = [];
	temp_count = 0;
	return_type = None;
	current_class_path = None;
	current_method_name = None;
	origin_class_path = None;
	captured_vars = [];
	captures_this = false;
	type_params_in_scope = [];
	type_param_constraints = [];
	in_switch = false;
	loop_break_label = None;
}

(* Add a closure to the list for its origin class *)
let add_closure_for_class gctx origin_class_path closure_def =
	let existing = try List.assoc origin_class_path gctx.closures_by_class with Not_found -> [] in
	gctx.closures_by_class <- (origin_class_path, closure_def :: existing) ::
		List.filter (fun (p, _) -> p <> origin_class_path) gctx.closures_by_class

(* Get closures for a specific class path *)
let get_closures_for_class gctx class_path =
	try List.assoc class_path gctx.closures_by_class with Not_found -> []

(* Generate a fresh temporary variable name *)
let fresh_temp ectx =
	let n = ectx.temp_count in
	ectx.temp_count <- n + 1;
	Printf.sprintf "_hx_tmp%d" n

(* Generate a unique closure class name based on current context *)
let generate_closure_name gctx ectx =
	let count = gctx.closure_count in
	gctx.closure_count <- count + 1;
	let base_name = match ectx.current_class_path, ectx.current_method_name with
		| Some (_, cname), Some mname -> Printf.sprintf "%s_%s" cname mname
		| Some (_, cname), None -> cname
		| None, Some mname -> mname
		| None, None -> "Closure"
	in
	Printf.sprintf "_hx_Closure_%s_%d" base_name count
