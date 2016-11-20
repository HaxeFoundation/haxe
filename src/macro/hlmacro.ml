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
open Type
open Hlinterp

type value = Hlinterp.value

type extern_api = {
	pos : Globals.pos;
	get_com : unit -> Common.context;
	get_type : string -> Type.t option;
	get_module : string -> Type.t list;
	after_typing : (module_type list -> unit) -> unit;
	on_generate : (Type.t list -> unit) -> unit;
	after_generate : (unit -> unit) -> unit;
	on_type_not_found : (string -> value) -> unit;
	parse_string : string -> Globals.pos -> bool -> Ast.expr;
	type_expr : Ast.expr -> Type.texpr;
	resolve_type  : Ast.complex_type -> Globals.pos -> t;
	type_macro_expr : Ast.expr -> Type.texpr;
	store_typed_expr : Type.texpr -> Ast.expr;
	allow_package : string -> unit;
	type_patch : string -> string -> bool -> string option -> unit;
	meta_patch : string -> string -> string option -> bool -> unit;
	set_js_generator : (value -> unit) -> unit;
	get_local_type : unit -> t option;
	get_expected_type : unit -> t option;
	get_call_arguments : unit -> Ast.expr list option;
	get_local_method : unit -> string;
	get_local_imports : unit -> Ast.import list;
	get_local_using : unit -> tclass list;
	get_local_vars : unit -> (string, Type.tvar) PMap.t;
	get_build_fields : unit -> value;
	get_pattern_locals : Ast.expr -> Type.t -> (string,Type.tvar * Globals.pos) PMap.t;
	define_type : value -> unit;
	define_module : string -> value list -> ((string * Globals.pos) list * Ast.import_mode) list -> Ast.type_path list -> unit;
	module_dependency : string -> string -> bool -> unit;
	current_module : unit -> module_def;
	mutable current_macro_module : unit -> module_def;
	delayed_macro : int -> (unit -> (unit -> value));
	use_cache : unit -> bool;
	format_string : string -> Globals.pos -> Ast.expr;
	cast_or_unify : Type.t -> texpr -> Globals.pos -> Type.texpr;
	add_global_metadata : string -> string -> (bool * bool * bool) -> unit;
	add_module_check_policy : string list -> int list -> bool -> int -> unit;
}

type context = {
	com : Common.context; (* macro one *)
	mutable gen : Genhl.context option;
	interp : Hlinterp.context;
	types : (Type.path,int) Hashtbl.t;
	mutable is_reused : bool;
	mutable curapi : extern_api;
	mutable on_reused : (unit -> bool) list;
	mutable has_error : bool;
}

exception Invalid_expr
exception Abort
exception Error of string * Globals.pos list

let debug = true (* TODO !!! set to false for speed++ ! *)

let get_ctx_ref = ref (fun() -> assert false)
let get_ctx() : context = (!get_ctx_ref)()

let setup() =
	Globals.macro_platform := Globals.Hl

let select ctx =
	get_ctx_ref := (fun() -> ctx)

let error_handler ctx v stack =
	let make_pos st =
		let file, line = Hlinterp.make_stack ctx.interp st in
		let low = line land 0xFFFFF in
		{
			Globals.pfile = file;
			Globals.pmin = low;
			Globals.pmax = low + (line lsr 20);
		}
	in
	(*let rec loop o =
		if o == ctx.error_proto then true else match o.oproto with None -> false | Some p -> loop p
	in
	(match v with
	| VObject o when loop o ->
		(match get_field o (hash "message"), get_field o (hash "pos") with
		| VObject msg, VAbstract (APos pos) ->
			(match get_field msg h_s with
			| VString msg -> raise (Error.Error (Error.Custom msg,pos))
			| _ -> ());
		| _ -> ());
	| _ -> ());*)
	raise (Error (Hlinterp.vstr ctx.interp v Hlcode.HDyn,List.map make_pos stack))

let create com api =
	let ctx = {
		com = com;
		gen = None;
		interp = Hlinterp.create debug;
		curapi = api;
		types = Hashtbl.create 0;
		on_reused = [];
		is_reused = true;
		has_error = false;
	} in
	select ctx;
	Hlinterp.set_error_handler ctx.interp (error_handler ctx);
	ctx

let init ctx =
	if ctx.gen = None then ctx.gen <- Some (Genhl.create_context ctx.com true false)

let set_error ctx e =
	ctx.has_error <- e

let add_types ctx types ready =
	let types = List.filter (fun t -> match t with
		| TAbstractDecl a when not (Meta.has Meta.CoreType a.a_meta) ->
			(* A @:native on an abstract causes the implementation class and the abstract
			   to have the same path. Let's skip all abstracts so this doesn't matter. *)
			false
		| _ ->
			let path = Type.t_path t in
			if Hashtbl.mem ctx.types path then false else begin
				Hashtbl.add ctx.types path (Type.t_infos t).mt_module.m_id;
				true;
			end
	) types in
	List.iter ready types;
	if ctx.gen = None then init ctx;
	match ctx.gen with
	| None -> assert false
	| Some gen ->
		Genhl.add_types gen types;
		if debug then Genhl.check gen;
		let code = Genhl.build_code gen types None in
		if debug then begin
			try
				Hlinterp.check code
			with Failure s ->
				let ch = open_out_bin "hlcode.txt" in
				Hlcode.dump (fun s -> output_string ch (s ^ "\n")) code;
				close_out ch;
				failwith s
		end;
		Hlinterp.add_code ctx.interp code

let do_reuse ctx api =
	ctx.is_reused <- false;
	ctx.curapi <- api

let can_reuse ctx types =
	let has_old_version t =
		let inf = Type.t_infos t in
		try
			Hashtbl.find ctx.types inf.mt_path <> inf.mt_module.m_id
		with Not_found ->
			false
	in
	if List.exists has_old_version types then
		false
	else if ctx.is_reused then
		true
	else if not (List.for_all (fun f -> f()) ctx.on_reused) then
		false
	else begin
		ctx.is_reused <- true;
		true;
	end

let catch_errors ctx ?(final=(fun() -> ())) f =
	let prev = get_ctx() in (* switch context in case we have an older one, see #5676 *)
	select ctx;
	try
		let v = f() in
		final();
		select prev;
		Some v
	with Error _ as e ->
		final();
		select prev;
		raise e
	|  Abort ->
		final();
		select prev;
		None

let call_path ctx cpath (f:string) args api =
	if ctx.has_error then
		None
	else let old = ctx.curapi in
	ctx.curapi <- api;
	let gid = Genhl.resolve_class_global (match ctx.gen with None -> assert false | Some ctx -> ctx) (String.concat "." cpath) in
	let gval = ctx.interp.Hlinterp.t_globals.(gid) in
	let fval = Hlinterp.dyn_get_field ctx.interp gval f Hlcode.HDyn in
	match fval with
	| Hlinterp.VClosure (f,None) ->
		catch_errors ~final:(fun() -> ctx.curapi <- old) ctx (fun() -> call_wrap ctx.interp f args)
	| _ ->
		prerr_endline (Hlinterp.vstr_d ctx.interp gval);
		assert false

let vnull = VNull
let vbool b = VBool b
let fun1 f : value = assert false

let exc_string msg =
	Hlinterp.throw_msg (get_ctx()).interp msg

let compiler_error msg pos =
	assert false (* TODO : raise haxe.macro.Error(msg,pos) *)

let call ctx _ callb args pos =
	assert false

let eval_expr ctx (e:texpr) =
	assert false

let eval_delayed _ _ =
	assert false (* macro - in - macro *)

let make_const e =
	assert false

let dec_string (e:value) : string = assert false
let dec_array (e:value) = assert false
let decode_type_def (td:value) = assert false
let decode_tdecl (td:value) = assert false
let decode_expr (e:value) : Ast.expr = assert false
let decode_texpr (e:value) : texpr = assert false
let decode_field (f:value) = assert false
let decode_ctype (c:value) = assert false
let decode_type (t:value) = assert false

let enc_string s : value = assert false
let enc_array ar : value = assert false
let enc_obj fl : value = assert false
let encode_type t : value = assert false
let encode_expr e : value = assert false
let encode_texpr e : value = assert false
let encode_field f : value = assert false
