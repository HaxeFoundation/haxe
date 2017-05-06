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
open Ast
open EvalValue
open EvalContext
open EvalPrototype
open EvalExceptions
open EvalJit
open EvalJitContext
open EvalPrinting
open EvalMisc
open EvalHash
open EvalEncode
open EvalField
open MacroApi

(* Create *)

let sid = ref (-1)

let stdlib = ref None
let debug = ref None

let create com api is_macro =
	let t = Common.timer [(if is_macro then "macro" else "interp");"create"] in
	incr sid;
	let builtins = match !stdlib with
		| None ->
			let builtins = {
				static_builtins = IntMap.empty;
				instance_builtins = IntMap.empty;
				constructor_builtins = Hashtbl.create 0;
				empty_constructor_builtins = Hashtbl.create 0;
			} in
			EvalStdLib.init_standard_library builtins;
			stdlib := Some builtins;
			builtins
		| Some (builtins) ->
			builtins
	in
	let debug = match !debug with
		| None ->
			let support_debugger = Common.raw_defined com "interp-debugger" in
			let socket =
				try
					if not support_debugger then raise Exit;
					let fail msg =
						print_endline msg;
						raise Exit
					in
					let s = Common.raw_defined_value com "interp-debugger-socket" in
					let host,port = try ExtString.String.split s ":" with _ -> fail "Invalid host format, expected host:port" in
					let host = try Unix.inet_addr_of_string host with exc -> fail (Printexc.to_string exc) in
					let port = try int_of_string port with _ -> fail "Invalid port, expected int" in
					let socket = try (Unix.socket Unix.PF_INET Unix.SOCK_STREAM) 0 with exc -> fail (Printexc.to_string exc) in
					Unix.connect socket (Unix.ADDR_INET (host,port));
					print_endline "Created socket";
					Some {addr = host; port = port; socket = Some socket}
				with _ ->
					None
			in
			let debug' = {
				debug = com.Common.debug || support_debugger;
				breakpoints = Hashtbl.create 0;
				support_debugger = support_debugger;
				debug_state = DbgStart;
				breakpoint = EvalDebug.make_breakpoint 0 0 BPDisabled BPAny;
				caught_types = Hashtbl.create 0;
				environment_offset_delta = 0;
				debug_socket = socket;
			} in
			debug := Some debug';
			debug'
		| Some debug ->
			debug
	in
	let record_stack = (debug.debug && not (Common.raw_defined com "no-interp-stack")) || Common.raw_defined com "interp-stack" in
	let evals = DynArray.create () in
	let eval = {
		environments = DynArray.make 32;
		environment_offset = 0;
	} in
	DynArray.add evals eval;
	let rec ctx = {
		ctx_id = !sid;
		is_macro = is_macro;
		debug = debug;
		record_stack = record_stack;
		detail_times = Common.raw_defined com "interp-times";
		curapi = api;
		builtins = builtins;
		type_cache = IntMap.empty;
		overrides = Hashtbl.create 0;
		(* prototypes *)
		string_prototype = fake_proto key_String;
		static_prototypes = IntMap.empty;
		instance_prototypes = IntMap.empty;
		constructors = IntMap.empty;
		get_object_prototype = get_object_prototype;
		(* eval *)
		eval = eval;
		exception_stack = [];
	} in
	t();
	ctx

(* API for macroContext.ml *)

let eval_delayed ctx e =
	let jit,f = jit_expr ctx e in
	let info = create_env_info true (file_hash e.Type.epos.pfile) EKDelayed jit.capture_infos in
	fun () ->
		let env = push_environment ctx info jit.max_local_count (Hashtbl.length jit.captures) in
		match catch_exceptions ctx (fun () -> Std.finally (fun _ -> pop_environment ctx env) f env) e.Type.epos with
			| Some v -> v
			| None -> vnull

let call_path ctx path f vl api =
	ctx.curapi <- api;
	let path = match List.rev path with
		| [] -> assert false
		| name :: path -> List.rev path,name
	in
	catch_exceptions ctx (fun () ->
		let vtype = get_static_prototype_as_value ctx (path_hash path) api.pos in
		let vfield = field vtype (hash_s f) in
		call_value_on vtype vfield vl
	) api.pos

let value_signature v = Digest.string "" (* TODO: probably won't implement this... *)

let prepare_callback v n =
	match v with
	| VFunction _ | VFieldClosure _ ->
		let ctx = get_ctx() in
		(fun args -> match catch_exceptions ctx (fun() -> call_value v args) null_pos with
			| Some v -> v
			| None -> vnull)
	| _ ->
		raise Invalid_expr

let init ctx = ()

let setup get_api =
	let api = get_api (fun() -> (get_ctx()).curapi.get_com()) (fun() -> (get_ctx()).curapi) in
	List.iter (fun (n,v) -> match v with
		| VFunction(f,b) ->
			let v = match f with
				| Fun0 f -> VFunction (Fun0 (fun () -> try f () with Sys_error msg | Failure msg -> exc_string msg),b)
				| Fun1 f -> VFunction (Fun1 (fun a -> try f a with Sys_error msg | Failure msg -> exc_string msg),b)
				| Fun2 f -> VFunction (Fun2 (fun a b -> try f a b with Sys_error msg | Failure msg -> exc_string msg),b)
				| Fun3 f -> VFunction (Fun3 (fun a b c -> try f a b c with Sys_error msg | Failure msg -> exc_string msg),b)
				| Fun4 f -> VFunction (Fun4 (fun a b c d -> try f a b c d with Sys_error msg | Failure msg -> exc_string msg),b)
				| Fun5 f -> VFunction (Fun5 (fun a b c d e -> try f a b c d e with Sys_error msg | Failure msg -> exc_string msg),b)
				| FunN f -> VFunction (FunN (fun vl -> try f vl with Sys_error msg | Failure msg -> exc_string msg),b)
			in
			Hashtbl.replace EvalStdLib.macro_lib n v
		| _ -> assert false
	) api;
	Globals.macro_platform := Globals.Eval

let can_reuse ctx types = true

let do_reuse ctx api =
	ctx.curapi <- api

let set_error ctx e = () (* TODO: Figure out what this does. *)

let add_types ctx types ready =
	ignore(catch_exceptions ctx (fun () -> ignore(add_types ctx types ready)) null_pos)

let compiler_error msg pos =
	let vi = encode_instance key_haxe_macro_Error in
	match vi with
	| VInstance i ->
		set_instance_field i key_message (encode_string msg);
		set_instance_field i key_pos (encode_pos pos);
		exc vi
	| _ ->
		assert false

let rec value_to_expr v p =
	match v with
	| VNull -> (EConst (Ident "null"),p)
	| VTrue -> (EConst (Ident "true"),p)
	| VFalse -> (EConst (Ident "false"),p)
	| VInt32 i -> (EConst (Int (Int32.to_string i)),p)
	| VFloat f -> haxe_float f p
	| VInstance {ikind = IString(r,s)} -> (EConst (String (Lazy.force s)),p)
	| VInstance {ikind = IArray va} -> (EArrayDecl (List.map (fun v -> value_to_expr v p) (EvalArray.to_list va)),p)
	| VObject o -> (EObjectDecl (List.map (fun (k,v) -> ((rev_hash_s k,p),(value_to_expr v p))) (object_fields o)),p)
	| VEnumValue e ->
		let epath =
			let proto = get_static_prototype_raise (get_ctx()) e.epath in
			let first, rest = match (ExtString.String.nsplit (rev_hash_s proto.ppath) ".") with
				| n :: rest -> n, rest
				| _ -> assert false
			in
			let expr = List.fold_left (fun e f -> (EField (e,f), p)) ((EConst (Ident first)),p) rest in
			let name = match proto.pkind with
				| PEnum names -> List.nth names e.eindex
				| _ -> assert false
			in
			(EField (expr, name), p)
		in
		begin
			match e.eargs with
			| [||] -> epath
			| _ ->
				let args = List.map (fun v -> value_to_expr v p) (Array.to_list e.eargs) in
				(ECall (epath, args), p)
		end

	| _ -> exc_string ("Cannot convert " ^ (value_string v) ^ " to expr")

let encode_obj = encode_obj_s

let field v f = field v (EvalHash.hash_s f)

let value_string = value_string

let exc_string = exc_string

let eval_expr ctx e = eval_expr ctx key_questionmark key_questionmark e