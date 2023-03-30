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
open EvalContext
open EvalValue
open EvalPrinting
open EvalHash
open EvalField

exception Break
exception Continue
exception Return of value
exception Sys_exit of int

let s_value_kind = function
	| VNull -> "VNull"
	| VTrue -> "VTrue"
	| VFalse -> "VFalse"
	| VInt32 _ -> "VInt32"
	| VInt64 _ -> "VInt64"
	| VUInt64 _ -> "VUInt64"
	| VFloat _ -> "VFloat"
	| VEnumValue _ -> "VEnumValue"
	| VObject _ -> "VObject"
	| VString _ -> "VString"
	| VArray _ -> "VArray"
	| VVector _ -> "VVector"
	| VInstance _ -> "VInstance"
	| VPrototype _ -> "VPrototype"
	| VFunction _ -> "VFunction"
	| VFieldClosure _ -> "VFieldClosure"
	| VLazy _ -> "VLazy"
	| VNativeString _ -> "VNativeString"
	| VHandle _ -> "VHandle"

let unexpected_value : 'a . value -> string -> 'a = fun v s ->
	let str = match v with
		| VNull -> "Null Access"
		| _ -> Printf.sprintf "Unexpected value %s(%s), expected %s" (s_value_kind v) (value_string v) s
	in
	exc_string str

let invalid_call_arg_number i i2 =
	exc_string (Printf.sprintf "Invalid number of call arguments: Expected %i, got %i" i i2)

let format_pos p =
	let error_printer file line = Printf.sprintf "%s:%d:" file line in
	Lexer.get_error_pos error_printer p

let uncaught_exception_string v p extra =
	(Printf.sprintf "%s : Uncaught exception %s%s" (format_pos p) (value_string v) extra)

let get_exc_error_stack ctx stack =
	let pl = List.map (fun env -> {pfile = rev_hash env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax}) stack in
	List.filter (fun p -> p <> null_pos) pl

let get_exc_error_message ctx v stack p =
	let pl = get_exc_error_stack ctx stack in
	match pl with
	| [] ->
		uncaught_exception_string v p ""
	| _ ->
		let sstack = String.concat "\n" (List.map (fun p -> Printf.sprintf "%s : Called from here" (format_pos p)) pl) in
		Printf.sprintf "%sUncaught exception %s\n%s" (if p = null_pos then "" else format_pos p ^ " : ") (value_string v) sstack

let build_exception_stack ctx env =
	let eval = env.env_eval in
	let rec loop acc env' =
		let acc = env' :: acc in
		if env == env' then
			List.rev acc
		else match env'.env_parent with
			| Some env -> loop acc env
			| None -> die "" __LOC__
	in
	let d = match eval.env with
	| Some env -> loop [] env
	| None -> []
	in
	ctx.exception_stack <- List.map (fun env ->
		{pfile = rev_hash env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax},env.env_info.kind
	) d

let handle_stack_overflow eval f =
	try f()
	with Stack_overflow -> exc_string "Stack overflow"

let catch_exceptions ctx ?(final=(fun() -> ())) f p =
	let reset_ctx =
		if !GlobalState.initialized then
			let prev = !GlobalState.get_ctx_ref in
			(fun() -> GlobalState.get_ctx_ref := prev)
		else
			(fun() -> ())
	in
	select ctx;
	let eval = get_eval ctx in
	let env = eval.env in
	let r = try
		let v = handle_stack_overflow eval f in
		reset_ctx();
		final();
		Some v
	with
	| RunTimeException(v,eval_stack,p') ->
		eval.caught_exception <- vnull;
		Option.may (build_exception_stack ctx) env;
		eval.env <- env;
		if is v key_haxe_macro_Error then begin
			let v1 = field v key_exception_message in
			let v2 = field v key_pos in
			let v3 = field v key_child_errors in
			let stack = match v3 with
				| VArray sub ->
						List.map (fun v ->
							if is v key_haxe_macro_Error then begin
								let v1 = field v key_exception_message in
								let v2 = match (field v key_pos) with
									| VInstance {ikind=IPos p} -> p
									| _ -> null_pos
								in
								(Error.Custom (value_string v1), v2)
							end else
								Error.typing_error "Something went wrong" null_pos
						) (EvalArray.to_list sub)
				| _ -> []
			in
			reset_ctx();
			final();
			match v1 with
				| VString s ->
					let p =
						match v2 with
						| VInstance { ikind = IPos p } -> p
						| VObject o ->
							(try
								let fields = object_fields o in
								let min = match List.assoc key_min fields with VInt32 i -> Int32.to_int i | _ -> raise Not_found
								and max = match List.assoc key_max fields with VInt32 i -> Int32.to_int i | _ -> raise Not_found
								and file = match List.assoc key_file fields with VString s -> s.sstring | _ -> raise Not_found
								in
								{ pmin = min; pmax = max; pfile = file }
							with Not_found ->
								null_pos
							)
						| _ -> null_pos
					in
					(* In some wild cases be get to the `else` block below which already adds stack to the error, so this displays it a second time... *)
					let stack = match eval_stack with
						| [] -> stack
						| _ ->
							let eval_stack = get_exc_error_stack ctx eval_stack in
							stack @ (List.map (fun p -> ((Error.Custom "Called from here"),p)) eval_stack)
					in
					(match stack with
						| [] ->
							raise (Error.Error (Error.Custom s.sstring,p,0))
						| _ ->
							raise (Error.Error (Stack ((Error.Custom ("Uncaught exception " ^ s.sstring),p) :: stack),p,0))
					);
				| _ ->
					Error.typing_error "Something went wrong" null_pos
		end else begin
			(* Careful: We have to get the message before resetting the context because toString() might access it. *)
			let stack = match eval_stack with
				| [] -> []
				| l when p' = null_pos -> l (* If the exception position is null_pos, we're "probably" in a built-in function. *)
				| _ :: l -> l (* Otherwise, ignore topmost frame position. *)
			in
			let stack = get_exc_error_stack ctx stack in
			reset_ctx();
			final();
			let p = if p' = null_pos then p else p' in
			raise (Error.Error (Stack (
				(Error.Custom ("Uncaught exception " ^ (value_string v)),p)
				:: (List.map (fun p -> ((Error.Custom "Called from here"),p)) stack)
			),p,0))
		end
	| MacroApi.Abort ->
		final();
		None
	| exc ->
		reset_ctx();
		final();
		raise exc
	in
	r
