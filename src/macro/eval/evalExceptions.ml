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
open EvalContext
open EvalValue
open EvalPrinting
open EvalHash

exception Break
exception Continue
exception Return of value

let s_value_kind = function
	| VNull -> "VNull"
	| VTrue -> "VTrue"
	| VFalse -> "VFalse"
	| VInt32 _ -> "VInt32"
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

let unexpected_value : 'a . value -> string -> 'a = fun v s ->
	let str = Printf.sprintf "Unexpected value %s(%s), expected %s" (s_value_kind v) (value_string v) s in
	exc_string str

let invalid_call_arg_number i i2 =
	exc_string (Printf.sprintf "Invalid number of call arguments: Expected %i, got %i" i i2)

let format_pos p =
	let error_printer file line = Printf.sprintf "%s:%d:" file line in
	Lexer.get_error_pos error_printer p

let uncaught_exception_string v p extra =
	(Printf.sprintf "%s: Uncaught exception %s%s" (format_pos p) (value_string v) extra)

let get_exc_error_message ctx v stack p =
	let pl = List.map (fun env -> {pfile = rev_hash_s env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax}) stack in
	let pl = List.filter (fun p -> p <> null_pos) pl in
	match pl with
	| [] ->
		let extra = if ctx.record_stack then "" else "\nNo stack information available, consider compiling with -D eval-stack" in
		uncaught_exception_string v p extra
	| _ ->
		let sstack = String.concat "\n" (List.map (fun p -> Printf.sprintf "%s: Called from here" (format_pos p)) pl) in
		Printf.sprintf "%s: Uncaught exception %s\n%s" (format_pos p) (value_string v) sstack

let build_exception_stack ctx environment_offset =
	let eval = get_eval ctx in
	let d = if not ctx.record_stack then [] else DynArray.to_list (DynArray.sub eval.environments environment_offset (eval.environment_offset - environment_offset)) in
	ctx.exception_stack <- List.map (fun env ->
		env.env_in_use <- false;
		env.env_debug.timer();
		{pfile = rev_hash_s env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax},env.env_info.kind
	) d

let catch_exceptions ctx ?(final=(fun() -> ())) f p =
	let prev = !get_ctx_ref in
	select ctx;
	let eval = get_eval ctx in
	let environment_offset = eval.environment_offset in
	let r = try
		let v = f() in
		get_ctx_ref := prev;
		final();
		Some v
	with
	| RunTimeException(v,stack,p') ->
		build_exception_stack ctx environment_offset;
		eval.environment_offset <- environment_offset;
		let msg = get_exc_error_message ctx v (match stack with [] -> [] | _ :: l -> l) (if p' = null_pos then p else p') in
		get_ctx_ref := prev;
		final();
		Error.error msg null_pos
	| MacroApi.Abort ->
		final();
		None
	| exc ->
		get_ctx_ref := prev;
		final();
		raise exc
	in
	r