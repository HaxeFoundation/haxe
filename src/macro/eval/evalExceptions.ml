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

let error_exc ctx v stack p =
	let pl = List.map (fun env -> {pfile = rev_hash_s env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax}) stack in
	let pl = List.filter (fun p -> p <> null_pos) pl in
	match pl with
	| [] ->
		let extra = if ctx.record_stack then "" else "\nNo stack information available, consider compiling with -D interp-stack" in
		Error.error (uncaught_exception_string v p extra) null_pos
	| _ ->
		let sstack = String.concat "\n" (List.map (fun p -> Printf.sprintf "\t%s" (format_pos p)) pl) in
		Error.error (Printf.sprintf "%s: Uncaught exception %s\nCalled from:\n%s" (format_pos p) (value_string v) sstack) null_pos

let build_exception_stack ctx environment_offset =
	let d = if not ctx.debug.debug then [] else DynArray.to_list (DynArray.sub (ctx.eval()).environments environment_offset ((ctx.eval()).environment_offset - environment_offset)) in
	ctx.exception_stack <- List.map (fun env ->
		env.env_in_use <- false;
		{pfile = rev_hash_s env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax},env.env_info.kind
	) d

let catch_exceptions ctx f p =
	let prev = !get_ctx_ref in
	select ctx;
	let environment_offset = (ctx.eval()).environment_offset in
	let r = try
		let v = f() in
		get_ctx_ref := prev;
		Some v
	with
	| RunTimeException(v,stack,p) ->
		build_exception_stack ctx environment_offset;
		(ctx.eval()).environment_offset <- environment_offset;
		get_ctx_ref := prev;
		error_exc ctx v (match stack with [] -> [] | _ :: l -> l) p
	| MacroApi.Abort ->
		None
	| exc ->
		get_ctx_ref := prev;
		raise exc
	in
	r