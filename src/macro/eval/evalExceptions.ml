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
open EvalField

exception Break
exception Continue
exception Return of value

let is v path =
	path = key_Dynamic || match v with
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
	(Printf.sprintf "%s : Uncaught exception %s%s" (format_pos p) (value_string v) extra)

let get_exc_error_message ctx v stack p =
	let pl = List.map (fun env -> {pfile = rev_hash_s env.env_info.pfile;pmin = env.env_leave_pmin; pmax = env.env_leave_pmax}) stack in
	let pl = List.filter (fun p -> p <> null_pos) pl in
	match pl with
	| [] ->
		let extra = if ctx.record_stack then "" else "\nNo stack information available, consider compiling with -D eval-stack" in
		uncaught_exception_string v p extra
	| _ ->
		let sstack = String.concat "\n" (List.map (fun p -> Printf.sprintf "%s : Called from here" (format_pos p)) pl) in
		Printf.sprintf "%s : Uncaught exception %s\n%s" (format_pos p) (value_string v) sstack

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
		if is v key_haxe_macro_Error then begin
			let v1 = field v key_message in
			let v2 = field v key_pos in
			get_ctx_ref := prev;
			final();
			match v1,v2 with
				| VString(_,s),VInstance {ikind = IPos p} ->
					raise (Error.Error (Error.Custom (Lazy.force s),p))
				| _ ->
					Error.error "Something went wrong" null_pos
		end else begin
			(* Careful: We have to get the message before resetting the context because toString() might access it. *)
			let msg = get_exc_error_message ctx v (match stack with [] -> [] | _ :: l -> l) (if p' = null_pos then p else p') in
			get_ctx_ref := prev;
			final();
			Error.error msg null_pos
		end
	| MacroApi.Abort ->
		final();
		None
	| exc ->
		get_ctx_ref := prev;
		final();
		raise exc
	in
	r
