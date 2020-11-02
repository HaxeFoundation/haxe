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
open EvalExceptions

let decode_object v = match vresolve v with
	| VObject o -> o
	| _ -> unexpected_value v "object"

let decode_enum v = match v with
	| VEnumValue ev -> ev.eindex,Array.to_list ev.eargs
	| _ -> unexpected_value v "enum"

let decode_enum_with_pos v = match v with
	| VEnumValue ev -> (ev.eindex,Array.to_list ev.eargs),(match ev.enpos with None -> null_pos | Some p -> p)
	| _ -> unexpected_value v "enum"

let decode_instance v = match v with
	| VInstance vi -> vi
	| _ -> unexpected_value v "instance"

let decode_array v = match v with
	| VArray va -> EvalArray.to_list va
	| _ -> unexpected_value v "array"

let decode_vector v = match v with
	| VVector vv -> vv
	| _ -> unexpected_value v "vector"

let decode_varray v = match v with
	| VArray va -> va
	| _ -> unexpected_value v "array"

let decode_string v = match v with
	| VString s -> s.sstring
	| _ -> unexpected_value v "string"

let decode_vstring v = match v with
	| VString s -> s
	| _ -> unexpected_value v "string"

let decode_native_string v = match v with
	| VNativeString s -> s
	| _ -> unexpected_value v "native string"

let decode_bytes v = match v with
	| VInstance {ikind=IBytes s} -> s
	| _ -> unexpected_value v "string"

let decode_i32 v = match v with
	| VInt32 i -> i
	| VFloat f -> (Int32.of_float f)
	| _ -> unexpected_value v "int"

let decode_int v = match v with
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| _ -> unexpected_value v "int"

let decode_float v = match v with
	| VFloat f -> f
	| _ -> unexpected_value v "float"

let decode_bool v = match v with
	| VTrue -> true
	| VFalse -> false
	| _ -> unexpected_value v "bool"

let default_int v vd = match v with
	| VNull -> vd
	| VInt32 i -> Int32.to_int i
	| VFloat f -> int_of_float f
	| _ -> unexpected_value v "int"

let decode_unsafe v = match v with
	| VInstance {ikind = IRef o} -> o
	| _ -> unexpected_value v "unsafe"

let decode_lazytype v = match v with
	| VInstance {ikind=ILazyType(t,_)} -> t
	| _ -> unexpected_value v "lazy type"

let decode_tdecl v = match v with
	| VInstance {ikind=ITypeDecl t} -> t
	| _ -> unexpected_value v "type declaration"

let decode_pos v = match v with
	| VInstance {ikind=IPos p} -> p
	| _ -> raise MacroApi.Invalid_expr (* maybe_decode_pos relies on this being raised *)

let rec decode_ref v : 'a = match v with
	| VInstance {ikind=IRef r} -> Obj.obj r
	| _ -> unexpected_value v "unsafe"

let num = function
	| VInt32 i -> Int32.to_float i
	| VFloat f -> f
	| v -> unexpected_value v "number"