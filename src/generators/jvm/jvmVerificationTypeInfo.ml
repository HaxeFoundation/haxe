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

open JvmGlobals
open JvmSignature

type t =
	| VTop
	| VInteger
	| VFloat
	| VDouble
	| VLong
	| VNull
	| VUninitializedThis
	| VObject of jvm_constant_pool_index
	| VUninitialized of int

let of_signature pool jsig = match jsig with
    | TByte | TChar | TBool | TShort | TInt -> VInteger
    | TFloat -> VFloat
    | TLong -> VLong
    | TDouble -> VDouble
    | TObject(path,_) -> VObject (pool#add_path path)
	| TMethod _ -> VObject (pool#add_path NativeSignatures.haxe_function_path)
	| TArray _ -> VObject (pool#add_path ([],generate_signature false jsig))
	| TTypeParameter _ -> VObject (pool#add_path (["java";"lang"],"Object"))
	| TUninitialized (Some i) -> VUninitialized i
	| TUninitialized None -> VUninitializedThis
    | _ -> Globals.die "" __LOC__

let to_string vtt = match vtt with
	| VTop -> "top"
	| VInteger -> "integer"
	| VFloat -> "float"
	| VLong -> "long"
	| VDouble -> "double"
	| VNull -> "null"
	| VUninitializedThis -> "uninitializedThis"
	| VObject i -> "object " ^ (string_of_int i)
	| VUninitialized i -> "uninitializedVariable " ^ (string_of_int i)