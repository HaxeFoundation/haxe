(*
 *  This file is part of ilLib
 *  Copyright (c)2004-2013 Haxe Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
open IlMeta;;
open IlMetaTools;;

let path_s = function
	| [],[], s -> s
	| ns,[], s -> String.concat "." ns ^ "." ^ s
	| [],enc, s -> String.concat "@" enc ^ "." ^ s
	| ns,enc,s -> String.concat "." ns ^ "." ^ String.concat "@" enc ^ "." ^ s

let rec ilsig_s = function
	| SBoxed -> "boxed"
	| SEnum e -> "enum " ^ e
	| SType -> "System.Type"
	| SVoid -> "void"
	| SBool -> "bool"
	| SChar -> "char"
	| SInt8 -> "int8"
	| SUInt8 -> "uint8"
	| SInt16 -> "int16"
	| SUInt16 -> "uint16"
	| SInt32 -> "int32"
	| SUInt32 -> "uint32"
	| SInt64 -> "int64"
	| SUInt64 -> "uint64"
	| SFloat32 -> "float"
	| SFloat64 -> "double"
	| SString -> "string"
	| SPointer s -> ilsig_s s ^ "*"
	| SManagedPointer s -> ilsig_s s ^ "&"
	| SValueType td -> "valuetype " ^ path_s (get_path td)
	| SClass cl -> "classtype " ^ path_s (get_path cl)
	| STypeParam t | SMethodTypeParam t -> "!" ^ string_of_int t
	| SArray (s,opts) ->
		ilsig_s s ^ "[" ^ String.concat "," (List.map (function
			| Some i,None when i <> 0 ->
				string_of_int i ^ "..."
			| None, Some i when i <> 0 ->
				string_of_int i
			| Some s, Some b when b = 0 && s <> 0 ->
				string_of_int s ^ "..."
			| Some s, Some b when s <> 0 || b <> 0 ->
				let b = if b > 0 then b - 1 else b in
				string_of_int s ^ "..." ^ string_of_int (s + b)
			| _ ->
				""
		) (Array.to_list opts)) ^ "]"
	| SGenericInst (t,tl) ->
		"generic " ^ (ilsig_s t) ^ "<" ^ String.concat ", " (List.map ilsig_s tl) ^ ">"
	| STypedReference -> "typedreference"
	| SIntPtr -> "native int"
	| SUIntPtr -> "native unsigned int"
	| SFunPtr (callconv,ret,args) ->
		"function " ^ ilsig_s ret ^ "(" ^ String.concat ", " (List.map ilsig_s args) ^ ")"
	| SObject -> "object"
	| SVector s -> ilsig_s s ^ "[]"
	| SReqModifier (_,s) -> "modreq() " ^ ilsig_s s
	| SOptModifier (_,s) -> "modopt() " ^ ilsig_s s
	| SSentinel -> "..."
	| SPinned s -> "pinned " ^ ilsig_s s
