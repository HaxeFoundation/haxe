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

let rec ilsig_s = function
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
	| SValueType td -> "valuetype"
	| SClass cl -> "classtype"
	| STypeParam t -> "!" ^ string_of_int t
	| SArray (s,opts) ->
		ilsig_s s ^ String.concat "" (List.map (function
			| None,None ->
				"[]"
			| Some i,None ->
				"[" ^ string_of_int i ^"...]"
			| None, Some i ->
				"[..." ^ string_of_int i ^"]"
			| Some s, Some b ->
				"[" ^ string_of_int s ^ "..." ^ string_of_int b ^"]"
		) (Array.to_list opts))
	| SGenericInst (t,tl) ->
		"generic " ^ "<" ^ String.concat ", " (List.map ilsig_s tl) ^ ">"
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
