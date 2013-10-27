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

let rec follow s = match s with
	| SReqModifier (_,s)
	| SOptModifier (_,s) ->
		follow s
	| SPinned s ->
		follow s
	| s -> s

(* tells if a type_def_or_ref is of type `path` *)
let rec is_type path = function
	| TypeDef td ->
		td.td_namespace = fst path && td.td_name = snd path
	| TypeRef tr ->
		tr.tr_namespace = fst path && tr.tr_name = snd path
	| TypeSpec ts -> (match follow ts.ts_signature with
	| SClass c | SValueType c ->
		is_type path c
	| SGenericInst(s,_) -> (match follow s with
		| SClass c | SValueType c ->
			is_type path c
		| _ -> false)
	| _ -> false)
	| _ -> assert false

let rec get_path type_def_or_ref = match type_def_or_ref with
	| TypeDef td ->
		(td.td_namespace, td.td_name)
	| TypeRef tr ->
		(tr.tr_namespace, tr.tr_name)
	| TypeSpec ts -> (match follow ts.ts_signature with
	| SClass c | SValueType c ->
		get_path c
	| SGenericInst(s,_) -> (match follow s with
		| SClass c | SValueType c ->
			get_path c
		| _ -> "","")
	| _ -> "","")
	| _ -> assert false

let constant_s = function
	| IBool true -> "true"
	| IBool false -> "false"
	| IChar chr -> "'" ^ Char.escaped (Char.chr chr) ^ "'"
	| IByte i ->
		Printf.sprintf "(byte) 0x%x" i
	| IShort i ->
		Printf.sprintf "(short) 0x%x" i
	| IInt i ->
		Printf.sprintf "0x%lx" i
	| IInt64 i ->
		Printf.sprintf "0x%Lx" i
	| IFloat32 f ->
		Printf.sprintf "%ff" f
	| IFloat64 f ->
		Printf.sprintf "%fd" f
	| IString s -> "\"" ^ s ^ "\""
	| INull -> "null"

let rec instance_s = function
	| InstConstant c -> constant_s c
	| InstBoxed b -> "boxed " ^ constant_s b
	| InstType t -> "Type " ^ t
	| InstArray il -> "[" ^ String.concat ", " (List.map instance_s il) ^ "]"
	| InstEnum e -> "Enum " ^ string_of_int e

let named_attribute_s (is_prop,name,inst) =
	(if is_prop then
		"/*prop*/ "
	else
		"")
	^ name ^ " = " ^ instance_s inst

let attributes_s (il,nal) =
	"(" ^ (String.concat ", " (List.map instance_s il)) ^ (if nal <> [] then ", " ^ (String.concat ", " (List.map named_attribute_s nal)) else "") ^")"
