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

type ilpath = string list * string list * string

type ilsig = IlMeta.ilsig

and ilsig_norm =
	| LVoid | LBool | LChar
	| LInt8 | LUInt8 | LInt16
	| LUInt16 | LInt32 | LUInt32
	| LInt64 | LUInt64 | LFloat32
	| LFloat64 | LString | LObject
	| LPointer of ilsig_norm
	| LTypedReference | LIntPtr | LUIntPtr
	| LManagedPointer of ilsig_norm
	| LValueType of ilpath * ilsig_norm list
	| LClass of ilpath * ilsig_norm list
	| LTypeParam of int
	| LMethodTypeParam of int
	| LVector of ilsig_norm
	| LArray of ilsig_norm * (int option * int option) array
	| LMethod of callconv list * ilsig_norm * (ilsig_norm list)
	| LSentinel

and ilsig_t = {
	snorm : ilsig_norm;
	ssig : ilsig;
}

type ilversion = int * int (* minor + major *)

type ilclass = {
	cpath : ilpath;
	cflags : type_def_flags;
	csuper : ilsig_t option;
	cfields : ilfield list;
	cmethods : ilmethod list;
	cimplements : ilsig_t list;
	ctypes : type_param list;
	cprops : ilprop list;
	cevents : ilevent list;
	(* cevents :  *)
	cenclosing : ilpath option;
	cnested : ilpath list;
}

and type_param = {
	tnumber : int;
	tflags : generic_flags;
	tname : string option;
	tconstraints : ilsig_t list;
}

and ilevent = {
	ename : string;
	eflags : event_flags;
	eadd : (string * method_flags) option;
	eremove : (string * method_flags) option;
	eraise : (string * method_flags) option;
	esig : ilsig_t;
}

and ilfield = {
	fname : string;
	fflags : field_flags;
	fsig : ilsig_t;
}

and ilmethod = {
	mname : string;
	mflags : method_flags;
	msig : ilsig_t;
	margs : ilmethod_arg list;
	mret : ilsig_t;
	moverride : (ilpath * string) option; (* method_impl *)
		(* refers to the signature of the declaring class *)
	mtypes : type_param list;
}

and ilmethod_arg = string * param_flags * ilsig_t

and ilprop = {
	pname : string;
	psig : ilsig_t;
	pflags : property_flags;
	pget : (string * method_flags) option;
	pset : (string * method_flags) option;
}

type ilctx = {
	il_tables : (clr_meta DynArray.t) array;
	il_relations : (meta_pointer, clr_meta) Hashtbl.t;
	il_typedefs : (ilpath, meta_type_def) Hashtbl.t;
}
