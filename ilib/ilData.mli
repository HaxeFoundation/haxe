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

type ilpath = (string list) * string

type ilsig = IlMeta.ilsig

and ilsig_norm =
	| TVoid | TBool | TChar
	| TInt8 | TUInt8 | TInt16
	| TUInt16 | TInt32 | TUInt32
	| TInt64 | TUInt64 | TFloat32
	| TFloat64 | TString | TObject
	| TPointer of ilsig_norm
	| TTypedReference | TIntPtr | TUIntPtr
	| TManagedPointer of ilsig_norm
	| TValueType of ilpath * ilsig_norm list
	| TClass of ilpath * ilsig_norm list
	| TTypeParam of int
	| TMethodTypeParam of int
	| TVector of ilsig_norm
	| TArray of ilsig_norm * (int option * int option) array
	| TMethod of callconv list * ilsig_norm * (ilsig_norm list)
	| TSentinel

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
	mis_override : bool; (* method_impl *)
	mtypes : type_param list;
}

and ilmethod_arg = string * param_flags * ilsig_t

and ilprop = {
	pname : string;
	psig : ilsig_t;
	pflags : property_flags;
	pget : string option;
	pset : string option;
}

type ilctx = {
	il_tables : (clr_meta DynArray.t) array;
	il_relations : (meta_pointer, clr_meta) Hashtbl.t;
	il_typedefs : (string list * string, meta_type_def) Hashtbl.t;
}
