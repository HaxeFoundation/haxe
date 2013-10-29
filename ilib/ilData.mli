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

type ilversion = int * int (* minor + major *)

type ilclass = {
	cpath : ilpath;
	cflags : type_def_flags;
	csuper : ilsig option;
	cfields : ilfield list;
	cmethods : ilmethod list;
	mutable cimplements : ilsig list;
	mutable ctypes : type_param list;
	mutable cprops : ilprop list;
	(* cevents :  *)
	mutable cenclosing : ilclass option;
	mutable cnested : ilclass list;
}

and type_param = {
	tnumber : int;
	tflags : generic_flags;
	tname : string option;
	mutable tconstraints : ilsig list;
}

and ilfield = {
	fname : string;
	fflags : field_flags;
	fsig : ilsig;
}

and ilmethod = {
	mname : string;
	mflags : method_flags;
	msig : ilsig;
	mutable mparams : ilmethod_param list;
	mutable mret : ilsig;
	mutable mis_override : bool; (* method_impl *)
	mutable mtypes : type_param list;
}

and ilmethod_param = string * param_flags * ilsig

and ilprop = {
	pname : string;
	psig : ilsig;
	pflags : property_flags;
	mutable pget : ilmethod option;
	mutable pset : ilmethod option;
}

type ilctx = {
	il_tables : (clr_meta DynArray.t) array;
	il_relations : (meta_pointer, clr_meta) Hashtbl.t;
}
