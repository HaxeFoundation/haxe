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
open EvalValue
open EvalExceptions
open EvalContext
open EvalHash

(* Functions *)

let vifun0 f = vfunction (Fun1 (fun a -> f a))
let vifun1 f = vfunction (Fun2 (fun a b -> f a b))
let vifun2 f = vfunction (Fun3 (fun a b c -> f a b c))
let vifun3 f = vfunction (Fun4 (fun a b c d -> f a b c d))
let vifun4 f = vfunction (Fun5 (fun a b c d e -> f a b c d e))

let vfun0 f = vstatic_function (Fun0 (fun vl -> f ()))
let vfun1 f = vstatic_function (Fun1 (fun a -> f a))
let vfun2 f = vstatic_function (Fun2 (fun a b -> f a b))
let vfun3 f = vstatic_function (Fun3 (fun a b c -> f a b c))
let vfun4 f = vstatic_function (Fun4 (fun a b c d -> f a b c d))
let vfun5 f = vstatic_function (Fun5 (fun a b c d e -> f a b c d e))

(* Objects *)

let encode_obj _ l =
	let ctx = get_ctx() in
	let proto,sorted = ctx.get_object_prototype ctx l in
	vobject {
		ofields = Array.of_list (List.map snd sorted);
		oproto = proto;
		oextra = IntMap.empty;
		oremoved = IntMap.empty;
	}

let encode_obj_s k l =
	encode_obj k (List.map (fun (s,v) -> (hash_s s),v) l)

(* Enum values *)

let encode_enum_value path i vl pos =
	venum_value {
		eindex = i;
		eargs = vl;
		epath = path;
		enpos = pos;
	}

let encode_enum i pos index pl =
	let open MacroApi in
	let key = match i with
		| IExpr -> key_haxe_macro_ExprDef
		| IBinop -> key_haxe_macro_Binop
		| IUnop -> key_haxe_macro_Unop
		| IConst -> key_haxe_macro_Constant
		| ITParam -> key_haxe_macro_TypeParam
		| ICType -> key_haxe_macro_ComplexType
		| IField -> key_haxe_macro_FieldType
		| IType -> key_haxe_macro_Type
		| IFieldKind -> key_haxe_macro_FieldKind
		| IMethodKind -> key_haxe_macro_MethodKind
		| IVarAccess -> key_haxe_macro_VarAccess
		| IAccess -> key_haxe_macro_Access
		| IClassKind -> key_haxe_macro_ClassKind
		| ITypedExpr -> key_haxe_macro_TypedExprDef
		| ITConstant -> key_haxe_macro_TConstant
		| IModuleType -> key_haxe_macro_ModuleType
		| IFieldAccess -> key_haxe_macro_FieldAccess
		| IAnonStatus -> key_haxe_macro_AnonStatus
		| IImportMode -> key_haxe_macro_ImportMode
		| IQuoteStatus -> key_haxe_macro_QuoteStatus
		| IStringKind -> key_haxe_macro_StringKind
	in
	encode_enum_value key index (Array.of_list pl) pos

(* Instances *)

let create_instance_direct proto =
	vinstance {
		ifields = if Array.length proto.pinstance_fields = 0 then proto.pinstance_fields else Array.copy proto.pinstance_fields;
		iproto = proto;
		ikind = INormal;
	}

let create_instance ?(kind=INormal) path =
	let proto = get_instance_prototype (get_ctx()) path null_pos in
	{
		ifields = if Array.length proto.pinstance_fields = 0 then proto.pinstance_fields else Array.copy proto.pinstance_fields;
		iproto = proto;
		ikind = kind;
	}

let encode_instance ?(kind=INormal) path =
	vinstance (create_instance ~kind path)

let encode_array_instance a =
	VArray a

let encode_vector_instance v =
	VVector v

let encode_array l =
	encode_array_instance (EvalArray.create (Array.of_list l))

let encode_string s =
	VString(Rope.of_string s,lazy s)

let encode_rope s =
	vstring s

let encode_bytes s =
	encode_instance key_haxe_io_Bytes ~kind:(IBytes s)

let encode_int_map_direct h =
	encode_instance key_haxe_ds_IntMap ~kind:(IIntMap h)

let encode_string_map_direct h =
	encode_instance key_haxe_ds_StringMap ~kind:(IStringMap h)

let encode_object_map_direct h =
	encode_instance key_haxe_ds_ObjectMap ~kind:(IObjectMap (Obj.magic h))

let encode_string_map convert m =
	let h = StringHashtbl.create 0 in
	PMap.iter (fun key value -> StringHashtbl.add h (Rope.of_string key,lazy key) (convert value)) m;
	encode_string_map_direct h

let fake_proto path =
	let proto = {
		ppath = path;
		pfields = [||];
		pnames = IntMap.empty;
		pinstance_names = IntMap.empty;
		pinstance_fields = [||];
		pparent = None;
		pkind = PInstance;
		pvalue = vnull;
	} in
	proto.pvalue <- vprototype proto;
	proto

let encode_unsafe o =
	vinstance {
		ifields = [||];
		iproto = fake_proto key_haxe_macro_Unsafe;
		ikind = IRef (Obj.repr o);
	}

let encode_pos p =
	vinstance {
		ifields = [||];
		iproto = fake_proto key_haxe_macro_Position;
		ikind = IPos p;
	}

let encode_lazytype t f =
	vinstance {
		ifields = [||];
		iproto = fake_proto key_haxe_macro_LazyType;
		ikind = ILazyType(t,f);
	}

let encode_tdecl t =
	vinstance {
		ifields = [||];
		iproto = fake_proto key_haxe_macro_TypeDecl;
		ikind = ITypeDecl t;
	}

let ref_proto =
	let proto = {
		ppath = key_haxe_macro_Ref;
		pfields = [||];
		pnames = IntMap.empty;
		pinstance_names = IntMap.add key_get 0 (IntMap.singleton key_toString 1);
		pinstance_fields = [|vnull;vnull|];
		pparent = None;
		pkind = PInstance;
		pvalue = vnull;
	} in
	proto.pvalue <- vprototype proto;
	proto

let encode_ref v convert tostr =
	vinstance {
		ifields = [|vifun0 (fun _ -> convert v);vifun0 (fun _ -> encode_string (tostr()))|];
		iproto = ref_proto;
		ikind = IRef (Obj.repr v);
	}
