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
open EvalContext
open EvalHash
open EvalString

(* Functions *)

let vifun0 f = vfunction (fun vl -> match vl with
	| [] -> f vnull
	| [v0] -> f v0
	| _ -> invalid_call_arg_number 1 (List.length  vl
))

let vifun1 f = vfunction (fun vl -> match vl with
	| [] -> f vnull vnull
	| [v0] -> f v0 vnull
	| [v0;v1] -> f v0 v1
	| _ -> invalid_call_arg_number 2 (List.length  vl
))

let vifun2 f = vfunction (fun vl -> match vl with
	| [] -> f vnull vnull vnull
	| [v0] -> f v0 vnull vnull
	| [v0;v1] -> f v0 v1 vnull
	| [v0;v1;v2] -> f v0 v1 v2
	| _ -> invalid_call_arg_number 3 (List.length  vl
))

let vifun3 f = vfunction (fun vl -> match vl with
	| [] -> f vnull vnull vnull vnull
	| [v0] -> f v0 vnull vnull vnull
	| [v0;v1] -> f v0 v1 vnull vnull
	| [v0;v1;v2] -> f v0 v1 v2 vnull
	| [v0;v1;v2;v3] -> f v0 v1 v2 v3
	| _ -> invalid_call_arg_number 4 (List.length  vl
))

let vifun4 f = vfunction (fun vl -> match vl with
	| [] -> f vnull vnull vnull vnull vnull
	| [v0] -> f v0 vnull vnull vnull vnull
	| [v0;v1] -> f v0 v1 vnull vnull vnull
	| [v0;v1;v2] -> f v0 v1 v2 vnull vnull
	| [v0;v1;v2;v3] -> f v0 v1 v2 v3 vnull
	| [v0;v1;v2;v3;v4] -> f v0 v1 v2 v3 v4
	| _ -> invalid_call_arg_number 4 (List.length  vl
))

let vfun0 f = vstatic_function (fun vl -> match vl with
	| [] -> f ()
	| _ -> invalid_call_arg_number 1 (List.length  vl
))

let vfun1 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull
	| [v0] -> f v0
	| _ -> invalid_call_arg_number 1 (List.length  vl
))

let vfun2 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull vnull
	| [v0] -> f v0 vnull
	| [v0;v1] -> f v0 v1
	| _ -> invalid_call_arg_number 2 (List.length  vl
))

let vfun3 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull vnull vnull
	| [v0] -> f v0 vnull vnull
	| [v0;v1] -> f v0 v1 vnull
	| [v0;v1;v2] -> f v0 v1 v2
	| _ -> invalid_call_arg_number 3 (List.length  vl
))

let vfun4 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull vnull vnull vnull
	| [v0] -> f v0 vnull vnull vnull
	| [v0;v1] -> f v0 v1 vnull vnull
	| [v0;v1;v2] -> f v0 v1 v2 vnull
	| [v0;v1;v2;v3] -> f v0 v1 v2 v3
	| _ -> invalid_call_arg_number 4 (List.length  vl
))

let vfun5 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull vnull vnull vnull vnull
	| [v0] -> f v0 vnull vnull vnull vnull
	| [v0;v1] -> f v0 v1 vnull vnull vnull
	| [v0;v1;v2] -> f v0 v1 v2 vnull vnull
	| [v0;v1;v2;v3] -> f v0 v1 v2 v3 vnull
	| [v0;v1;v2;v3;v4] -> f v0 v1 v2 v3 v4
	| _ -> invalid_call_arg_number 5 (List.length  vl
))

let vfun6 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull vnull vnull vnull vnull vnull
	| [v0] -> f v0 vnull vnull vnull vnull vnull
	| [v0;v1] -> f v0 v1 vnull vnull vnull vnull
	| [v0;v1;v2] -> f v0 v1 v2 vnull vnull vnull
	| [v0;v1;v2;v3] -> f v0 v1 v2 v3 vnull vnull
	| [v0;v1;v2;v3;v4] -> f v0 v1 v2 v3 v4 vnull
	| [v0;v1;v2;v3;v4;v5] -> f v0 v1 v2 v3 v4 v5
	| _ -> invalid_call_arg_number 6 (List.length  vl
))

let vfun7 f = vstatic_function (fun vl -> match vl with
	| [] -> f vnull vnull vnull vnull vnull vnull vnull
	| [v0] -> f v0 vnull vnull vnull vnull vnull vnull
	| [v0;v1] -> f v0 v1 vnull vnull vnull vnull vnull
	| [v0;v1;v2] -> f v0 v1 v2 vnull vnull vnull vnull
	| [v0;v1;v2;v3] -> f v0 v1 v2 v3 vnull vnull vnull
	| [v0;v1;v2;v3;v4] -> f v0 v1 v2 v3 v4 vnull vnull
	| [v0;v1;v2;v3;v4;v5] -> f v0 v1 v2 v3 v4 v5 vnull
	| [v0;v1;v2;v3;v4;v5;v6] -> f v0 v1 v2 v3 v4 v5 v6
	| _ -> invalid_call_arg_number 7 (List.length  vl
))

(* Objects *)

(* The prototype of the objects whose fields are given in a certain order (the
   macro API encodes many objects with the same fields), with the slot of each
   field: a direct-mapped cache per domain in front of [get_object_prototype],
   which sorts the fields and takes a lock. *)
type encoded_object = {
	eo_ctx : int;
	eo_names : int array;
	eo_slots : int array;
	eo_proto : vprototype;
}

let encoded_objects_size = 1024

let encoded_objects : encoded_object option array Domain.DLS.key =
	Domain.DLS.new_key (fun () -> Array.make encoded_objects_size None)

let rec names_key h l = match l with
	| [] -> h land (encoded_objects_size - 1)
	| (name,_) :: l -> names_key (h * 31 + name) l

let rec same_names names k l = match l with
	| [] -> k = Array.length names
	| (name,_) :: l -> k < Array.length names && Array.unsafe_get names k = name && same_names names (k + 1) l

let rec set_fields a slots k l = match l with
	| [] -> ()
	| (_,v) :: l ->
		a.(Array.unsafe_get slots k) <- v;
		set_fields a slots (k + 1) l

let encode_obj l =
	let ctx = get_ctx() in
	let cache = Domain.DLS.get encoded_objects in
	let key = names_key 0 l in
	match Array.unsafe_get cache key with
	| Some e when e.eo_ctx = ctx.ctx_id && same_names e.eo_names 0 l ->
		let a = make_null_array (Array.length e.eo_slots) in
		set_fields a e.eo_slots 0 l;
		vobject {
			ofields = a;
			oproto = OProto e.eo_proto;
			omark = 0;
		}
	| _ ->
		let proto,sorted = ctx.get_object_prototype ctx l in
		let fields = Array.of_list (List.map snd sorted) in
		(* only fields given once, at the slot their position in [sorted] gives *)
		let names = Array.of_list (List.map fst l) in
		let slots = Array.map (fun name -> try IntMap.find name proto.pinstance_names with Not_found -> -1) names in
		let sorted_names = Array.of_list (List.map fst sorted) in
		let consistent =
			Array.length sorted_names = IntMap.cardinal proto.pinstance_names &&
			Array.for_all (fun i -> i >= 0 && i < Array.length sorted_names) slots &&
			(let ok = ref true in Array.iteri (fun k name -> if sorted_names.(slots.(k)) <> name then ok := false) names; !ok)
		in
		if consistent then cache.(key) <- Some {eo_ctx = ctx.ctx_id; eo_names = names; eo_slots = slots; eo_proto = proto};
		vobject {
			ofields = fields;
			oproto = OProto proto;
			omark = 0;
		}

let encode_obj_s l =
	encode_obj (List.map (fun (s,v) -> (hash_literal s),v) l)

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
		| IEFieldKind -> key_haxe_macro_EFieldKind
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
		| IDisplayKind -> key_haxe_macro_DisplayKind
		| IDisplayMode -> key_haxe_macro_DisplayMode
		| ICapturePolicy -> key_haxe_macro_CapturePolicy
		| IVarScope -> key_haxe_macro_VarScope
		| IVarScopingFlags -> key_haxe_macro_VarScopingFlags
		| IPlatform -> key_haxe_macro_Platform
		| IPackageRule -> key_haxe_macro_PackageRule
		| IMessage -> key_haxe_macro_Message
		| IFunctionKind -> key_haxe_macro_FunctionKind
		| IStringLiteralKind -> key_haxe_macro_StringLiteralKind
	in
	encode_enum_value key index (Array.of_list pl) pos

(* Instances *)

let create_instance_direct proto kind =
	vinstance {
		ifields = if Array.length proto.pinstance_fields = 0 then proto.pinstance_fields else Array.copy proto.pinstance_fields;
		iproto = proto;
		ikind = kind;
		imark = 0;
	}

let create_instance ?(kind=INormal) path =
	let proto = get_instance_prototype (get_ctx()) path null_pos in
	{
		ifields = if Array.length proto.pinstance_fields = 0 then proto.pinstance_fields else Array.copy proto.pinstance_fields;
		iproto = proto;
		ikind = kind;
		imark = 0;
	}

let encode_instance ?(kind=INormal) path =
	vinstance (create_instance ~kind path)

let encode_array_instance a =
	VArray a

let encode_vector_instance v =
	VVector v

let encode_array l =
	encode_array_instance (EvalArray.create (Array.of_list l))

let encode_array_a a =
	encode_array_instance (EvalArray.create a)

let encode_string s =
	create_unknown s

(* Should only be used for std types that aren't expected to change while the compilation server is running *)
let create_cached_instance path fkind =
	let proto = AtomicLazy.from_fun (fun () -> get_instance_prototype (get_ctx()) path null_pos) in
	(fun v ->
		create_instance_direct (AtomicLazy.force proto) (fkind v)
	)

let encode_bytes =
	create_cached_instance key_haxe_io_Bytes (fun s -> IBytes s)

let encode_int_map_direct =
	create_cached_instance key_haxe_ds_IntMap (fun s -> IIntMap s)

let encode_int64_map_direct =
	create_cached_instance key_haxe_ds_Int64Map (fun s -> IInt64Map s)

let encode_string_map_direct =
	create_cached_instance key_haxe_ds_StringMap (fun s -> IStringMap s)

let encode_object_map_direct =
	create_cached_instance key_haxe_ds_ObjectMap (fun (s : value ValueHashtbl.t) -> IObjectMap (Obj.magic s))

let encode_string_map convert m =
	let h = RuntimeStringHashtbl.create () in
	PMap.iter (fun key value -> RuntimeStringHashtbl.add h (create_ascii key) (convert value)) m;
	encode_string_map_direct h

let fake_proto_mutex = Mutex.create ()

let fake_proto cache path =
	Mutex.protect fake_proto_mutex @@ fun () ->
	try Hashtbl.find cache path
	with Not_found ->
		let proto = {
			ppath = path;
			pfields = [||];
			pnames = IntMap.empty;
			pinstance_names = IntMap.empty;
			pinstance_fields = [||];
			pparent = None;
			pkind = PInstance;
			pvalue = vnull;
			pinit = [];
			preset = [||];
			presettable = [||];
			pshared = [||];
			pdomain = [||];
			pdomain_epoch = [||];
			pfrozen = [||];
		} in
		proto.pvalue <- vprototype proto;
		Hashtbl.replace cache path proto;
		proto

let encode_unsafe o =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_Unsafe;
		ikind = IRef (Obj.repr o);
		imark = 0;
	}

let encode_pos p =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_Position;
		ikind = IPos p;
		imark = 0;
	}

let encode_lazytype t f =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_LazyType;
		ikind = ILazyType(t,f);
		imark = 0;
	}

let encode_tdecl t =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_TypeDecl;
		ikind = ITypeDecl t;
		imark = 0;
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
		pinit = [];
		preset = [||];
		presettable = [||];
		pshared = [||];
		pdomain = [||];
		pdomain_epoch = [||];
		pfrozen = [||];
	} in
	proto.pvalue <- vprototype proto;
	proto

let encode_ref v convert tostr =
	vinstance {
		ifields = [|vifun0 (fun _ -> convert v);vifun0 (fun _ -> encode_string (tostr()))|];
		iproto = ref_proto;
		ikind = IRef (Obj.repr v);
		imark = 0;
	}

let encode_lazy r =
	VLazy r

let encode_option encode_value o =
	match o with
	| Some v -> encode_enum_value key_haxe_ds_Option 0 [|encode_value v|] None
	| None -> encode_enum_value key_haxe_ds_Option 1 [||] None

let encode_nullable encode_value o =
	match o with
	| Some v -> encode_value v
	| None -> VNull
