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

let encode_obj l =
	let ctx = get_ctx() in
	let proto,sorted = ctx.get_object_prototype ctx l in
	vobject {
		ofields = Array.of_list (List.map snd sorted);
		oproto = OProto proto;
	}

(*
	The objects encoded by the macro API have a few shapes, given by the names of
	their fields in a fixed order: the prototype of a shape and the place of each
	field in it are kept per domain, so that encoding an object only allocates it.
*)
type object_shape = {
	shape_ctx : context;
	shape_names : string list;
	shape_proto : vprototype;
	(* [shape_slots.(i)]: the index in [ofields] of the [i]-th field given *)
	shape_slots : int array;
}

let object_shapes : (int,object_shape) Hashtbl.t Domain.DLS.key = Domain.DLS.new_key (fun () -> Hashtbl.create 64)

let rec same_names l1 l2 = match l1,l2 with
	| [],[] -> true
	| s1 :: l1,s2 :: l2 -> (s1 == s2 || String.equal s1 s2) && same_names l1 l2
	| _ -> false

let encode_obj_s l =
	let ctx = get_ctx() in
	let names = List.map fst l in
	let shapes = Domain.DLS.get object_shapes in
	let key = Hashtbl.hash names in
	let shape = match List.find_opt (fun s -> s.shape_ctx == ctx && same_names s.shape_names names) (Hashtbl.find_all shapes key) with
		| Some shape ->
			shape
		| None ->
			let hashed = List.map (fun (s,v) -> (hash s),v) l in
			let proto,sorted = ctx.get_object_prototype ctx hashed in
			let sorted = Array.of_list (List.map fst sorted) in
			let slot h =
				let rec loop i = if sorted.(i) = h then i else loop (i + 1) in
				loop 0
			in
			let shape = {
				shape_ctx = ctx;
				shape_names = names;
				shape_proto = proto;
				shape_slots = Array.of_list (List.map (fun (h,_) -> slot h) hashed);
			} in
			(* the contexts of past compilations are not kept alive forever *)
			if Hashtbl.length shapes > 4096 then Hashtbl.reset shapes;
			Hashtbl.add shapes key shape;
			shape
	in
	let a = Array.make (Array.length shape.shape_slots) vnull in
	List.iteri (fun i (_,v) -> a.(shape.shape_slots.(i)) <- v) l;
	vobject {
		ofields = a;
		oproto = OProto shape.shape_proto;
	}

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
		} in
		proto.pvalue <- vprototype proto;
		Hashtbl.replace cache path proto;
		proto

let encode_unsafe o =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_Unsafe;
		ikind = IRef (Obj.repr o);
	}

let encode_pos p =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_Position;
		ikind = IPos p;
	}

let encode_lazytype t f =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_LazyType;
		ikind = ILazyType(t,f);
	}

let encode_tdecl t =
	vinstance {
		ifields = [||];
		iproto = fake_proto (get_ctx()).fake_proto_cache key_haxe_macro_TypeDecl;
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
		pinit = [];
		preset = [||];
		presettable = [||];
		pshared = [||];
		pdomain = [||];
		pdomain_epoch = [||];
	} in
	proto.pvalue <- vprototype proto;
	proto

let encode_ref v convert tostr =
	vinstance {
		ifields = [|vifun0 (fun _ -> convert v);vifun0 (fun _ -> encode_string (tostr()))|];
		iproto = ref_proto;
		ikind = IRef (Obj.repr v);
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
