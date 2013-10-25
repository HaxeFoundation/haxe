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

open PeData;;
open PeReader;;
open IlMeta;;
open IO;;
open Printf;;

(* decoding helpers *)
let type_def_vis_of_int i = match i land 0x7 with
	(* visibility flags - mask 0x7 *)
	| 0x0 -> VPrivate (* 0x0 *)
	| 0x1 -> VPublic (* 0x1 *)
	| 0x2 -> VNestedPublic (* 0x2 *)
	| 0x3 -> VNestedPrivate (* 0x3 *)
	| 0x4 -> VNestedFamily (* 0x4 *)
	| 0x5 -> VNestedAssembly (* 0x5 *)
	| 0x6 -> VNestedFamAndAssem (* 0x6 *)
	| 0x7 -> VNestedFamOrAssem (* 0x7 *)
	| _ -> assert false

let type_def_layout_of_int i = match i land 0x18 with
	(* layout flags - mask 0x18 *)
	| 0x0 -> LAuto (* 0x0 *)
	| 0x8 -> LSequential (* 0x8 *)
	| 0x10 -> LExplicit (* 0x10 *)
	| _ -> assert false

let type_def_semantics_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* semantics flags - mask 0x5A0 *)
		| 0x20 -> SInterface (* 0x20 *)
		| 0x80 -> SAbstract (* 0x80 *)
		| 0x100 -> SSealed (* 0x100 *)
		| 0x400 -> SSpecialName (* 0x400 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x20;0x80;0x100;0x400]

let type_def_impl_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* type implementation flags - mask 0x103000 *)
		| 0x1000 -> IImport (* 0x1000 *)
		| 0x2000 -> ISerializable (* 0x2000 *)
		| 0x00100000 -> IBeforeFieldInit (* 0x00100000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1000;0x2000;0x00100000]

let type_def_string_of_int i = match i land 0x00030000 with
	(* string formatting flags - mask 0x00030000 *)
	| 0x0 -> SAnsi (* 0x0 *)
	| 0x00010000 -> SUnicode (* 0x00010000 *)
	| 0x00020000 -> SAutoChar (* 0x00020000 *)
	| _ -> assert false

let type_def_flags_of_int i =
	{
		tdf_vis = type_def_vis_of_int i;
		tdf_layout = type_def_layout_of_int i;
		tdf_semantics = type_def_semantics_of_int i;
		tdf_impl = type_def_impl_of_int i;
		tdf_string = type_def_string_of_int i;
	}

let field_access_of_int i = match i land 0x07 with
	(* access flags - mask 0x07 *)
	| 0x0 -> FAPrivateScope (* 0x0 *)
	| 0x1 -> FAPrivate (* 0x1 *)
	| 0x2 -> FAFamAndAssem (* 0x2 *)
	| 0x3 -> FAAssembly (* 0x3 *)
	| 0x4 -> FAFamily (* 0x4 *)
	| 0x5 -> FAFamOrAssem (* 0x5 *)
	| 0x6 -> FAPublic (* 0x6 *)
	| _ -> assert false

let field_contract_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* contract flags - mask 0x02F0 *)
		| 0x10 -> CStatic (* 0x10 *)
		| 0x20 -> CInitOnly (* 0x20 *)
		| 0x40 -> CLiteral (* 0x40 *)
		| 0x80 -> CNotSerialized (* 0x80 *)
		| 0x200 -> CSpecialName (* 0x200 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x10;0x20;0x40;0x80;0x200]

let field_reserved_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* reserved flags - cannot be set explicitly. mask 0x9500 *)
		| 0x400 -> RSpecialName (* 0x400 *)
		| 0x1000 -> RMarshal (* 0x1000 *)
		| 0x8000 -> RConstant (* 0x8000 *)
		| 0x0100 -> RFieldRVA (* 0x0100 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x400;0x1000;0x8000;0x100]

let field_flags_of_int i =
	{
		ff_access = field_access_of_int i;
		ff_contract = field_contract_of_int i;
		ff_reserved = field_reserved_of_int i;
	}

let method_contract_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* contract flags - mask 0xF0 *)
		| 0x10 -> CStatic (* 0x10 *)
		| 0x20 -> CFinal (* 0x20 *)
		| 0x40 -> CVirtual (* 0x40 *)
		| 0x80 -> CHideBySig (* 0x80 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x10;0x20;0x40;0x80]

let method_vtable_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* vtable flags - mask 0x300 *)
		| 0x100 -> VNewSlot (* 0x100 *)
		| 0x200 -> VStrict (* 0x200 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x100;0x200]

let method_impl_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* implementation flags - mask 0x2C08 *)
		| 0x0400 -> IAbstract (* 0x0400 *)
		| 0x0800 -> ISpecialName (* 0x0800 *)
		| 0x2000 -> IPInvokeImpl (* 0x2000 *)
		| 0x0008 -> IUnmanagedExp (* 0x0008 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x0400;0x0800;0x2000;0x0008]

let method_reserved_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* reserved flags - cannot be set explicitly. mask 0xD000 *)
		| 0x1000 -> RTSpecialName (* 0x1000 *)
		| 0x4000 -> RHasSecurity (* 0x4000 *)
		| 0x8000 -> RReqSecObj (* 0x8000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1000;0x4000;0x8000]

let method_code_type_of_int i = match i land 0x3 with
	| 0x0 -> CCil (* 0x0 *)
	| 0x1 -> CNative (* 0x1 *)
	| 0x2 -> COptIl (* 0x2 *)
	| 0x3 -> CRuntime (* 0x3 *)
	| _ -> assert false

let method_code_mngmt_of_int i = match i land 0x4 with
	| 0x0 -> MManaged (* 0x0 *)
	| 0x4 -> MUnmanaged (* 0x4 *)

let method_interop_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x10 -> OForwardRef (* 0x10 *)
		| 0x80 -> OPreserveSig (* 0x80 *)
		| 0x1000 -> OInternalCall (* 0x1000 *)
		| 0x20 -> OSynchronized (* 0x20 *)
		| 0x08 -> ONoInlining (* 0x08 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x10;0x80;0x1000;0x20;0x08]

let method_flags_of_int iflags flags =
	{
		mf_access = field_access_of_int flags;
		mf_contract = method_contract_of_int flags;
		mf_vtable = method_vtable_of_int flags;
		mf_impl = method_impl_of_int flags;
		mf_reserved = method_reserved_of_int flags;
		mf_code_type = method_code_type_of_int iflags;
		mf_code_mngmt = method_code_mngmt_of_int iflags;
		mf_interop = method_interop_of_int iflags;
	}

let callconv_of_int i =
	let basic = match i land 0x1F with
		| 0x0 -> CallDefault (* 0x0 *)
		| 0x5 -> CallVararg (* 0x5 *)
		| 0x6 -> CallField (* 0x6 *)
		| 0x7 -> CallLocal (* 0x7 *)
		| 0x8 -> CallProp (* 0x8 *)
		| 0x9 -> CallUnmanaged (* 0x9 *)
		| _ -> assert false
	in
	match i land 0x20 with
		| 0x20 ->
			CallHasThis basic
		| _ when i land 0x40 = 0x40 ->
			CallExplicitThis basic
		| _ -> basic

(* TODO: convert from string to Bigstring if OCaml 4 is available *)
type meta_ctx = {
	compressed : bool;
		(* is a compressed stream *)
	strings_stream : string;
	mutable strings_offset : int;
		(* #Strings: a string heap containing the names of metadata items *)
	blob_stream : string;
	mutable blob_offset : int;
		(* #Blob: blob heap containing internal metadata binary object, such as default values, signatures, etc *)
	guid_stream : string;
	mutable guid_offset : int;
		(* #GUID: a GUID heap *)
	us_stream : string;
		(* #US: user-defined strings *)
	meta_stream : string;
		(* may be either: *)
			(* #~: compressed (optimized) metadata stream *)
			(* #-: uncompressed (unoptimized) metadata stream *)
	mutable meta_edit_continue : bool;
	mutable meta_has_deleted : bool;

	tables : (clr_meta DynArray.t) array;
	table_sizes : ( string -> int -> int * int ) array;
	extra_streams : clr_stream_header list;
}

let empty = "<not initialized>"

(* ******* Reading from Strings ********* *)

let sget s pos = Char.code (String.get s pos)

let read_compressed_i32 s pos =
	let v = sget s pos in
	(* Printf.printf "compressed: %x (18 0x%x 19 0x%x)\n" v (sget s (pos+20)) (sget s (pos+21)); *)
	if v land 0x80 = 0x00 then
		pos+1, v
	else if v land 0xC0 = 0x80 then
		pos+2, ((v land 0x3F) lsl 8) lor (sget s (pos+1))
	else if v land 0xE0 = 0xC0 then
		pos+4, ((v land 0x1F) lsl 24) lor ((sget s (pos+1)) lsl 16) lor ((sget s (pos+2)) lsl 8) lor (sget s (pos+3))
	else
		error (Printf.sprintf "Error reading compressed data. Invalid first byte: %x" v)

let int_of_table (idx : clr_meta_idx) : int = Obj.magic idx
let table_of_int (idx : int) : clr_meta_idx = Obj.magic idx

let sread_ui8 s pos =
	let n1 = sget s pos in
	pos+1,n1

let sread_i32 s pos =
	let n1 = sget s pos in
	let n2 = sget s (pos+1) in
	let n3 = sget s (pos+2) in
	let n4 = sget s (pos+3) in
	pos+4, (n4 lsl 24) lor (n3 lsl 16) lor (n2 lsl 8) lor n1

let sread_ui16 s pos =
	let n1 = sget s pos in
	let n2 = sget s (pos+1) in
	pos+2, (n2 lsl 8) lor n1

let read_cstring ctx pos =
	let s = ctx.strings_stream in
	let rec loop en =
		match String.get s en with
		| '\x00' -> en - pos
		| _ -> loop (en+1)
	in
	let len = loop pos in
	String.sub s pos len

let read_sstring_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.strings_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	match i with
	| 0 ->
		metapos, ""
	| _ ->
		metapos, read_cstring ctx i

let read_sguid_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.guid_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	match i with
	| 0 ->
		metapos, ""
	| _ ->
		let s = ctx.guid_stream in
		let i = i - 1 in
		let pos = i * 16 in
		metapos, String.sub s pos 16

(* ******* Metadata Tables ********* *)
let null_meta = UnknownMeta (-1)

let null_module () =
	{
		m_generation = 0;
		m_name = empty;
		m_vid = empty;
		m_encid = empty;
		m_encbase_id = empty;
	}

let null_type_ref () =
	{
		tr_resolution_scope = null_meta;
		tr_name = empty;
		tr_namespace = empty;
	}

let null_type_def () =
	{
		td_flags = type_def_flags_of_int 0;
		td_name = empty;
		td_namespace = empty;
		td_extends = null_meta;
		td_field_list = -1;
		td_method_list = -1;
	}

let null_field_ptr () =
	{
		fp_field = -1;
	}

let null_field () =
	{
		f_flags = field_flags_of_int 0;
		f_name = empty;
		f_signature = SVoid;
	}

let null_method_ptr () =
	{
		mp_method = -1;
	}

let mk_null = function
	| IModule -> Module (null_module())
	| ITypeRef -> TypeRef (null_type_ref())
	| ITypeDef -> TypeDef (null_type_def())
	| IFieldPtr -> FieldPtr (null_field_ptr())
	| IField -> Field (null_field())
	| IMethodPtr -> MethodPtr (null_method_ptr())
	| i ->
		UnknownMeta (int_of_table i)

let get_table ctx idx rid =
	let cur = ctx.tables.(int_of_table idx) in
	DynArray.get cur rid

(* special coded types  *)
let max_clr_meta_idx = 76

let coded_description = Array.init (max_clr_meta_idx - 63) (fun i ->
	let i = 64 + i in
	match table_of_int i with
		| ITypeDefOrRef ->
			Array.of_list [ITypeDef;ITypeRef;ITypeSpec], 2
		| IHasConstant ->
			Array.of_list [IField;IParam;IProperty], 2
		| IHasCustomAttribute ->
			Array.of_list
			[IMethod;IField;ITypeRef;ITypeDef;IParam;IInterfaceImpl;IMemberRef;
			 IModule;IDeclSecurity;IProperty;IEvent;IStandAloneSig;IModuleRef;
			 ITypeSpec;IAssembly;IAssemblyRef;IFile;IExportedType;IManifestResource;
			 IGenericParam;IGenericParamConstraint;IMethodSpec], 5
		| IHasFieldMarshal ->
			Array.of_list [IField;IParam], 1
		| IHasDeclSecurity ->
			Array.of_list [ITypeDef;IMethod;IAssembly], 2
		| IMemberRefParent ->
			Array.of_list [ITypeDef;ITypeRef;IModuleRef;IMethod;ITypeSpec], 3
		| IHasSemantics ->
			Array.of_list [IEvent;IProperty], 1
		| IMethodDefOrRef ->
			Array.of_list [IMethod;IMemberRef], 1
		| IMemberForwarded ->
			Array.of_list [IField;IMethod], 1
		| IImplementation ->
			Array.of_list [IFile;IAssemblyRef;IExportedType], 2
		| ICustomAttributeType ->
			Array.of_list [ITypeRef;ITypeDef;IMethod;IMemberRef(*;IString FIXME *)], 3
		| IResolutionScope ->
			Array.of_list [IModule;IModuleRef;IAssemblyRef;ITypeRef], 2
		| ITypeOrMethodDef ->
			Array.of_list [ITypeDef;IMethod], 1
		| _ ->
			print_endline ("Unknown coded index: " ^ string_of_int i);
			assert false)

let set_coded_sizes ctx rows =
	let check i tbls max =
		if List.exists (fun t ->
			let _, nrows = rows.(int_of_table t) in
			nrows >= max
		) tbls then
			ctx.table_sizes.(i) <- sread_i32
	in
	for i = 64 to (max_clr_meta_idx) do
		let tbls, size = coded_description.(i - 64) in
		let max = 1 lsl (16 - size) in
		check i (Array.to_list tbls) max
	done

let sread_from_table ctx in_blob tbl s pos =
	let i = int_of_table tbl in
	let sread = if in_blob then
		read_compressed_i32
	else
		ctx.table_sizes.(i)
	in
	let pos, rid = sread s pos in
	if i >= 64 then begin
		let tbls,size = coded_description.(i-64) in
		let mask = (1 lsl size) - 1 in
		let mask = if mask = 0 then 1 else mask in
		let tidx = rid land mask in
		let real_rid = rid lsr size in
		let real_tbl = tbls.(tidx) in
		printf "rid 0x%x - table idx 0x%x - real_rid 0x%x\n\n" rid tidx real_rid;
		pos, get_table ctx real_tbl real_rid
	end else
		pos, get_table ctx tbl rid

(* ******* SIGNATURE READING ********* *)

let rec read_ilsig ctx s pos =
	let i = sget s pos in
	let pos = pos + 1 in
	match i with
		| 0x1 -> pos, SVoid (* 0x1 *)
		| 0x2 -> pos, SBool (* 0x2 *)
		| 0x3 -> pos, SChar (* 0x3 *)
		| 0x4 -> pos, SInt8 (* 0x4 *)
		| 0x5 -> pos, SUInt8 (* 0x5 *)
		| 0x6 -> pos, SInt16 (* 0x6 *)
		| 0x7 -> pos, SUInt16 (* 0x7 *)
		| 0x8 -> pos, SInt32 (* 0x8 *)
		| 0x9 -> pos, SUInt32 (* 0x9 *)
		| 0xA -> pos, SInt64 (* 0xA *)
		| 0xB -> pos, SUInt64 (* 0xB *)
		| 0xC -> pos, SFloat32 (* 0xC *)
		| 0xD -> pos, SFloat64 (* 0xD *)
		| 0xE -> pos, SString (* 0xE *)
		| 0xF ->
			let pos, s = read_ilsig ctx s pos in
			pos, SPointer s
		| 0x10 ->
			let pos, s = read_ilsig ctx s pos in
			pos, SManagedPointer s
		| 0x11 ->
			let pos, vt = sread_from_table ctx true ITypeDefOrRef s pos in
			pos, SValueType vt
		| 0x12 ->
			let pos, c = sread_from_table ctx true ITypeDefOrRef s pos in
			pos, SClass c
		| 0x13 ->
			let n = sget s pos in
			pos + 1, STypeParam n
		| 0x14 ->
			let pos, ssig = read_ilsig ctx s pos in
			let pos, rank = read_compressed_i32 s pos in
			let pos, numsizes = read_compressed_i32 s pos in
			let pos = ref pos in
			let sizearray = Array.init numsizes (fun _ ->
				let p, size = read_compressed_i32 s !pos in
				pos := p;
				size
			) in
			let pos, bounds = read_compressed_i32 s !pos in
			let pos = ref pos in
			let boundsarray = Array.init bounds (fun _ ->
				let p, b = read_compressed_i32 s !pos in
				pos := p;
				let signed = b land 0x1 = 0x1 in
				let b = b lsr 1 in
				if signed then -b else b
			) in
			let ret = Array.init rank (fun i ->
				(if i >= numsizes then None else Some sizearray.(i))
				, (if i >= bounds then None else Some boundsarray.(i))
			) in
			!pos, SArray(ssig, ret)
		| 0x15 ->
			(* let pos, c = sread_from_table ctx ITypeDefOrRef s pos in *)
			let pos, ssig = read_ilsig ctx s pos in
			let pos, ntypes = read_compressed_i32 s pos in
			let rec loop acc pos n =
				if n >= ntypes then
					pos, List.rev acc
				else
					let pos, ssig = read_ilsig ctx s pos in
					loop (ssig :: acc) pos (n+1)
			in
			let pos, args = loop [] pos 1 in
			pos, SGenericInst (ssig, args)
		| 0x16 -> pos, STypedReference (* 0x16 *)
		| 0x18 -> pos, SIntPtr (* 0x18 *)
		| 0x19 -> pos, SUIntPtr (* 0x19 *)
		| 0x1B ->
			let pos, conv = read_compressed_i32 s pos in
			let callconv = callconv_of_int conv in
			let pos, ntypes = read_compressed_i32 s pos in
			let pos, ret = read_ilsig ctx s pos in
			let rec loop acc pos n =
				if n >= ntypes then
					pos, List.rev acc
				else
					let pos, ssig = read_ilsig ctx s pos in
					loop (ssig :: acc) pos (n+1)
			in
			let pos, args = loop [] pos 1 in
			pos, SFunPtr (callconv, ret, args)
		| 0x1C -> pos, SObject (* 0x1C *)
		| 0x1D ->
			let pos, ssig = read_ilsig ctx s pos in
			pos, SVector ssig
		| 0x1F ->
			let pos, tdef = sread_from_table ctx true ITypeDefOrRef s pos in
			let pos, ilsig = read_ilsig ctx s pos in
			pos, SReqModifier (tdef, ilsig)
		| 0x20 ->
			let pos, tdef = sread_from_table ctx true ITypeDefOrRef s pos in
			let pos, ilsig = read_ilsig ctx s pos in
			pos, SOptModifier (tdef, ilsig)
		| 0x41 -> pos, SSentinel (* 0x41 *)
		| 0x45 ->
			let pos, ssig = read_ilsig ctx s pos in
			pos,SPinned ssig (* 0x45 *)
		| _ ->
			Printf.printf "unknown ilsig 0x%x\n\n" i;
			assert false

let read_ilsig_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset  = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	let s = ctx.blob_stream in
	let i, _ = read_compressed_i32 s i in
	let _, ilsig = read_ilsig ctx s i in
	metapos, ilsig

let read_field_ilsig_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset  = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	let s = ctx.blob_stream in
	let i, _ = read_compressed_i32 s i in
	if sget s i <> 0x6 then error ("Invalid field signature: " ^ string_of_int (sget s i));
	let _, ilsig = read_ilsig ctx s (i+1) in
	metapos, ilsig

let rec ilsig_s = function (* TODO: delete me - leave only in ilMetaDebug *)
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

let read_table_at ctx tbl n pos = match get_table ctx tbl (n+1 (* indices start at 1 *)) with
	| Module m ->
		let s = ctx.meta_stream in
		let pos, gen = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, vid = read_sguid_idx ctx pos in
		let pos, encid = read_sguid_idx ctx pos in
		let pos, encbase_id = read_sguid_idx ctx pos in
		m.m_generation <- gen;
		m.m_name <- name;
		m.m_vid <- vid;
		m.m_encid <- encid;
		m.m_encbase_id <- encbase_id;
		pos, Module m
	| TypeRef tr ->
		let s = ctx.meta_stream in
		let pos, scope = sread_from_table ctx false IResolutionScope s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, ns = read_sstring_idx ctx pos in
		tr.tr_resolution_scope <- scope;
		tr.tr_name <- name;
		tr.tr_namespace <- ns;
		print_endline name;
		print_endline ns;
		pos, TypeRef tr
	| TypeDef td ->
		let s = ctx.meta_stream in
		let pos, flags = sread_i32 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, ns = read_sstring_idx ctx pos in
		let pos, extends = sread_from_table ctx false ITypeDefOrRef s pos in
		let pos, flist = ctx.table_sizes.(int_of_table IField) s pos in
		let pos, fmeth = ctx.table_sizes.(int_of_table IMethod) s pos in
		td.td_flags <- type_def_flags_of_int flags;
		td.td_name <- name;
		td.td_namespace <- ns;
		td.td_extends <- extends;
		td.td_field_list <- flist;
		td.td_method_list <- fmeth;
		print_endline "Type Def!";
		print_endline name;
		print_endline ns;
		pos, TypeDef td
	| FieldPtr fp ->
		let s = ctx.meta_stream in
		let pos, field = ctx.table_sizes.(int_of_table IField) s pos in
		fp.fp_field <- field;
		pos, FieldPtr fp
	| Field f ->
		let s = ctx.meta_stream in
		let pos, flags = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		print_endline ("FIELD NAME " ^ name);
		let pos, ilsig = read_field_ilsig_idx ctx pos in
		print_endline (ilsig_s ilsig);
		f.f_flags <- field_flags_of_int flags;
		f.f_name <- name;
		f.f_signature <- ilsig;
		pos, Field f
	| MethodPtr mp ->
		let s = ctx.meta_stream in
		let pos, m = ctx.table_sizes.(int_of_table IMethod) s pos in
		mp.mp_method <- m;
		pos, MethodPtr mp
	| _ -> assert false

(* ******* META READING ********* *)

let preset_sizes ctx rows =
	Array.iteri (fun n r -> match r with
		| false,_ -> ()
		| true,nrows ->
			ctx.tables.(n) <- DynArray.init (nrows+1) (fun _ -> mk_null (table_of_int n))
	) rows

(* let read_ *)
let read_meta ctx =
	(* read header *)
	let s = ctx.meta_stream in
	let pos = 4 + 1 + 1 in
	let flags = sget s pos in
	List.iter (fun i -> if flags land i = i then match i with
		| 0x01 ->
			ctx.strings_offset <- 4
		| 0x02 ->
			ctx.guid_offset <- 4
		| 0x04 ->
			ctx.blob_offset <- 4
		| 0x20 ->
			assert (not ctx.compressed);
			ctx.meta_edit_continue <- true
		| 0x80 ->
			assert (not ctx.compressed);
			ctx.meta_has_deleted <- true
		| _ -> assert false
	) [0x01;0x02;0x04;0x20;0x80];
	let rid = sget s (pos+1) in
	ignore rid;
	let pos = pos + 2 in
	let mask = Array.init 8 ( fun n -> sget s (pos + n) ) in
	(* loop over masks and check which table is set *)
	let set_table = Array.init 64 (fun n ->
		let idx = n / 8 in
		let bit = n mod 8 in
		(mask.(idx) lsr bit) land 0x1 = 0x1
	) in
	let pos = ref (pos + 8 + 8) in (* there is an extra 'sorted' field, which we do not use *)
	let rows = Array.mapi (fun i b -> match b with
		| false -> false,0
		| true ->
			let nidx, nrows = sread_i32 s !pos in
			if nrows > 0xFFFF then ctx.table_sizes.(i) <- sread_i32;
			pos := nidx;
			true,nrows
	) set_table in
	set_coded_sizes ctx rows;
	(* pre-set all sizes *)
	preset_sizes ctx rows;
	Array.iteri (fun n r -> match r with
		| false,_ -> ()
		| true,nrows ->
			print_endline (string_of_int n);
			let fn = read_table_at ctx (table_of_int n) in
			let rec loop_fn n =
				if n = nrows then
					()
				else begin
					let p, _ = fn n !pos in
					pos := p;
					loop_fn (n+1)
				end
			in
			loop_fn 0
	) rows;
	()

let read_padded i npad =
	let buf = Buffer.create 10 in
	let rec loop n =
		let chr = read i in
		if chr = '\x00' then begin
			let npad = n land 0x3 in
			if npad <> 0 then ignore (nread i (4 - npad));
			Buffer.contents buf
		end else begin
			Buffer.add_char buf chr;
			if n = npad then
				Buffer.contents buf
			else
				loop (n+1)
		end
	in
	loop 1

let read_meta_tables pctx header =
	let i = pctx.r.i in
	seek_rva pctx (fst header.clr_meta);
	let magic = nread i 4 in
	if magic <> "BSJB" then error ("Error reading metadata table: Expected magic 'BSJB'. Got " ^ magic);
	let major = read_ui16 i in
	let minor = read_ui16 i in
	ignore major; ignore minor; (* no use for them *)
	ignore (read_i32 i); (* reserved *)
	let vlen = read_i32 i in
	let ver = nread i vlen in
	ignore ver;

	(* meta storage header *)
	ignore (read_ui16 i); (* reserved *)
	let nstreams = read_ui16 i in
	let rec streams n acc =
		let offset = read_i32 i in
		let size = read_real_i32 i in
		let name = read_padded i 32 in
		let acc = {
			str_offset = offset;
			str_size = size;
			str_name = name;
		} :: acc in
		if (n+1) = nstreams then
			acc
		else
			streams (n+1) acc
	in
	let streams = streams 0 [] in

	(* streams *)
	let compressed = ref None in
	let sstrings = ref "" in
	let sblob = ref "" in
	let sguid = ref "" in
	let sus = ref "" in
	let smeta = ref "" in
	let extra = ref [] in
	List.iter (fun s ->
		let rva = Int32.add (fst header.clr_meta) (Int32.of_int s.str_offset) in
		seek_rva pctx rva;
		match String.lowercase s.str_name with
		| "#guid" ->
			sguid := nread i (Int32.to_int s.str_size)
		| "#strings" ->
			sstrings := nread i (Int32.to_int s.str_size)
		| "#us" ->
			sus := nread i (Int32.to_int s.str_size)
		| "#blob" ->
			sblob := nread i (Int32.to_int s.str_size)
		| "#~" ->
			assert (Option.is_none !compressed);
			compressed := Some true;
			smeta := nread i (Int32.to_int s.str_size)
		| "#-" ->
			assert (Option.is_none !compressed);
			compressed := Some false;
			smeta := nread i (Int32.to_int s.str_size)
		| _ ->
			extra := s :: !extra
	) streams;
	let compressed = match !compressed with
		| None -> error "No compressed or uncompressed metadata streams was found!"
		| Some c -> c
	in
	let tables = Array.init 64 (fun _ -> DynArray.create ()) in
	let ctx = {
		compressed = compressed;
		strings_stream = !sstrings;
		strings_offset = 2;
		blob_stream = !sblob;
		blob_offset = 2;
		guid_stream = !sguid;
		guid_offset = 2;
		us_stream = !sus;
		meta_stream = !smeta;
		meta_edit_continue = false;
		meta_has_deleted = false;
		extra_streams = !extra;
		tables = tables;
		table_sizes = Array.make (max_clr_meta_idx+1) sread_ui16;
	} in
	read_meta ctx;
	()

