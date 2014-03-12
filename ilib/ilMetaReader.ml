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
open IlMetaTools;;
open ExtString;;
open IlData;;

(* *)
let get_field = function
	| Field f -> f
	| _ -> assert false

let get_method = function
	| Method m -> m
	| _ -> assert false

let get_param = function
	| Param p -> p
	| _ -> assert false

let get_type_def = function
	| TypeDef p -> p
	| _ -> assert false

let get_event = function
	| Event e -> e
	| _ -> assert false

let get_property = function
	| Property p -> p
	| _ -> assert false

let get_module_ref = function
	| ModuleRef r -> r
	| _ -> assert false

let get_assembly_ref = function
	| AssemblyRef r -> r
	| _ -> assert false

let get_generic_param = function
	| GenericParam p -> p
	| _ -> assert false

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

let null_type_def_flags = type_def_flags_of_int 0

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

let null_field_flags = field_flags_of_int 0

let method_contract_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* contract flags - mask 0xF0 *)
		| 0x10 -> CMStatic (* 0x10 *)
		| 0x20 -> CMFinal (* 0x20 *)
		| 0x40 -> CMVirtual (* 0x40 *)
		| 0x80 -> CMHideBySig (* 0x80 *)
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
	| _ -> assert false

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

let null_method_flags = method_flags_of_int 0 0

let param_io_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* input/output flags - mask 0x13 *)
		| 0x1 -> PIn (* 0x1 *)
		| 0x2 -> POut (* 0x2 *)
		| 0x10 -> POpt (* 0x10 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1;0x2;0x10]

let param_reserved_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* reserved flags - mask 0xF000 *)
		| 0x1000 -> PHasConstant (* 0x1000 *)
		| 0x2000 -> PMarshal (* 0x2000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1000;0x2000]

let param_flags_of_int i =
	{
		pf_io = param_io_of_int i;
		pf_reserved = param_reserved_of_int i;
	}

let null_param_flags = param_flags_of_int 0

let callconv_of_int i =
	let basic = match i land 0xF with
		| 0x0 -> CallDefault (* 0x0 *)
		| 0x5 -> CallVararg (* 0x5 *)
		| 0x6 -> CallField (* 0x6 *)
		| 0x7 -> CallLocal (* 0x7 *)
		| 0x8 -> CallProp (* 0x8 *)
		| 0x9 -> CallUnmanaged (* 0x9 *)
		| i -> printf "error 0x%x\n\n" i; assert false
	in
	match i land 0x20 with
		| 0x20 ->
			[CallHasThis;basic]
		| _ when i land 0x40 = 0x40 ->
			[CallExplicitThis;basic]
		| _ -> [basic]

let event_flags_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x0200 -> ESpecialName (* 0x0200 *)
		| 0x0400 -> ERTSpecialName (* 0x0400 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x0200;0x0400]

let property_flags_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x0200 -> PSpecialName (* 0x0200 *)
		| 0x0400 -> PRTSpecialName (* 0x0400 *)
		| 0x1000 -> PHasDefault (* 0x1000 *)
		| 0xE9FF -> PUnused (* 0xE9FF *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x0200;0x0400;0x1000;0xE9FF]

let semantic_flags_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x0001 -> SSetter (* 0x0001 *)
		| 0x0002 -> SGetter (* 0x0002 *)
		| 0x0004 -> SOther (* 0x0004 *)
		| 0x0008 -> SAddOn (* 0x0008 *)
		| 0x0010 -> SRemoveOn (* 0x0010 *)
		| 0x0020 -> SFire (* 0x0020 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x0001;0x0002;0x0004;0x0008;0x0010;0x0020]

let impl_charset_of_int = function
	| 0x0 -> IDefault (* 0x0 *)
	| 0x2 -> IAnsi (* 0x2 *)
	| 0x4 -> IUnicode (* 0x4 *)
	| 0x6 -> IAutoChar (* 0x6 *)
	| _ -> assert false

let impl_callconv_of_int = function
	| 0x0 -> IDefaultCall (* 0x0 *)
	| 0x100 -> IWinApi (* 0x100 *)
	| 0x200 -> ICDecl (* 0x200 *)
	| 0x300 -> IStdCall (* 0x300 *)
	| 0x400 -> IThisCall (* 0x400 *)
	| 0x500 -> IFastCall (* 0x500 *)
	| _ -> assert false

let impl_flag_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x1 -> INoMangle (* 0x1 *)
		| 0x10 -> IBestFit (* 0x10 *)
		| 0x20 -> IBestFitOff (* 0x20 *)
		| 0x40 -> ILastErr (* 0x40 *)
		| 0x1000 -> ICharMapError (* 0x1000 *)
		| 0x2000 -> ICharMapErrorOff (* 0x2000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1;0x10;0x20;0x40;0x1000;0x2000]

let impl_flags_of_int i =
	{
		if_charset = impl_charset_of_int (i land 0x6);
		if_callconv = impl_callconv_of_int (i land 0x700);
		if_flags = impl_flag_of_int i;
	}

let null_impl_flags = impl_flags_of_int 0

let assembly_flags_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		| 0x1 -> APublicKey (* 0x1 *)
		| 0x100 -> ARetargetable (* 0x100 *)
		| 0x4000 -> ADisableJitCompileOptimizer (* 0x4000 *)
		| 0x8000 -> AEnableJitCompileTracking (* 0x8000 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x1;0x100;0x4000;0x8000]

let hash_algo_of_int = function
	| 0x0 -> HNone (* 0x0 *)
	| 0x8003 -> HReserved (* 0x8003 *)
	| 0x8004 -> HSha1 (* 0x8004 *)
	| _ -> assert false

let file_flag_of_int = function
	| 0x0 -> ContainsMetadata (* 0x0 *)
	| 0x1 -> ContainsNoMetadata (* 0x1 *)
	| _ -> assert false

let manifest_resource_flag_of_int i = match i land 0x7 with
	| 0x0 -> RNone (* 0x0 *)
	| 0x1 -> RPublic (* 0x1 *)
	| 0x2 -> RPrivate (* 0x2 *)
	| _ -> assert false

let generic_variance_of_int = function
	(* mask 0x3 *)
	| 0x0 -> VNone (* 0x0 *)
	| 0x1 -> VCovariant (* 0x1 *)
	| 0x2 -> VContravariant (* 0x2 *)
	| _ -> assert false

let generic_constraint_of_int iprops = List.fold_left (fun acc i ->
	if (iprops land i) = i then (match i with
		(* mask 0x1C *)
		| 0x4 -> CInstanceType (* 0x4 *)
		| 0x8 -> CValueType (* 0x8 *)
		| 0x10 -> CDefaultCtor (* 0x10 *)
		| _ -> assert false) :: acc
	else
		acc) [] [0x4;0x8;0x10]

let generic_flags_of_int i =
	{
		gf_variance = generic_variance_of_int (i land 0x3);
		gf_constraint = generic_constraint_of_int (i land 0x1C);
	}

let null_generic_flags = generic_flags_of_int 0

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

  module_cache : meta_cache;
	tables : (clr_meta DynArray.t) array;
	table_sizes : ( string -> int -> int * int ) array;
	extra_streams : clr_stream_header list;
	relations : (meta_pointer, clr_meta) Hashtbl.t;
	typedefs : (ilpath, meta_type_def) Hashtbl.t;

	mutable delays : (unit -> unit) list;
}

and meta_cache = {
	mutable lookups : (string -> meta_ctx option) list;
	mutable mcache : (meta_module * meta_ctx) list;
}

let empty = "<not initialized>"

let create_cache () =
	{
		lookups = [];
		mcache = [];
	}

let add_lookup cache fn =
	cache.lookups <- fn :: cache.lookups

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

let sread_real_i32 s pos =
	let n1 = sget s pos in
	let n2 = sget s (pos+1) in
	let n3 = sget s (pos+2) in
	let n4 = Int32.of_int (sget s (pos+3)) in
	let n = Int32.of_int ((n3 lsl 16) lor (n2 lsl 8) lor n1) in
	let n4 = Int32.shift_left n4 24 in
	pos+4, (Int32.logor n4 n)

let sread_i64 s pos =
	let pos, v1 = sread_real_i32 s (pos+1) in
	let v1 = Int64.of_int32 v1 in
	let pos, v2 = sread_real_i32 s pos in
	let v2 = Int64.of_int32 v2 in
	let v2 = Int64.shift_left v2 32 in
	pos, (Int64.logor v1 v2)

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
	(* printf "len 0x%x - pos 0x%x\n" (String.length s) pos; *)
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

let read_sblob_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos, i = if ctx.blob_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	match i with
	| 0 ->
		metapos,""
	| _ ->
		let bpos, len = read_compressed_i32 ctx.blob_stream i in
		metapos, String.sub ctx.blob_stream bpos len

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

let read_callconv ctx s pos =
	let pos, conv = read_compressed_i32 s pos in
	let basic = match conv land 0xF with
		| 0x0 -> CallDefault (* 0x0 *)
		| 0x5 -> CallVararg (* 0x5 *)
		| 0x6 -> CallField (* 0x6 *)
		| 0x7 -> CallLocal (* 0x7 *)
		| 0x8 -> CallProp (* 0x8 *)
		| 0x9 -> CallUnmanaged (* 0x9 *)
		| 0xa -> CallGenericInst (* 0xA *)
		| i -> printf "error 0x%x\n" i; assert false
	in
	let basic = [basic] in
	let pos, c = match conv land 0x10 with
		| 0x10 ->
			let pos, nparams = read_compressed_i32 s pos in
			pos, CallGeneric nparams :: basic
		| _ ->
			pos, basic
	in
	match conv land 0x20 with
		| 0x20 ->
			pos, CallHasThis :: basic
		| _ when conv land 0x40 = 0x40 ->
			pos, CallExplicitThis :: basic
		| _ -> pos, basic

let read_constant ctx with_type s pos =
	match with_type with
	| CBool ->
		pos+1, IBool (sget s (pos) <> 0)
	| CChar ->
		let pos, v = sread_ui16 s (pos) in
		pos, IChar v
	| CInt8 | CUInt8 ->
		pos+1,IByte (sget s (pos))
	| CInt16 | CUInt16 ->
		let pos, v = sread_ui16 s (pos) in
		pos, IShort v
	| CInt32 | CUInt32 ->
		let pos, v = sread_real_i32 s (pos) in
		pos, IInt v
	| CInt64 | CUInt64 ->
		let pos, v = sread_i64 s (pos) in
		pos, IInt64 v
	| CFloat32 ->
		let pos, v1 = sread_real_i32 s (pos) in
		pos, IFloat32 (Int32.float_of_bits v1)
	| CFloat64 ->
		let pos, v1 = sread_i64 s (pos) in
		pos, IFloat64 (Int64.float_of_bits v1)
	| CString ->
		if sget s pos = 0xff then
			pos+1,IString ""
		else
			let pos, len = read_compressed_i32 s pos in
			pos+len, IString (String.sub s pos len)
	| CNullRef ->
		pos+1, INull

let sig_to_const = function
	| SBool -> CBool
	| SChar -> CChar
	| SInt8 -> CInt8
	| SUInt8 -> CUInt8
	| SInt16 -> CInt16
	| SUInt16 -> CUInt16
	| SInt32 -> CInt32
	| SUInt32 -> CUInt32
	| SInt64 -> CInt64
	| SUInt64 -> CUInt64
	| SFloat32 -> CFloat32
	| SFloat64 -> CFloat64
	| SString -> CString
	| _ -> CNullRef

let read_constant_type ctx s pos = match sget s pos with
	| 0x2 -> pos+1, CBool (* 0x2 *)
	| 0x3 -> pos+1, CChar (* 0x3 *)
	| 0x4 -> pos+1, CInt8 (* 0x4 *)
	| 0x5 -> pos+1, CUInt8 (* 0x5 *)
	| 0x6 -> pos+1, CInt16 (* 0x6 *)
	| 0x7 -> pos+1, CUInt16 (* 0x7 *)
	| 0x8 -> pos+1, CInt32 (* 0x8 *)
	| 0x9 -> pos+1, CUInt32 (* 0x9 *)
	| 0xA -> pos+1, CInt64 (* 0xA *)
	| 0xB -> pos+1, CUInt64 (* 0xB *)
	| 0xC -> pos+1, CFloat32 (* 0xC *)
	| 0xD -> pos+1, CFloat64 (* 0xD *)
	| 0xE -> pos+1, CString (* 0xE *)
	| 0x12 -> pos+1, CNullRef (* 0x12 *)
	| i -> Printf.printf "0x%x\n" i; assert false

let action_security_of_int = function
	| 0x1 -> SecRequest (* 0x1 *)
	| 0x2 -> SecDemand (* 0x2 *)
	| 0x3 -> SecAssert (* 0x3 *)
	| 0x4 -> SecDeny (* 0x4 *)
	| 0x5 -> SecPermitOnly (* 0x5 *)
	| 0x6 -> SecLinkCheck (* 0x6 *)
	| 0x7 -> SecInheritCheck (* 0x7 *)
	| 0x8 -> SecReqMin (* 0x8 *)
	| 0x9 -> SecReqOpt (* 0x9 *)
	| 0xA -> SecReqRefuse (* 0xA *)
	| 0xB -> SecPreJitGrant (* 0xB *)
	| 0xC -> SecPreJitDeny (* 0xC *)
	| 0xD -> SecNonCasDemand (* 0xD *)
	| 0xE -> SecNonCasLinkDemand (* 0xE *)
	| 0xF -> SecNonCasInheritance (* 0xF *)
	| _ -> assert false

(* ******* Metadata Tables ********* *)
let null_meta = UnknownMeta (-1)

let mk_module id =
	{
		md_id = id;
		md_generation = 0;
		md_name = empty;
		md_vid = empty;
		md_encid = empty;
		md_encbase_id = empty;
	}

let null_module = mk_module (-1)

let mk_type_ref id =
	{
		tr_id = id;
		tr_resolution_scope = null_meta;
		tr_name = empty;
		tr_namespace = [];
	}

let null_type_ref = mk_type_ref (-1)

let mk_type_def id =
	{
		td_id = id;
		td_flags = null_type_def_flags;
		td_name = empty;
		td_namespace = [];
		td_extends = None;
		td_field_list = [];
		td_method_list = [];
		td_extra_enclosing = None;
	}

let null_type_def = mk_type_def (-1)

let mk_field id =
	{
		f_id = id;
		f_flags = null_field_flags;
		f_name = empty;
		f_signature = SVoid;
	}

let null_field = mk_field (-1)

let mk_field_ptr id =
	{
		fp_id = id;
		fp_field = null_field;
	}

let null_field_ptr = mk_field_ptr (-1)

let mk_method id =
	{
		m_id = id;
		m_rva = Int32.of_int (-1);
		m_flags = null_method_flags;
		m_name = empty;
		m_signature = SVoid;
		m_param_list = [];
		m_declaring = None;
	}

let null_method = mk_method (-1)

let mk_method_ptr id =
	{
		mp_id = id;
		mp_method = null_method;
	}

let null_method_ptr = mk_method_ptr (-1)

let mk_param id =
	{
		p_id = id;
		p_flags = null_param_flags;
		p_sequence = -1;
		p_name = empty;
	}

let null_param = mk_param (-1)

let mk_param_ptr id =
	{
		pp_id = id;
		pp_param = null_param;
	}

let null_param_ptr = mk_param_ptr (-1)

let mk_interface_impl id =
	{
		ii_id = id;
		ii_class = null_type_def; (* TypeDef rid *)
		ii_interface = null_meta;
	}

let null_interface_impl = mk_interface_impl (-1)

let mk_member_ref id =
	{
		memr_id = id;
		memr_class = null_meta;
		memr_name = empty;
		memr_signature = SVoid;
	}

let null_member_ref = mk_member_ref (-1)

let mk_constant id =
	{
		c_id = id;
		c_type = CNullRef;
		c_parent = null_meta;
		c_value = INull;
	}

let null_constant = mk_constant (-1)

let mk_custom_attribute id =
	{
		ca_id = id;
		ca_parent = null_meta;
		ca_type = null_meta;
		ca_value = None;
	}

let null_custom_attribute = mk_custom_attribute (-1)

let mk_field_marshal id =
	{
		fm_id = id;
		fm_parent = null_meta;
		fm_native_type = NVoid;
	}

let null_field_marshal = mk_field_marshal (-1)

let mk_decl_security id =
	{
		ds_id = id;
		ds_action = SecNull;
		ds_parent = null_meta;
		ds_permission_set = empty;
	}

let mk_class_layout id =
	{
		cl_id = id;
		cl_packing_size = -1;
		cl_class_size = -1;
		cl_parent = null_type_def;
	}

let mk_field_layout id =
	{
		fl_id = id;
		fl_offset = -1;
		fl_field = null_field;
	}

let mk_stand_alone_sig id =
	{
		sa_id = id;
		sa_signature = SVoid;
	}

let mk_event id =
	{
		e_id = id;
		e_flags = [];
		e_name = empty;
		e_event_type = null_meta;
	}

let null_event = mk_event (-1)

let mk_event_map id =
	{
		em_id = id;
		em_parent = null_type_def;
		em_event_list = [];
	}

let mk_event_ptr id =
	{
		ep_id = id;
		ep_event = null_event;
	}

let mk_property id =
	{
		prop_id = id;
		prop_flags = [];
		prop_name = empty;
		prop_type = SVoid;
	}

let null_property = mk_property (-1)

let mk_property_map id =
	{
		pm_id = id;
		pm_parent = null_type_def;
		pm_property_list = [];
	}

let mk_property_ptr id =
	{
		prp_id = id;
		prp_property = null_property;
	}

let mk_method_semantics id =
	{
		ms_id = id;
		ms_semantic = [];
		ms_method = null_method;
		ms_association = null_meta;
	}

let mk_method_impl id =
	{
		mi_id = id;
		mi_class = null_type_def;
		mi_method_body = null_meta;
		mi_method_declaration = null_meta;
	}

let mk_module_ref id =
	{
		modr_id = id;
		modr_name = empty;
	}

let null_module_ref = mk_module_ref (-1)

let mk_type_spec id =
	{
		ts_id = id;
		ts_signature = SVoid;
	}

let mk_enc_log id =
	{
		el_id = id;
		el_token = -1;
		el_func_code = -1;
	}

let mk_impl_map id =
	{
		im_id = id;
		im_flags = null_impl_flags;
		im_forwarded = null_meta;
		im_import_name = empty;
		im_import_scope = null_module_ref;
	}

let mk_enc_map id =
	{
		encm_id = id;
		encm_token = -1;
	}

let mk_field_rva id =
	{
		fr_id = id;
		fr_rva = Int32.zero;
		fr_field = null_field;
	}

let mk_assembly id =
	{
		a_id = id;
		a_hash_algo = HNone;
		a_major = -1;
		a_minor = -1;
		a_build = -1;
		a_rev = -1;
		a_flags = [];
		a_public_key = empty;
		a_name = empty;
		a_locale = empty;
	}

let mk_assembly_processor id =
	{
		ap_id = id;
		ap_processor = -1;
	}

let mk_assembly_os id =
	{
		aos_id = id;
		aos_platform_id = -1;
		aos_major_version = -1;
		aos_minor_version = -1;
	}

let mk_assembly_ref id =
	{
		ar_id = id;
		ar_major = -1;
		ar_minor = -1;
		ar_build = -1;
		ar_rev = -1;
		ar_flags = [];
		ar_public_key = empty;
		ar_name = empty;
		ar_locale = empty;
		ar_hash_value = empty;
	}

let null_assembly_ref = mk_assembly_ref (-1)

let mk_assembly_ref_processor id =
	{
		arp_id = id;
		arp_processor = -1;
		arp_assembly_ref = null_assembly_ref;
	}

let mk_assembly_ref_os id =
	{
		aros_id = id;
		aros_platform_id = -1;
		aros_major = -1;
		aros_minor = -1;
		aros_assembly_ref = null_assembly_ref;
	}

let mk_file id =
	{
		file_id = id;
		file_flags = ContainsMetadata;
		file_name = empty;
		file_hash_value = empty;
	}

let mk_exported_type id =
	{
		et_id = id;
		et_flags = null_type_def_flags;
		et_type_def_id = -1;
		et_type_name = empty;
		et_type_namespace = [];
		et_implementation = null_meta;
	}

let mk_manifest_resource id =
	{
		mr_id = id;
		mr_offset = -1;
		mr_flags = RNone;
		mr_name = empty;
		mr_implementation = None;
	}

let mk_nested_class id =
	{
		nc_id = id;
		nc_nested = null_type_def;
		nc_enclosing = null_type_def;
	}

let mk_generic_param id =
	{
		gp_id = id;
		gp_number = -1;
		gp_flags = null_generic_flags;
		gp_owner = null_meta;
		gp_name = None;
	}

let null_generic_param = mk_generic_param (-1)

let mk_method_spec id =
	{
		mspec_id = id;
		mspec_method = null_meta;
		mspec_instantiation = SVoid;
	}

let mk_generic_param_constraint id =
	{
		gc_id = id;
		gc_owner = null_generic_param;
		gc_constraint = null_meta;
	}

let mk_meta tbl id = match tbl with
	| IModule -> Module (mk_module id)
	| ITypeRef -> TypeRef (mk_type_ref id)
	| ITypeDef -> TypeDef (mk_type_def id)
	| IFieldPtr -> FieldPtr (mk_field_ptr id)
	| IField -> Field (mk_field id)
	| IMethodPtr -> MethodPtr (mk_method_ptr id)
	| IMethod -> Method (mk_method id)
	| IParamPtr -> ParamPtr (mk_param_ptr id)
	| IParam -> Param (mk_param id)
	| IInterfaceImpl -> InterfaceImpl (mk_interface_impl id)
	| IMemberRef -> MemberRef (mk_member_ref id)
	| IConstant -> Constant (mk_constant id)
	| ICustomAttribute -> CustomAttribute (mk_custom_attribute id)
	| IFieldMarshal -> FieldMarshal(mk_field_marshal id)
	| IDeclSecurity -> DeclSecurity(mk_decl_security id)
	| IClassLayout -> ClassLayout(mk_class_layout id)
	| IFieldLayout -> FieldLayout(mk_field_layout id)
	| IStandAloneSig -> StandAloneSig(mk_stand_alone_sig id)
	| IEventMap -> EventMap(mk_event_map id)
	| IEventPtr -> EventPtr(mk_event_ptr id)
	| IEvent -> Event(mk_event id)
	| IPropertyMap -> PropertyMap(mk_property_map id)
	| IPropertyPtr -> PropertyPtr(mk_property_ptr id)
	| IProperty -> Property(mk_property id)
	| IMethodSemantics -> MethodSemantics(mk_method_semantics id)
	| IMethodImpl -> MethodImpl(mk_method_impl id)
	| IModuleRef -> ModuleRef(mk_module_ref id)
	| ITypeSpec -> TypeSpec(mk_type_spec id)
	| IImplMap -> ImplMap(mk_impl_map id)
	| IFieldRVA -> FieldRVA(mk_field_rva id)
	| IENCLog -> ENCLog(mk_enc_log id)
	| IENCMap -> ENCMap(mk_enc_map id)
	| IAssembly -> Assembly(mk_assembly id)
	| IAssemblyProcessor -> AssemblyProcessor(mk_assembly_processor id)
	| IAssemblyOS -> AssemblyOS(mk_assembly_os id)
	| IAssemblyRef -> AssemblyRef(mk_assembly_ref id)
	| IAssemblyRefProcessor -> AssemblyRefProcessor(mk_assembly_ref_processor id)
	| IAssemblyRefOS -> AssemblyRefOS(mk_assembly_ref_os id)
	| IFile -> File(mk_file id)
	| IExportedType -> ExportedType(mk_exported_type id)
	| IManifestResource -> ManifestResource(mk_manifest_resource id)
	| INestedClass -> NestedClass(mk_nested_class id)
	| IGenericParam -> GenericParam(mk_generic_param id)
	| IMethodSpec -> MethodSpec(mk_method_spec id)
	| IGenericParamConstraint -> GenericParamConstraint(mk_generic_param_constraint id)
	| i -> UnknownMeta (int_of_table i)

let get_table ctx idx rid =
	let cur = ctx.tables.(int_of_table idx) in
	DynArray.get cur (rid-1)

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
			Array.of_list [ITypeRef(* unused ? *);ITypeDef (* unused ? *);IMethod;IMemberRef(*;IString FIXME *)], 3
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

let sread_from_table_opt ctx in_blob tbl s pos =
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
		(* printf "rid 0x%x - table idx 0x%x - real_rid 0x%x\n\n" rid tidx real_rid; *)
		if real_rid = 0 then
			pos, None
		else
			pos, Some (get_table ctx real_tbl real_rid)
	end else if rid = 0 then
		pos, None
	else
		pos, Some (get_table ctx tbl rid)

let sread_from_table ctx in_blob tbl s pos =
	let pos, opt = sread_from_table_opt ctx in_blob tbl s pos in
	pos, Option.get opt

(* ******* SIGNATURE READING ********* *)
let read_inline_str s pos =
	let pos, len = read_compressed_i32 s pos in
	let ret = String.sub s pos len in
	pos+len,ret

let rec read_ilsig ctx s pos =
	let i = sget s pos in
	(* printf "0x%x\n" i; *)
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
				(if i >= bounds then None else Some boundsarray.(i))
				, (if i >= numsizes then None else Some sizearray.(i))
			) in
			!pos, SArray(ssig, ret)
		| 0x15 ->
			(* let pos, c = sread_from_table ctx ITypeDefOrRef s pos in *)
			let pos, ssig = read_ilsig ctx s pos in
			let pos, ntypes = read_compressed_i32 s pos in
			let rec loop acc pos n =
				if n > ntypes then
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
		| 0x1E ->
			let pos, conv = read_compressed_i32 s pos in
			pos, SMethodTypeParam conv
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
		(* special undocumented constants *)
		| 0x50 -> pos, SType
		| 0x51 -> pos, SBoxed
		| 0x55 ->
			let pos, vt = read_inline_str s pos in
			pos, SEnum vt
		| _ ->
			Printf.printf "unknown ilsig 0x%x\n\n" i;
			assert false

let rec read_variantsig ctx s pos =
	let pos, b = sread_ui8 s pos in
	match b with
		| 0x00 -> pos, VT_EMPTY (* 0x00 *)
		| 0x01 -> pos, VT_NULL (* 0x01 *)
		| 0x02 -> pos, VT_I2 (* 0x02 *)
		| 0x03 -> pos, VT_I4 (* 0x03 *)
		| 0x04 -> pos, VT_R4 (* 0x04 *)
		| 0x05 -> pos, VT_R8 (* 0x05 *)
		| 0x06 -> pos, VT_CY (* 0x06 *)
		| 0x07 -> pos, VT_DATE (* 0x07 *)
		| 0x08 -> pos, VT_BSTR (* 0x08 *)
		| 0x09 -> pos, VT_DISPATCH (* 0x09 *)
		| 0x0A -> pos, VT_ERROR (* 0x0A *)
		| 0x0B -> pos, VT_BOOL (* 0x0B *)
		| 0x0C -> pos, VT_VARIANT (* 0x0C *)
		| 0x0D -> pos, VT_UNKNOWN (* 0x0D *)
		| 0x0E -> pos, VT_DECIMAL (* 0x0E *)
		| 0x10 -> pos, VT_I1 (* 0x10 *)
		| 0x11 -> pos, VT_UI1 (* 0x11 *)
		| 0x12 -> pos, VT_UI2 (* 0x12 *)
		| 0x13 -> pos, VT_UI4 (* 0x13 *)
		| 0x14 -> pos, VT_I8 (* 0x14 *)
		| 0x15 -> pos, VT_UI8 (* 0x15 *)
		| 0x16 -> pos, VT_INT (* 0x16 *)
		| 0x17 -> pos, VT_UINT (* 0x17 *)
		| 0x18 -> pos, VT_VOID (* 0x18 *)
		| 0x19 -> pos, VT_HRESULT (* 0x19 *)
		| 0x1A -> pos, VT_PTR (* 0x1A *)
		| 0x1B -> pos, VT_SAFEARRAY (* 0x1B *)
		| 0x1C -> pos, VT_CARRAY (* 0x1C *)
		| 0x1D -> pos, VT_USERDEFINED (* 0x1D *)
		| 0x1E -> pos, VT_LPSTR (* 0x1E *)
		| 0x1F -> pos, VT_LPWSTR (* 0x1F *)
		| 0x24 -> pos, VT_RECORD (* 0x24 *)
		| 0x40 -> pos, VT_FILETIME (* 0x40 *)
		| 0x41 -> pos, VT_BLOB (* 0x41 *)
		| 0x42 -> pos, VT_STREAM (* 0x42 *)
		| 0x43 -> pos, VT_STORAGE (* 0x43 *)
		| 0x44 -> pos, VT_STREAMED_OBJECT (* 0x44 *)
		| 0x45 -> pos, VT_STORED_OBJECT (* 0x45 *)
		| 0x46 -> pos, VT_BLOB_OBJECT (* 0x46 *)
		| 0x47 -> pos, VT_CF (* 0x47 *)
		| 0x48 -> pos, VT_CLSID (* 0x48 *)
		| _ -> assert false

let rec read_nativesig ctx s pos : int * nativesig =
	let pos, b = sread_ui8 s pos in
	match b with
		| 0x01 -> pos, NVoid (* 0x01 *)
		| 0x02 -> pos, NBool (* 0x02 *)
		| 0x03 -> pos, NInt8 (* 0x03 *)
		| 0x4 -> pos, NUInt8 (* 0x4 *)
		| 0x5 -> pos, NInt16 (* 0x5 *)
		| 0x6 -> pos, NUInt16 (* 0x6 *)
		| 0x7 -> pos, NInt32 (* 0x7 *)
		| 0x8 -> pos, NUInt32 (* 0x8 *)
		| 0x9 -> pos, NInt64 (* 0x9 *)
		| 0xA -> pos, NUInt64 (* 0xA *)
		| 0xB -> pos, NFloat32 (* 0xB *)
		| 0xC -> pos, NFloat64 (* 0xC *)
		| 0xD -> pos, NSysChar (* 0xD *)
		| 0xE -> pos, NVariant (* 0xE *)
		| 0xF -> pos, NCurrency (* 0xF *)
		| 0x10 -> pos, NPointer (* 0x10 *)
		| 0x11 -> pos, NDecimal (* 0x11 *)
		| 0x12 -> pos, NDate (* 0x12 *)
		| 0x13 -> pos, NBStr (* 0x13 *)
		| 0x14 -> pos, NLPStr (* 0x14 *)
		| 0x15 -> pos, NLPWStr (* 0x15 *)
		| 0x16 -> pos, NLPTStr (* 0x16 *)
		| 0x17 ->
			let pos, size = read_compressed_i32 s pos in
			pos, NFixedString size
		| 0x18 -> pos, NObjectRef (* 0x18 *)
		| 0x19 -> pos, NUnknown (* 0x19 *)
		| 0x1A -> pos, NDispatch (* 0x1A *)
		| 0x1B -> pos, NStruct (* 0x1B *)
		| 0x1C -> pos, NInterface (* 0x1C *)
		| 0x1D ->
			let pos, v = read_variantsig ctx s pos in
			pos, NSafeArray v
		| 0x1E ->
			let pos, size = read_compressed_i32 s pos in
			let pos, t = read_variantsig ctx s pos in
			pos, NFixedArray (size,t)
		| 0x1F -> pos, NIntPointer (* 0x1F *)
		| 0x20 -> pos, NUIntPointer (* 0x20 *)
		| 0x21 -> pos, NNestedStruct (* 0x21 *)
		| 0x22 -> pos, NByValStr (* 0x22 *)
		| 0x23 -> pos, NAnsiBStr (* 0x23 *)
		| 0x24 -> pos, NTBStr (* 0x24 *)
		| 0x25 -> pos, NVariantBool (* 0x25 *)
		| 0x26 -> pos, NFunctionPtr (* 0x26 *)
		| 0x28 -> pos, NAsAny (* 0x28 *)
		| 0x2A ->
			let pos, elt = read_nativesig ctx s pos in
			let pos, paramidx = read_compressed_i32 s pos in
			let pos, size = read_compressed_i32 s pos in
			let pos, param_mult = read_compressed_i32 s pos in
			pos, NArray(elt,paramidx,size,param_mult)
		| 0x2B -> pos, NLPStruct (* 0x2B *)
		| 0x2C ->
			let pos, guid_val = read_inline_str s pos in
			let pos, unmanaged = read_inline_str s pos in
			(* FIXME: read TypeRef *)
			pos, NCustomMarshaler (guid_val,unmanaged)
		| 0x2D -> pos, NError (* 0x2D *)
		| _ -> assert false

let read_blob_idx ctx s pos =
	let metapos,i = if ctx.blob_offset = 2 then
			sread_ui16 s pos
		else
			sread_i32 s pos
	in
	metapos, i


let read_nativesig_idx ctx s pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	let s = ctx.blob_stream in
	let _, ret = read_nativesig ctx s i in
	metapos, ret

let read_method_ilsig_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	let s = ctx.blob_stream in
	let pos, len = read_compressed_i32 s i in
	(* for x = 0 to len do *)
	(* 	printf "%x " (sget s (i+x)) *)
	(* done; *)
	let endpos = pos + len in
	(* printf "\n"; *)
	let pos, callconv = read_callconv ctx s pos in
	let pos, ntypes = read_compressed_i32 s pos in
	let pos, ret = read_ilsig ctx s pos in
	let rec loop acc pos n =
		if n > ntypes || pos >= endpos then
			pos, List.rev acc
		else
			let pos, ssig = read_ilsig ctx s pos in
			loop (ssig :: acc) pos (n+1)
	in
	let pos, args = loop [] pos 1 in
	metapos, SFunPtr (callconv, ret, args)

let read_ilsig_idx ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	let s = ctx.blob_stream in
	let i, _ = read_compressed_i32 s i in
	let _, ilsig = read_ilsig ctx s i in
	metapos, ilsig

let read_field_ilsig_idx ?(force_field=true) ctx pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	let s = ctx.blob_stream in
	let i, _ = read_compressed_i32 s i in
	if sget s i <> 0x6 then
		if force_field then
			error ("Invalid field signature: " ^ string_of_int (sget s i))
		else
			read_method_ilsig_idx ctx pos
	else
		let _, ilsig = read_ilsig ctx s (i+1) in
		metapos, ilsig

let get_underlying_enum_type ctx name =
  (* first try to get a typedef *)
	let ns, name = match List.rev (String.nsplit name ".") with
		| name :: ns -> List.rev ns, name
		| _ -> assert false
	in
	try
		let tdefs = ctx.tables.(int_of_table ITypeDef) in
		let len = DynArray.length tdefs in
		let rec loop_find idx =
			if idx >= len then
				raise Not_found
			else
				let tdef = match DynArray.get tdefs idx with | TypeDef td -> td | _ -> assert false in
				if tdef.td_name = name && tdef.td_namespace = ns then
					tdef
				else
					loop_find (idx+1)
		in
		let tdef = loop_find 1 in
		(* now find the first static field associated with it *)
		try
			let nonstatic = List.find (fun f ->
				not (List.mem CStatic f.f_flags.ff_contract)
			) tdef.td_field_list in
			nonstatic.f_signature
		with | Not_found -> assert false (* should never happen! *)
	with | Not_found ->
		(* FIXME: in order to correctly handle SEnum, we need to look it up *)
		(* from either this assembly or from any other assembly that we reference *)
		(* this is tricky - specially since this reader does not intend to handle file system *)
		(* operations by itself. For now, if an enum is referenced from another module, *)
		(* we won't handle it. The `cache` structure is laid out to deal with these problems *)
		(* but isn't implemented yet *)
		raise Exit

let read_custom_attr ctx attr_type s pos =
	let pos, prolog = sread_ui16 s pos in
	if prolog <> 0x0001 then error (sprintf "Error reading custom attribute: Expected prolog 0x0001 ; got 0x%x" prolog);
	let isig = match attr_type with
		| Method m -> m.m_signature
		| MemberRef mr -> mr.memr_signature
		| _ -> assert false
	in
	let args = match follow isig with
		| SFunPtr (_,ret,args) -> args
		| _ -> assert false
	in
	let rec read_instance ilsig pos =
		(* print_endline (IlMetaDebug.ilsig_s ilsig); *)
		match follow ilsig with
		| SBool | SChar	| SInt8 | SUInt8 | SInt16 | SUInt16
		| SInt32 | SUInt32 | SInt64 | SUInt64 | SFloat32 | SFloat64 | SString ->
			let pos, cons = read_constant ctx (sig_to_const ilsig) s pos in
			pos, InstConstant (cons)
		| SClass c when is_type (["System"],"Type") c ->
			let pos, len = read_compressed_i32 s pos in
			pos+len, InstType (String.sub s pos len)
		| SType ->
			let pos, len = read_compressed_i32 s pos in
			pos+len, InstType (String.sub s pos len)
		| SObject | SBoxed -> (* boxed *)
			let pos = if sget s pos = 0x51 then pos+1 else pos in
			let pos, ilsig = read_ilsig ctx s pos in
			let pos, ret = read_instance ilsig pos in
			pos, InstBoxed( ret )
			(* (match follow ilsig with *)
			(* | SEnum e -> *)
			(* 		let ilsig = get_underlying_enum_type ctx e; *)
			(* 	let pos,e = if is_boxed then sread_i32 s pos else read_compressed_i32 s pos in *)
			(* 	pos, InstBoxed(InstEnum e) *)
			(* | _ -> *)
			(* 	let pos, boxed = read_constant ctx (sig_to_const ilsig) s pos in *)
			(* 	pos, InstBoxed (InstConstant boxed)) *)
		| SEnum e ->
			let ilsig = get_underlying_enum_type ctx e in
			read_instance ilsig pos
		| SValueType _ -> (* enum *)
			let pos, e = sread_i32 s pos in
			pos, InstEnum e
		| _ -> assert false
	in
	let rec read_fixed acc args pos = match args with
		| [] ->
			pos, List.rev acc
		| SVector isig :: args ->
			(* print_endline "vec"; *)
			let pos, nelem = sread_real_i32 s pos in
			let pos, ret = if nelem = -1l then
				pos, InstConstant INull
			else
				let nelem = Int32.to_int nelem in
				let rec loop acc pos n =
					if n = nelem then
						pos, InstArray (List.rev acc)
					else
						let pos, inst = read_instance isig pos in
						loop (inst :: acc) pos (n+1)
				in
				loop [] pos 0
			in
			read_fixed (ret :: acc) args pos
		| isig :: args ->
			let pos, i = read_instance isig pos in
			read_fixed (i :: acc) args pos
	in
	(* let tpos = pos in *)
	let pos, fixed = read_fixed [] args pos in
	(* printf "fixed %d : " (List.length args); *)
	(* for x = tpos to pos do *)
	(* 	printf "%x " (sget s x) *)
	(* done; *)
	(* printf "\n"; *)
	(* for x = 0 to 10 do *)
	(* 	printf "%x " (sget s (pos + x)) *)
	(* done; *)
	(* printf "\n"; *)
	let pos, nnamed = read_compressed_i32 s pos in
	let pos = if nnamed > 0 then pos+1 else pos in
	let rec read_named acc pos n =
		if n = nnamed then
			pos, List.rev acc
		else
			let pos, forp = sread_ui8 s pos in
			let is_prop = if forp = 0x53 then
					false
				else if forp = 0x54 then
					true
				else
					error (sprintf "named custom attribute error: expected 0x53 or 0x54 - got 0x%x" forp)
			in
			let pos, t = read_ilsig ctx s pos in
			let pos, len = read_compressed_i32 s pos in
			let name = String.sub s pos len in
			let pos = pos+len in
			let pos, inst = read_instance t pos in
			read_named ( (is_prop, name, inst) :: acc ) pos (n+1)
	in
	let pos, named = read_named [] pos 0 in
	pos, (fixed, named)

let read_custom_attr_idx ctx ca attr_type pos =
	let s = ctx.meta_stream in
	let metapos,i = if ctx.blob_offset = 2 then
		sread_ui16 s pos
	else
		sread_i32 s pos
	in
	if i = 0 then
		metapos
	else
		let s = ctx.blob_stream in
		let i, _ = read_compressed_i32 s i in
		ctx.delays <- (fun () ->
			try
				let _, attr = read_custom_attr ctx attr_type s i in
				ca.ca_value <- Some attr
			with | Exit ->
				()
		) :: ctx.delays;
		metapos

let read_next_index ctx offset table last pos =
	if last then
		DynArray.length ctx.tables.(int_of_table table) + 1
	else
		let s = ctx.meta_stream in
		let _, idx = ctx.table_sizes.(int_of_table table) s (pos+offset) in
		idx

let get_rev_list ctx table ptr_table begin_idx end_idx =
	(* first check if index exists on pointer table *)
	let ptr_table_t = ctx.tables.(int_of_table ptr_table) in
	(* printf "table %d begin %d end %d\n" (int_of_table table) begin_idx end_idx; *)
	match ctx.compressed, DynArray.length ptr_table_t with
	| true, _ | _, 0 ->
		(* use direct index *)
		let rec loop idx acc =
			if idx >= end_idx then
				acc
			else
				loop (idx+1) (get_table ctx table idx :: acc)
		in
		loop begin_idx []
	| _ ->
		(* use indirect index *)
		let rec loop idx acc =
			if idx > end_idx then
				acc
			else
				loop (idx+1) (get_table ctx ptr_table idx :: acc)
		in
		let ret = loop begin_idx [] in
		List.map (fun meta ->
			let p = meta_root_ptr meta in
			get_table ctx table p.ptr_to.root_id
		) ret

let read_list ctx table ptr_table begin_idx offset last pos =
	let end_idx = read_next_index ctx offset table last pos in
	get_rev_list ctx table ptr_table begin_idx end_idx

let parse_ns id = match String.nsplit id "." with
	| [""] -> []
	| ns -> ns

let get_meta_pointer = function
	| Module r -> IModule, r.md_id
	| TypeRef r -> ITypeRef, r.tr_id
	| TypeDef r -> ITypeDef, r.td_id
	| FieldPtr r -> IFieldPtr, r.fp_id
	| Field r -> IField, r.f_id
	| MethodPtr r -> IMethodPtr, r.mp_id
	| Method r -> IMethod, r.m_id
	| ParamPtr r -> IParamPtr, r.pp_id
	| Param r -> IParam, r.p_id
	| InterfaceImpl r -> IInterfaceImpl, r.ii_id
	| MemberRef r -> IMemberRef, r.memr_id
	| Constant r -> IConstant, r.c_id
	| CustomAttribute r -> ICustomAttribute, r.ca_id
	| FieldMarshal r -> IFieldMarshal, r.fm_id
	| DeclSecurity r -> IDeclSecurity, r.ds_id
	| ClassLayout r -> IClassLayout, r.cl_id
	| FieldLayout r -> IFieldLayout, r.fl_id
	| StandAloneSig r -> IStandAloneSig, r.sa_id
	| EventMap r -> IEventMap, r.em_id
	| EventPtr r -> IEventPtr, r.ep_id
	| Event r -> IEvent, r.e_id
	| PropertyMap r -> IPropertyMap, r.pm_id
	| PropertyPtr r -> IPropertyPtr, r.prp_id
	| Property r -> IProperty, r.prop_id
	| MethodSemantics r -> IMethodSemantics, r.ms_id
	| MethodImpl r -> IMethodImpl, r.mi_id
	| ModuleRef r -> IModuleRef, r.modr_id
	| TypeSpec r -> ITypeSpec, r.ts_id
	| ImplMap r -> IImplMap, r.im_id
	| FieldRVA r -> IFieldRVA, r.fr_id
	| ENCLog r -> IENCLog, r.el_id
	| ENCMap r -> IENCMap, r.encm_id
	| Assembly r -> IAssembly, r.a_id
	| AssemblyProcessor r -> IAssemblyProcessor, r.ap_id
	| AssemblyOS r -> IAssemblyOS, r.aos_id
	| AssemblyRef r -> IAssemblyRef, r.ar_id
	| AssemblyRefProcessor r -> IAssemblyRefProcessor, r.arp_id
	| AssemblyRefOS r -> IAssemblyRefOS, r.aros_id
	| File r -> IFile, r.file_id
	| ExportedType r -> IExportedType, r.et_id
	| ManifestResource r -> IManifestResource, r.mr_id
	| NestedClass r -> INestedClass, r.nc_id
	| GenericParam r -> IGenericParam, r.gp_id
	| MethodSpec r -> IMethodSpec, r.mspec_id
	| GenericParamConstraint r -> IGenericParamConstraint, r.gc_id
	| _ -> assert false

let add_relation ctx key v =
	let ptr = get_meta_pointer key in
	Hashtbl.add ctx.relations ptr v

let read_table_at ctx tbl n last pos =
	(* print_endline ("rr " ^ string_of_int (n+1)); *)
	let s = ctx.meta_stream in
	match get_table ctx tbl (n+1 (* indices start at 1 *)) with
	| Module m ->
		let pos, gen = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, vid = read_sguid_idx ctx pos in
		let pos, encid = read_sguid_idx ctx pos in
		let pos, encbase_id = read_sguid_idx ctx pos in
		m.md_generation <- gen;
		m.md_name <- name;
		m.md_vid <- vid;
		m.md_encid <- encid;
		m.md_encbase_id <- encbase_id;
		pos, Module m
	| TypeRef tr ->
		let pos, scope = sread_from_table ctx false IResolutionScope s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, ns = read_sstring_idx ctx pos in
		tr.tr_resolution_scope <- scope;
		tr.tr_name <- name;
		tr.tr_namespace <- parse_ns ns;
		(* print_endline name; *)
		(* print_endline ns; *)
		pos, TypeRef tr
	| TypeDef td ->
		let startpos = pos in
		let pos, flags = sread_i32 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, ns = read_sstring_idx ctx pos in
		let ns = parse_ns ns in
		let pos, extends = sread_from_table_opt ctx false ITypeDefOrRef s pos in
		let field_offset = pos - startpos in
		let pos, flist_begin = ctx.table_sizes.(int_of_table IField) s pos in
		let method_offset = pos - startpos in
		let pos, mlist_begin = ctx.table_sizes.(int_of_table IMethod) s pos in
		td.td_flags <- type_def_flags_of_int flags;
		td.td_name <- name;
		td.td_namespace <- ns;
		td.td_extends <- extends;
		td.td_field_list <- List.rev_map get_field (read_list ctx IField IFieldPtr flist_begin field_offset last pos);
		td.td_method_list <- List.rev_map get_method (read_list ctx IMethod IMethodPtr mlist_begin method_offset last pos);
		List.iter (fun m -> m.m_declaring <- Some td) td.td_method_list;
		let path = get_path (TypeDef td) in
		Hashtbl.add ctx.typedefs path td;
		(* print_endline "Type Def!"; *)
		(* print_endline name; *)
		(* print_endline ns; *)
		pos, TypeDef td
	| FieldPtr fp ->
		let pos, field = sread_from_table ctx false IField s pos in
		let field = get_field field in
		fp.fp_field <- field;
		pos, FieldPtr fp
	| Field f ->
		let pos, flags = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		(* print_endline ("FIELD NAME " ^ name); *)
		let pos, ilsig = read_field_ilsig_idx ctx pos in
		(* print_endline (ilsig_s ilsig); *)
		f.f_flags <- field_flags_of_int flags;
		f.f_name <- name;
		f.f_signature <- ilsig;
		pos, Field f
	| MethodPtr mp ->
		let pos, m = sread_from_table ctx false IMethod s pos in
		let m = get_method m in
		mp.mp_method <- m;
		pos, MethodPtr mp
	| Method m ->
		let startpos = pos in
		let pos, rva = sread_i32 s pos in
		let pos, iflags = sread_ui16 s pos in
		let pos, flags = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, ilsig = read_method_ilsig_idx ctx pos in
		let offset = pos - startpos in
		let pos, paramlist = ctx.table_sizes.(int_of_table IParam) s pos in
		m.m_rva <- Int32.of_int rva;
		m.m_flags <- method_flags_of_int iflags flags;
		m.m_name <- name;
		m.m_signature <- ilsig;
		m.m_param_list <- List.rev_map get_param (read_list ctx IParam IParamPtr paramlist offset last pos);
		pos, Method m
	| ParamPtr pp ->
		let pos, p = sread_from_table ctx false IParam s pos in
		let p = get_param p in
		pp.pp_param <- p;
		pos, ParamPtr pp
	| Param p ->
		let pos, flags = sread_ui16 s pos in
		let pos, sequence = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		p.p_flags <- param_flags_of_int flags;
		p.p_sequence <- sequence;
		p.p_name <- name;
		pos, Param p
	| InterfaceImpl ii ->
		let pos, cls = sread_from_table ctx false ITypeDef s pos in
		add_relation ctx cls (InterfaceImpl ii);
		let cls = get_type_def cls in
		let pos, interface  = sread_from_table ctx false ITypeDefOrRef s pos in
		ii.ii_class <- cls;
		ii.ii_interface <- interface;
		pos, InterfaceImpl ii
	| MemberRef mr ->
		let pos, cls = sread_from_table ctx false IMemberRefParent s pos in
		let pos, name = read_sstring_idx ctx pos in
		(* print_endline name; *)
		(* let pos, signature = read_ilsig_idx ctx pos in *)
		let pos, signature = read_field_ilsig_idx ~force_field:false ctx pos in
		(* print_endline (ilsig_s signature); *)
		mr.memr_class <- cls;
		mr.memr_name <- name;
		mr.memr_signature <- signature;
		add_relation ctx cls (MemberRef mr);
		pos, MemberRef mr
	| Constant c ->
		let pos, ctype = read_constant_type ctx s pos in
		let pos = pos+1 in
		let pos, parent = sread_from_table ctx false IHasConstant s pos in
		let pos, blobpos = if ctx.blob_offset = 2 then
				sread_ui16 s pos
			else
				sread_i32 s pos
		in
		let blob = ctx.blob_stream in
		let blobpos, _ = read_compressed_i32 blob blobpos in
		let _, value = read_constant ctx ctype blob blobpos in
		c.c_type <- ctype;
		c.c_parent <- parent;
		c.c_value <- value;
		add_relation ctx parent (Constant c);
		pos, Constant c
	| CustomAttribute ca ->
		let pos, parent = sread_from_table ctx false IHasCustomAttribute s pos in
		let pos, t = sread_from_table ctx false ICustomAttributeType s pos in
		let pos = read_custom_attr_idx ctx ca t pos in
		ca.ca_parent <- parent;
		ca.ca_type <- t;
		ca.ca_value <- None; (* this will be delayed by read_custom_attr_idx *)
		add_relation ctx parent (CustomAttribute ca);
		pos, CustomAttribute ca
	| FieldMarshal fm ->
		let pos, parent = sread_from_table ctx false IHasFieldMarshal s pos in
		let pos, nativesig = read_nativesig_idx ctx s pos in
		fm.fm_parent <- parent;
		fm.fm_native_type <- nativesig;
		add_relation ctx parent (FieldMarshal fm);
		pos, FieldMarshal fm
	| DeclSecurity ds ->
		let pos, action = sread_ui16 s pos in
		let action = action_security_of_int action in
		let pos, parent = sread_from_table ctx false IHasDeclSecurity s pos in
		let pos, permission_set = read_sblob_idx ctx pos in
		ds.ds_action <- action;
		ds.ds_parent <- parent;
		ds.ds_permission_set <- permission_set;
		add_relation ctx parent (DeclSecurity ds);
		pos, DeclSecurity ds
	| ClassLayout cl ->
		let pos, psize = sread_ui16 s pos in
		let pos, csize = sread_i32 s pos in
		let pos, parent = sread_from_table ctx false ITypeDef s pos in
		add_relation ctx parent (ClassLayout cl);
		let parent = get_type_def parent in
		cl.cl_packing_size <- psize;
		cl.cl_class_size <- csize;
		cl.cl_parent <- parent;
		pos, ClassLayout cl
	| FieldLayout fl ->
		let pos, offset = sread_i32 s pos in
		let pos, field = sread_from_table ctx false IField s pos in
		fl.fl_offset <- offset;
		fl.fl_field <- get_field field;
		add_relation ctx field (FieldLayout fl);
		pos, FieldLayout fl
	| StandAloneSig sa ->
		let pos, ilsig = read_field_ilsig_idx ~force_field:false ctx pos in
		(* print_endline (ilsig_s ilsig); *)
		sa.sa_signature <- ilsig;
		pos, StandAloneSig sa
	| EventMap em ->
		let startpos = pos in
		let pos, parent = sread_from_table ctx false ITypeDef s pos in
		let offset = pos - startpos in
		let pos, event_list = ctx.table_sizes.(int_of_table IEvent) s pos in
		em.em_parent <- get_type_def parent;
		em.em_event_list <- List.rev_map get_event (read_list ctx IEvent IEventPtr event_list offset last pos);
		add_relation ctx parent (EventMap em);
		pos, EventMap em
	| EventPtr ep ->
		let pos, event = sread_from_table ctx false IEvent s pos in
		ep.ep_event <- get_event event;
		pos, EventPtr ep
	| Event e ->
		let pos, flags = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, event_type = sread_from_table ctx false ITypeDefOrRef s pos in
		e.e_flags <- event_flags_of_int flags;
		e.e_name <- name;
		(* print_endline name; *)
		e.e_event_type <- event_type;
		add_relation ctx event_type (Event e);
		pos, Event e
	| PropertyMap pm ->
		let startpos = pos in
		let pos, parent = sread_from_table ctx false ITypeDef s pos in
		let offset = pos - startpos in
		let pos, property_list = ctx.table_sizes.(int_of_table IProperty) s pos in
		pm.pm_parent <- get_type_def parent;
		pm.pm_property_list <- List.rev_map get_property (read_list ctx IProperty IPropertyPtr property_list offset last pos);
		add_relation ctx parent (PropertyMap pm);
		pos, PropertyMap pm
	| PropertyPtr pp ->
		let pos, property = sread_from_table ctx false IProperty s pos in
		pp.prp_property <- get_property property;
		pos, PropertyPtr pp
	| Property prop ->
		let pos, flags = sread_ui16 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, t = read_field_ilsig_idx ~force_field:false ctx pos in
		prop.prop_flags <- property_flags_of_int flags;
		prop.prop_name <- name;
		(* print_endline name; *)
		prop.prop_type <- t;
		(* print_endline (ilsig_s t); *)
		pos, Property prop
	| MethodSemantics ms ->
		let pos, semantic = sread_ui16 s pos in
		let pos, m = sread_from_table ctx false IMethod s pos in
		let pos, association = sread_from_table ctx false IHasSemantics s pos in
		ms.ms_semantic <- semantic_flags_of_int semantic;
		ms.ms_method <- get_method m;
		ms.ms_association <- association;
		add_relation ctx m (MethodSemantics ms);
		add_relation ctx association (MethodSemantics ms);
		pos, MethodSemantics ms
	| MethodImpl mi ->
		let pos, cls = sread_from_table ctx false ITypeDef s pos in
		let pos, method_body = sread_from_table ctx false IMethodDefOrRef s pos in
		let pos, method_declaration = sread_from_table ctx false IMethodDefOrRef s pos in
		mi.mi_class <- get_type_def cls;
		mi.mi_method_body <- method_body;
		mi.mi_method_declaration <- method_declaration;
		add_relation ctx method_body (MethodImpl mi);
		pos, MethodImpl mi
	| ModuleRef modr ->
		let pos, name = read_sstring_idx ctx pos in
		modr.modr_name <- name;
		(* print_endline name; *)
		pos, ModuleRef modr
	| TypeSpec ts ->
		let pos, signature = read_ilsig_idx ctx pos in
		(* print_endline (ilsig_s signature); *)
		ts.ts_signature <- signature;
		pos, TypeSpec ts
	| ENCLog el ->
		let pos, token = sread_i32 s pos in
		let pos, func_code = sread_i32 s pos in
		el.el_token <- token;
		el.el_func_code <- func_code;
		pos, ENCLog el
	| ImplMap im ->
		let pos, flags = sread_ui16 s pos in
		let pos, forwarded = sread_from_table ctx false IMemberForwarded s pos in
		let pos, import_name = read_sstring_idx ctx pos in
		let pos, import_scope = sread_from_table ctx false IModuleRef s pos in
		im.im_flags <- impl_flags_of_int flags;
		im.im_forwarded <- forwarded;
		im.im_import_name <- import_name;
		im.im_import_scope <- get_module_ref import_scope;
		add_relation ctx forwarded (ImplMap im);
		pos, ImplMap im
	| ENCMap em ->
		let pos, token = sread_i32 s pos in
		em.encm_token <- token;
		pos, ENCMap em
	| FieldRVA f ->
		let pos, rva = sread_real_i32 s pos in
		let pos, field = sread_from_table ctx false IField s pos in
		f.fr_rva <- rva;
		f.fr_field <- get_field field;
		add_relation ctx field (FieldRVA f);
		pos, FieldRVA f
	| Assembly a ->
		let pos, hash_algo = sread_i32 s pos in
		let pos, major = sread_ui16 s pos in
		let pos, minor = sread_ui16 s pos in
		let pos, build = sread_ui16 s pos in
		let pos, rev = sread_ui16 s pos in
		let pos, flags = sread_i32 s pos in
		let pos, public_key = read_sblob_idx ctx pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, locale = read_sstring_idx ctx pos in
		a.a_hash_algo <- hash_algo_of_int hash_algo;
		a.a_major <- major;
		a.a_minor <- minor;
		a.a_build <- build;
		a.a_rev <- rev;
		a.a_flags <- assembly_flags_of_int flags;
		a.a_public_key <- public_key;
		a.a_name <- name;
		a.a_locale <- locale;
		pos, Assembly a
	| AssemblyProcessor ap ->
		let pos, processor = sread_i32 s pos in
		ap.ap_processor <- processor;
		pos, AssemblyProcessor ap
	| AssemblyOS aos ->
		let pos, platform_id = sread_i32 s pos in
		let pos, major = sread_i32 s pos in
		let pos, minor = sread_i32 s pos in
		aos.aos_platform_id <- platform_id;
		aos.aos_major_version <- major;
		aos.aos_minor_version <- minor;
		pos, AssemblyOS aos
	| AssemblyRef ar ->
		let pos, major = sread_ui16 s pos in
		let pos, minor = sread_ui16 s pos in
		let pos, build = sread_ui16 s pos in
		let pos, rev = sread_ui16 s pos in
		let pos, flags = sread_i32 s pos in
		let pos, public_key = read_sblob_idx ctx pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, locale = read_sstring_idx ctx pos in
		let pos, hash_value = read_sblob_idx ctx pos in
		ar.ar_major <- major;
		ar.ar_minor <- minor;
		ar.ar_build <- build;
		ar.ar_rev <- rev;
		ar.ar_flags <- assembly_flags_of_int flags;
		ar.ar_public_key <- public_key;
		ar.ar_name <- name;
		(* print_endline name; *)
		ar.ar_locale <- locale;
		(* print_endline locale; *)
		ar.ar_hash_value <- hash_value;
		pos, AssemblyRef ar
	| AssemblyRefProcessor arp ->
		let pos, processor = sread_i32 s pos in
		let pos, assembly_ref = sread_from_table ctx false IAssemblyRef s pos in
		arp.arp_processor <- processor;
		arp.arp_assembly_ref <- get_assembly_ref assembly_ref;
		pos, AssemblyRefProcessor arp
	| AssemblyRefOS aros ->
		let pos, platform_id = sread_i32 s pos in
		let pos, major = sread_i32 s pos in
		let pos, minor = sread_i32 s pos in
		let pos, assembly_ref = sread_from_table ctx false IAssemblyRef s pos in
		aros.aros_platform_id <- platform_id;
		aros.aros_major <- major;
		aros.aros_minor <- minor;
		aros.aros_assembly_ref <- get_assembly_ref assembly_ref;
		pos, AssemblyRefOS aros
	| File file ->
		let pos, flags = sread_i32 s pos in
		let pos, name = read_sstring_idx ctx pos in
		let pos, hash_value = read_sblob_idx ctx pos in
		file.file_flags <- file_flag_of_int flags;
		file.file_name <- name;
		(* print_endline ("file " ^ name); *)
		file.file_hash_value <- hash_value;
		pos, File file
	| ExportedType et ->
		let pos, flags = sread_i32 s pos in
		let pos, type_def_id = sread_i32 s pos in
		let pos, type_name = read_sstring_idx ctx pos in
		let pos, type_namespace = read_sstring_idx ctx pos in
		let pos, impl = sread_from_table ctx false IImplementation s pos in
		et.et_flags <- type_def_flags_of_int flags;
		et.et_type_def_id <- type_def_id;
		et.et_type_name <- type_name;
		et.et_type_namespace <- parse_ns type_namespace;
		et.et_implementation <- impl;
		add_relation ctx impl (ExportedType et);
		pos, ExportedType et
	| ManifestResource mr ->
		let pos, offset = sread_i32 s pos in
		let pos, flags = sread_i32 s pos in
		(* printf "offset 0x%x flags 0x%x\n" offset flags; *)
		let pos, name = read_sstring_idx ctx pos in
		let rpos, i = ctx.table_sizes.(int_of_table IImplementation) s pos in
		let pos, impl =
			if i = 0 then
				rpos, None
			else
				let pos, ret = sread_from_table ctx false IImplementation s pos in
				add_relation ctx ret (ManifestResource mr);
				pos, Some ret
		in
		mr.mr_offset <- offset;
		mr.mr_flags <- manifest_resource_flag_of_int flags;
		mr.mr_name <- name;
		mr.mr_implementation <- impl;
		pos, ManifestResource mr
	| NestedClass nc ->
		let pos, nested = sread_from_table ctx false ITypeDef s pos in
		let pos, enclosing = sread_from_table ctx false ITypeDef s pos in
		nc.nc_nested <- get_type_def nested;
		nc.nc_enclosing <- get_type_def enclosing;

		assert (nc.nc_nested.td_extra_enclosing = None);
		nc.nc_nested.td_extra_enclosing <- Some nc.nc_enclosing;
		add_relation ctx enclosing (NestedClass nc);
		pos, NestedClass nc
	| GenericParam gp ->
		let pos, number = sread_ui16 s pos in
		let pos, flags = sread_ui16 s pos in
		let pos, owner = sread_from_table ctx false ITypeOrMethodDef s pos in
		let spos, nidx =
			if ctx.strings_offset = 2 then
				sread_ui16 s pos
			else
				sread_i32 s pos
		in
		let pos, name =
			if nidx = 0 then
				spos, None
			else
				let pos, ret = read_sstring_idx ctx pos in
				(* print_endline ret; *)
				pos, Some ret
		in
		gp.gp_number <- number;
		gp.gp_flags <- generic_flags_of_int flags;
		gp.gp_owner <- owner;
		gp.gp_name <- name;
		add_relation ctx owner (GenericParam gp);
		pos, GenericParam gp
	| MethodSpec mspec ->
		let pos, meth = sread_from_table ctx false IMethodDefOrRef s pos in
		let pos, instantiation = read_method_ilsig_idx ctx pos in
		(* print_endline (ilsig_s instantiation); *)
		mspec.mspec_method <- meth;
		mspec.mspec_instantiation <- instantiation;
		add_relation ctx meth (MethodSpec mspec);
		pos, MethodSpec mspec
	| GenericParamConstraint gc ->
		let pos, owner = sread_from_table ctx false IGenericParam s pos in
		let pos, c = sread_from_table ctx false ITypeDefOrRef s pos in
		gc.gc_owner <- get_generic_param owner;
		gc.gc_constraint <- c;
		add_relation ctx owner (GenericParamConstraint gc);
		pos, GenericParamConstraint gc
	| _ -> assert false

(* ******* META READING ********* *)

let preset_sizes ctx rows =
	Array.iteri (fun n r -> match r with
		| false,_ -> ()
		| true,nrows ->
			(* printf "table %d nrows %d\n" n nrows; *)
			let tbl = table_of_int n in
			ctx.tables.(n) <- DynArray.init (nrows) (fun id -> mk_meta tbl (id+1))
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
			(* print_endline (string_of_int n); *)
			let fn = read_table_at ctx (table_of_int n) in
			let rec loop_fn n =
				if n = nrows then
					()
				else begin
					let p, _ = fn n (n = (nrows-1)) !pos in
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

let read_meta_tables pctx header module_cache =
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

    module_cache = module_cache;
		extra_streams = !extra;
		relations = Hashtbl.create 64;
		typedefs = Hashtbl.create 64;
		tables = tables;
		table_sizes = Array.make (max_clr_meta_idx+1) sread_ui16;

		delays = [];
	} in
	read_meta ctx;
	let delays = ctx.delays in
	ctx.delays <- [];
	List.iter (fun fn -> fn()) delays;
	assert (ctx.delays = []);
	{
		il_tables = ctx.tables;
		il_relations = ctx.relations;
		il_typedefs = ctx.typedefs;
	}

