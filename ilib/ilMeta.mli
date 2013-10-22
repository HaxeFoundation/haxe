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

(* useful types for describing CLI metadata *)

type clr_meta_table =
	| Module
		(* the current module descriptor *)
	| TypeRef
		(* class reference descriptors *)
	| TypeDef
		(* class or interface definition descriptors *)
	| FieldPtr
		(* a class-to-fields lookup table - does not exist in optimized metadatas *)
	| Field
		(* field definition descriptors *)
	| MethodPtr
		(* a class-to-methods lookup table - does not exist in optimized metadatas *)
	| Method
		(* method definition descriptors *)
	| ParamPtr
		(* a method-to-parameters lookup table - does not exist in optimized metadatas *)
	| Param
		(* parameter definition descriptors *)
	| InterfaceImpl
		(* interface implementation descriptors *)
	| MemberRef
		(* member (field or method) reference descriptors *)
	| Constant
		(* constant value that map the default values stored in the #Blob stream to *)
		(* respective fields, parameters and properties *)
	| CustomAttribute
		(* custom attribute descriptors *)
	| FieldMarshal
		(* field or parameter marshaling descriptors for managed/unmanaged interop *)
	| DeclSecurity
		(* security descriptors *)
	| ClassLayout
		(* class layout descriptors that hold information about how the loader should lay out respective classes *)
	| FieldLayout
		(* field layout descriptors that specify the offset or oridnal of individual fields *)
	| StandAloneSig
		(* stand-alone signature descriptors. used in two capacities: *)
		(* as composite signatures of local variables of methods *)
		(* and as parameters of the call indirect (calli) IL instruction *)
	| EventMap
		(* a class-to-events mapping table. exists also in optimized metadatas *)
	| EventPtr
		(* an event map-to-events lookup table - does not exist in optimized metadata *)
	| Event
		(* event descriptors *)
	| PropertyMap
		(* a class-to-properties mapping table. exists also in optimized metadatas *)
	| PropertyPtr
		(* a property map-to-properties lookup table - does not exist in optimized metadata *)
	| Property
		(* property descriptors *)
	| MethodSemantics
		(* method semantics descriptors that hold information about which method is associated *)
		(* with a specific property or event and in what capacity *)
	| MethodImpl
		(* method implementation descriptors *)
	| ModuleRef
		(* module reference descriptors *)
	| TypeSpec
		(* Type specification descriptors *)
	| ImplMap
		(* implementation map descriptors used for platform invocation (P/Invoke) *)
	| FieldRVA
		(* field-to-data mapping descriptors *)
	| ENCLog
		(* edit-and-continue log descriptors that hold information about what changes *)
		(* have been made to specific metadata items during in-memory editing *)
		(* this table does not exist on optimized metadata *)
	| ENCMap
		(* edit-and-continue mapping descriptors. does not exist on optimized metadata *)
	| Assembly
		(* the current assembly descriptor, which should appear only in the prime module metadata *)
	| AssemblyProcessor | AssemblyOS
		(* unused *)
	| AssemblyRef
		(* assembly reference descriptors *)
	| AssemblyRefProcessor | AssemblyRefOS
		(* unused *)
	| File
		(* file descriptors that contain information about other files in the current assembly *)
	| ExportedType
		(* exported type descriptors that contain information about public classes *)
		(* exported by the current assembly, which are declared in other modules of the assembly *)
		(* only the prime module of the assembly should carry this table *)
	| ManifestResource
		(* managed resource descriptors *)
	| NestedClass
		(* nested class descriptors that provide mapping of nested classes to their respective enclosing classes *)
	| GenericParam
		(* type parameter descriptors for generic classes and methods *)
	| MethodSpec
		(* generic method instantiation descriptors *)
	| GenericParamConstraint
		(* descriptors of constraints specified for type parameters of generic classes and methods *)
	| UnknownMeta of int

type uint16 = int

type guid = string
	(* reference from the #GUID stream *)
type stringref = string
	(* reference from the #Strings stream *)
type id = stringref
	(* a stringref that references an identifier. *)
	(* must begin with an alphabetic character, or the following characters: *)
		(* #, $, @, _ *)
	(* and continue with alphanumeric characters or one of the following: *)
		(* ?, $, @, _, ` *)

type rid = int

type clr_meta_stream = {
	(* smeta_reserved : int32; *)
		(* reserved: always 0 *)
	smeta_major : int;
	smeta_minor : int;
	(* smeta_heaps_sizes : int; *)
		(* bitflag that annotates each table heaps, if offset can be 2-bytes (unset) or 4 bytes (set) *)
		(* 0x1 - #String; 0x2 - #GUID; 0x4 - #Blob *)
		(* if the meta stream is a #- stream, flag 0x20 indicates that the stream *)
		(* contains only changes made during an edit-and-continue session, and flag 0x80 indicates *)
		(* that the metadata might contain items marked as deleted *)
		(* the next fields is the uncompressed version of these infos *)
	smeta_size_string : int;
	smeta_size_guid : int;
	smeta_size_blob : int;
	(* only valid for #- (uncompressed) metadata streams *)
	smeta_edit_and_continue : bool;
	smeta_has_deleted : bool;

	(* smeta_max_record_index_bit : int; *)
		(* byte that represents the bit width of the maximal record index to all tables of the metadata *)
		(* doesn't seem to be needed *)
	smeta_tables : clr_meta_table array;
}
type meta_pointer = clr_meta_table * rid
	(* generic reference to the meta table *)

type 'a delayed = 'a ref

type ilpath = id * (id list)


type ilsig =
	(* primitive types *)
	| SVoid (* 0x1 *)
		(* obsolete: should not be used *)
	| SBool (* 0x2 *)
	| SChar (* 0x3 *)
	| SInt8 (* 0x4 *)
	| SUInt8 (* 0x5 *)
	| SInt16 (* 0x6 *)
	| SUInt16 (* 0x7 *)
	| SInt32 (* 0x8 *)
	| SUInt32 (* 0x9 *)
	| SInt64 (* 0xA *)
	| SUInt64 (* 0xB *)
	| SFloat32 (* 0xC *)
	| SFloat64 (* 0xD *)
	| SString (* 0xE *)
	| SPointer of ilsig (* 0xF *)
		(* unmanaged pointer to type ( * ) *)
	| SManagedPointer of ilsig (* 0x10 *)
		(* managed pointer to type ( & ) *)
	| SValueType of meta_pointer (*type_def_or_ref*) (* 0x11 *)
		(* a value type modifier, followed by TypeDef or TypeRef token *)
	| SClass of meta_pointer (* type_def_or_ref *) (* 0x12 *)
		(* a class type modifier, followed by TypeDef or TypeRef token *)
	| STypeParam of int (* 0x13 *)
		(* generic parameter in a generic type definition. represented by a number *)
	| SArray of (ilsig * int * int) list (* 0x14 *)
		(* a multi-dimensional array type modifier *)
		(* encoded like: *)
			(* SArray <underlying type><rank><num_sizes><size1>...<sizeN>
			          <num_lower_bounds><lower_bound1>...<lower_boundM> *)
			(* <rank> is the number of dimensions (K>0) *)
			(* <num_sizes> num of specified sizes for dimensions (N <= K) *)
			(* <num_lower_bounds> num of lower bounds (M <= K) *)
			(* all int values are compressed *)
	| SGenericInst of ilsig * (ilsig list)
		(* A generic type instantiation. encoded like: *)
			(* SGenericInst <type> <type-arg-count> <type1>...<typeN> *)
	| STypedReference (* 0x16 *)
		(* typed reference, carrying both a reference to a type *)
		(* and information identifying the referenced type *)
	| SIntPtr (* 0x18 *)
		(* pointer-sized managed integer *)
	| SUIntPtr (* 0x19 *)
		(* pointer-size managed unsigned integer *)
	| SFunPtr of ilsig * (ilsig list) (* 0x1B *)
		(* a pointer to a function, followed by full method signature *)
	| SExtra of ilsig_extra
	| SObsolete of ilsig_obsolete

and ilsig_extra =
	(* extra types *)
	| SBStr (* 0x13 *)
		(* unicode VB-style: used in COM operations *)
	| SLPStr (* 0x14 *)
		(* pointer to a zero-terminated ANSI string *)
	| SLPWStr (* 0x15 *)
		(* pointer to a zero-terminated Unicode string *)
	| SLPTStr (* 0x16 *)
		(* pointer to a zero-terminated ANSI or Unicode string - depends on platform *)

and ilsig_obsolete =
	| SSysChar (* 0x0D *)
	| SVariant (* 0x0E *)
	| SObsoletePointer (* 0x10 *)
	| SDecimal (* 0x11 *)
	| SDate (* 0x12 *)


type typedef_or_ref = (* 64 *)
	(* tag size: 2 *)
	| TTypeDef of typedef_or_ref (* 0 *)
	| TTypeRef of int (* 1 *)
	| TTypeSpec of int (* 2 *)
	| TypeDefOrRef of meta_pointer
		(* this pointer will be replaced by the real instance once the whole table is loaded *)

and has_constant = (* 65 *)
	(* tag size: 2 *)
	| CField of int (* 0 *)
	| CParam of int (* 1 *)
	| CProperty of int (* 2 *)
	| HasConstant of meta_pointer
		(* this pointer will be replaced by the real instance once the whole table is loaded *)

and has_custom_attribute = (* 66 *)
	(* tag size: 5 *)
	| AMethod of int (* 0 *)
	| AField of int (* 1 *)
	| ATypeRef of int (* 2 *)
	| ATypeDef of int (* 3 *)
	| AParam of int (* 4 *)
	| AInterfaceImpl of int (* 5 *)
	| AMemberRef of int (* 6 *)
	| AModule of int (* 7 *)
	| ADeclSecurity of int (* 8 *)
	| AProperty of int (* 9 *)
	| AEvent of int (* 10 *)
	| AStandAloneSig of int (* 11 *)
	| AModuleRef of int (* 12 *)
	| ATypeSpec of int (* 13 *)
	| AAssembly of int (* 14 *)
	| AAssemblyRef of int (* 15 *)
	| AFile of int (* 16 *)
	| AExportedType of int (* 17 *)
	| AManifestResource of int (* 18 *)
	| AGenericParam of int (* 19 *)
	| AGenericParamConstraint of int (* 20 *)
	| AMethodSpec of int (* 21 *)
	| HasCustomAttribute of meta_pointer

and has_field_marshal = (* 67 *)
	(* tag size: 1 *)
	| MField of int (* 0 *)
	| MParam of int (* 1 *)
	| HasFieldMarshal of meta_pointer

and has_decl_security = (* 68 *)
	(* tag size: 2 *)
	| STypeDef of int (* 0 *)
	| SMethod of int (* 1 *)
	| SAssembly of int (* 2 *)
	| HasDeclSecurity of meta_pointer

and member_ref_parent = (* 69 *)
	(* tag size: 3 *)
	| PTypeDef of int (* 0 *)
	| PTypeRef of int (* 1 *)
	| PModuleRef of int (* 2 *)
	| PMethod of int (* 3 *)
	| PTypeSpec of int (* 4 *)
	| MemberRefParent of meta_pointer

and has_semantics = (* 70 *)
	(* tag size: 1 *)
	| SEvent of int (* 0 *)
	| SProperty of int (* 1 *)
	| HasSemantics of meta_pointer

and method_def_or_ref = (* 71 *)
	(* tag size: 1 *)
	| MMethod of int (* 0 *)
	| MMemberRef of int (* 1 *)
	| MethodDefOrRef of meta_pointer

and member_forwarded = (* 72 *)
	(* tag size 1 *)
	| FField of int (* 0 *)
	| FMethod of int (* 1 *)
	| MemberForwarded of meta_pointer

and implementation = (* 73 *)
	(* tag size 2 *)
	| IFile of int (* 0 *)
	| IAssemblyRef of int (* 1 *)
	| IExportedType of int (* 2 *)
	| Implementation of meta_pointer

and custom_attribute_type = (* 74 *)
	(* tag size 3 *)
	| CTypeRef (* 0 *)
		(* obsolete, must not be used *)
	| CTypeDef (* 1 *)
		(* obsolete, must not be used *)
	| CMethod of int (* 2 *)
	| CMemberRef of int (* 3 *)
	| CString (* 4 *)
		(* obsolete, must not be used *)
	| CustomAttributeType of meta_pointer

and resolution_scope = (* 75 *)
	(* tag size 2 *)
	| RModule of int (* 0 *)
	| RModuleRef of int (* 1 *)
	| RAssemblyRef of int (* 2 *)
	| RTypeRef of int (* 3 *)
	| ResolutionScope of meta_pointer

and type_or_method_def = (* 76 *)
	(* tag size 1 - only 2.0+ *)
	| TMTypeDef of int (* 0 *)
	| TMMethod of int (* 1 *)
	| TypeOrMethodDef of meta_pointer

type clr_module = {
	m_generation : uint16;
		(* used to annotate version if edit-and-continue mode is turned on *)
	m_name : id;
		(* the module name, which is the same as the name of the executable file *)
		(* with its extension but wihtout a path. The length should not exceed 512 bytes *)
	m_uid : guid;
		(* a GUID from the #GUID stream *)
	m_encid : guid;
		(* used only in edit-and-continue mode *)
	m_baseid : guid;
		(* used only in edit-and-continue mode *)
}

type type_ref = {
	tr_rscope : meta_pointer; (* ResolutionScope *)
	tr_name : id;
	tr_ns : id;
}

type type_def_vis =
	(* visibility flags - mask 0x7 *)
	| VPrivate (* 0x0 *)
		(* type is not visible outside the assembly. default *)
	| VPublic (* 0x1 *)
		(* type visible outside the assembly *)
	| VNestedPublic (* 0x2 *)
		(* the nested type has public visibility *)
	| VNestedPrivate (* 0x3 *)
		(* nested type has private visibility - it's not visible outside the enclosing class *)
	| VNestedFamily (* 0x4 *)
		(* nested type has family visibility - it's visible to descendants of the enclosing class only *)
	| VNestedAssembly (* 0x5 *)
		(* nested type visible within the assembly only *)
	| VNestedFamAndAssem (* 0x6 *)
		(* nested type is visible to the descendants of the enclosing class residing in the same assembly *)
	| VNestedFamOrAssem (* 0x7 *)
		(* nested type is visible to the descendants of the enclosing class either within *)
		(* or outside the assembly and to every type within the assembly *)
	
type type_def_layout =
	(* layout flags - mask 0x18 *)
	| LAuto (* 0x0 *)
		(* type fields are laid out automatically *)
	| LSequential (* 0x8 *)
		(* loader must preserve the order of the instance fields *)
	| LExplicit (* 0x10 *)
		(* type layout is specified explicitly *)

type type_def_semantics =
	(* semantics flags - mask 0x5A0 *)
	| SNormal (* 0x0 *)
		(* either a class or a value type *)
	| SInterface (* 0x20 *)
		(* type is an interface. If specified, the default parent is set to nil *)
	| SAbstract (* 0x80 *)
	| SSealed (* 0x100 *)
	| SSpecialName (* 0x400 *)
		(* type has a special name. how special depends on the name itself *)
		(* e.g. .ctor or .cctor *)

type type_def_impl =
	(* type implementation flags - mask 0x103000 *)
	| IImport (* 0x1000 *)
		(* the type is imported from a COM type library *)
	| ISerializable (* 0x2000 *)
		(* the type can be serialized into sequential data *)
	| IBeforeFieldInit (* 0x00100000 *)
		(* the type can be initialized any time before the first access *)
		(* to a static field. *)
	
type type_def_string =
	(* string formatting flags - mask 0x00030000 *)
	| SAnsi (* 0x0 *)
		(* managed strings are marshaled to and from ANSI strings *)
	| SUnicode (* 0x00010000 *)
		(* managed strings are marshaled to and from UTF-16 *)
	| SAutoChar (* 0x00020000 *)
		(* marshaling is defined by the underlying platform *)

type type_def_flags = {
	tdf_vis : type_def_vis;
	tdf_layout : type_def_layout;
	tdf_semantics : type_def_semantics;
	tdf_impl : type_def_impl list;
	tdf_string : type_def_string;
}

type type_def = {
	td_flags : type_def_flags;
	td_name : id;
	td_ns : id;
	td_extends : meta_pointer;
	td_fields : meta_pointer; (* Field table *)
		(* a record index in the Field table, marking *)
		(* the start of the field records belonging to this type *)
	td_methods : meta_pointer; (* Method table *)
		(* a record index in the Method table, marking *)
		(* the start of the method records belonging to this type *)
}

type field_ptr = {
	fp_field : meta_pointer; (* Field table *)
}

(* type field = { *)
	(* f_flags : field_flags; *)
	(* f_name : id; *)
	(* f_sig : blob; *)
(* } *)
