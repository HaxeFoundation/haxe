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
	(* record id on a specified meta table *)

type clr_meta_idx =
	(* strongly-type each table index *)
	| IModule | ITypeRef | ITypeDef | IFieldPtr
	| IField | IMethodPtr | IMethod | IParamPtr
	| IParam | IInterfaceImpl | IMemberRef | IConstant
	| ICustomAttribute | IFieldMarshal | IDeclSecurity
	| IClassLayout | IFieldLayout | IStandAloneSig
	| IEventMap | IEventPtr | IEvent | IPropertyMap
	| IPropertyPtr | IProperty | IMethodSemantics
	| IMethodImpl | IModuleRef | ITypeSpec | IImplMap
	| IFieldRVA | IENCLog | IENCMap | IAssembly
	| IAssemblyProcessor | IAssemblyOS | IAssemblyRef
	| IAssemblyRefProcessor | IAssemblyRefOS
	| IFile | IExportedType | IManifestResource | INestedClass
	| IGenericParam | IMethodSpec | IGenericParamConstraint

type meta_pointer = clr_meta_idx * rid
	(* generic reference to the meta table *)

(* starting with all annotations of special coded types *)
type type_def_or_ref = clr_meta
and has_const = clr_meta
and has_custom_attribute = clr_meta
and has_field_marshal = clr_meta
and has_decl_security = clr_meta
and member_ref_parent = clr_meta
and has_semantics = clr_meta
and method_def_or_ref = clr_meta
and member_forwarded = clr_meta
and implementation = clr_meta
and custom_attribute_type = clr_meta
and resolution_scope = clr_meta

and clr_meta =
	| Module of meta_module
		(* the current module descriptor *)
	| TypeRef of meta_type_ref
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

and meta_module = {
	mutable m_generation : int;
	mutable m_name : stringref;
	mutable m_vid : guid;
	mutable m_encid : guid;
	mutable m_encbase_id : guid;
}

and meta_type_ref = {
	mutable tr_resolution_scope : resolution_scope;
	mutable tr_name : stringref;
	mutable tr_namespace : stringref;
}

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
