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
	(* reserved metas *)
	| IR0x2D | IR0x2E | IR0x2F
	| IR0x30 | IR0x31 | IR0x32 | IR0x33 | IR0x34 | IR0x35 | IR0x36 | IR0x37 
	| IR0x38 | IR0x39 | IR0x3A | IR0x3B | IR0x3C | IR0x3D | IR0x3E | IR0x3F
	(* coded tokens *)
	| ITypeDefOrRef | IHasConstant | IHasCustomAttribute
	| IHasFieldMarshal | IHasDeclSecurity | IMemberRefParent
	| IHasSemantics | IMethodDefOrRef | IMemberForwarded | IImplementation
	| ICustomAttributeType | IResolutionScope | ITypeOrMethodDef

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
and type_or_method_def = clr_meta

and clr_meta =
	| Module of meta_module
		(* the current module descriptor *)
	| TypeRef of meta_type_ref
		(* class reference descriptors *)
	| TypeDef of meta_type_def
		(* class or interface definition descriptors *)
	| FieldPtr of meta_field_ptr
		(* a class-to-fields lookup table - does not exist in optimized metadatas *)
	| Field of meta_field
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

and meta_type_def = {
	mutable td_flags : type_def_flags;
	mutable td_name : stringref;
	mutable td_namespace : stringref;
	mutable td_extends : type_def_or_ref;
	mutable td_field_list : rid;
	mutable td_method_list : rid;
}

and meta_field_ptr = {
	mutable fp_field : rid;
}

and meta_field = {
	mutable f_flags : field_flags;
	mutable f_name : stringref;
	mutable f_signature : ilsig;
}

and type_def_vis =
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
	
and type_def_layout =
	(* layout flags - mask 0x18 *)
	| LAuto (* 0x0 *)
		(* type fields are laid out automatically *)
	| LSequential (* 0x8 *)
		(* loader must preserve the order of the instance fields *)
	| LExplicit (* 0x10 *)
		(* type layout is specified explicitly *)

and type_def_semantics =
	(* semantics flags - mask 0x5A0 *)
	| SInterface (* 0x20 *)
		(* type is an interface. If specified, the default parent is set to nil *)
	| SAbstract (* 0x80 *)
	| SSealed (* 0x100 *)
	| SSpecialName (* 0x400 *)
		(* type has a special name. how special depends on the name itself *)
		(* e.g. .ctor or .cctor *)

and type_def_impl =
	(* type implementation flags - mask 0x103000 *)
	| IImport (* 0x1000 *)
		(* the type is imported from a COM type library *)
	| ISerializable (* 0x2000 *)
		(* the type can be serialized into sequential data *)
	| IBeforeFieldInit (* 0x00100000 *)
		(* the type can be initialized any time before the first access *)
		(* to a static field. *)
	
and type_def_string =
	(* string formatting flags - mask 0x00030000 *)
	| SAnsi (* 0x0 *)
		(* managed strings are marshaled to and from ANSI strings *)
	| SUnicode (* 0x00010000 *)
		(* managed strings are marshaled to and from UTF-16 *)
	| SAutoChar (* 0x00020000 *)
		(* marshaling is defined by the underlying platform *)

and type_def_flags = {
	tdf_vis : type_def_vis;
	tdf_layout : type_def_layout;
	tdf_semantics : type_def_semantics list;
	tdf_impl : type_def_impl list;
	tdf_string : type_def_string;
}

and field_access =
	(* access flags - mask 0x07 *)
	| FAPrivateScope (* 0x0 *)
		(* default - exempt from the requirement of having a unique triad of owner, name and signature *)
		(* so it must always be referenced by a FieldDef token and never by a MemberRef *)
		(* privatescope fields are accessible from anywhere within the current module *)
	| FAPrivate (* 0x1 *)
		(* field is accessible from its owner and from classes nested in the field's owner. *)
		(* global private fields are accessible from anywhere within current module *)
	| FAFamAndAssem (* 0x2 *)
		(* accessible from types belonging to the owner's family defined in the current assembly *)
		(* family means the type itself and all its descendants *)
	| FAAssembly (* 0x3 *)
		(* accessible from types defined in the current assembly *)
	| FAFamily (* 0x4 *)
		(* accessible from the owner's family - defined in this or any other assembly *)
	| FAFamOrAssem (* 0x5 *)
		(* accessible from the owner's family and from all types defined in the current assembly *)
	| FAPublic (* 0x6 *)
		(* field is accessible from any type *)

and field_contract =
	(* contract flags - mask 0x02F0 *)
	| CStatic (* 0x10 *)
		(* static field. global fields must be static *)
	| CInitOnly (* 0x20 *)
		(* field can be initialized only and cannot be written to later. *)
		(* Initialization takes place in an instance constructor (.ctor) for instance fields *)
		(* and in a class constructor (.cctor) for static fields. *)
		(* this flag is not enforced by the CLR *)
	| CLiteral (* 0x40 *)
		(* field is a compile-time constant. the loader does not lay out this field *)
		(* and does not create an internal handle for it *)
		(* it cannot be directly addressed from IL and can only be used as a Reflection reference *)
	| CNotSerialized (* 0x80 *)
		(* field is not serialized when the owner is remoted *)
	| CSpecialName (* 0x200 *)
		(* the field is special in some way, as defined by its name *)
		(* example is the field value__ of an enumeration type *)

and field_reserved = 
	(* reserved flags - cannot be set explicitly. mask 0x9500 *)
	| RSpecialName (* 0x400 *)
		(* has a special name that is reserved for internal use of the CLR *)
		(* two field names are reserved: value_, for instance fields in enumerations *)
		(* and _Deleted* for fields marked for deletion but not actually removed from metadata *)
	| RMarshal (* 0x1000 *)
		(* The field has an associated FieldMarshal record specifying how the field must be *)
		(* marshaled when consumed by unmanaged code. *)
	| RConstant (* 0x8000 *)
		(* field has an associated Constant record *)
	| RFieldRVA (* 0x0100 *)
		(* field is mapped to data and has an associated FieldRVA record *)

and field_flags = {
	ff_access : field_access;
	ff_contract : field_contract list;
	ff_reserved : field_reserved list;
}

and ilsig =
	(* primitive types *)
	| SVoid (* 0x1 *)
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
	| SValueType of type_def_or_ref (* 0x11 *)
		(* a value type modifier, followed by TypeDef or TypeRef token *)
	| SClass of type_def_or_ref (* 0x12 *)
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
	| SGenericInst of ilsig * (ilsig list) (* 0x15 *)
		(* A generic type instantiation. encoded like: *)
			(* SGenericInst <type> <type-arg-count> <type1>...<typeN> *)
	| STypedReference (* 0x16 *)
		(* typed reference, carrying both a reference to a type *)
		(* and information identifying the referenced type *)
	| SIntPtr (* 0x18 *)
		(* pointer-sized managed integer *)
	| SUIntPtr (* 0x19 *)
		(* pointer-size managed unsigned integer *)
	(* | SNativeFloat (* 0x1A *) *)
		(* refer to http://stackoverflow.com/questions/13961205/native-float-type-usage-in-clr *)
	| SFunPtr of ilsig * (ilsig list) (* 0x1B *)
		(* a pointer to a function, followed by full method signature *)
	| SObject (* 0x1C *)
		(* System.Object *)
	| SVector of ilsig (* 0x1D *)
		(* followed by the encoding of the underlying type *)
	| SReqModifier of type_def_or_ref * ilsig (* 0x1F *)
		(* modreq: required custom modifier : indicate that the item to which they are attached *)
		(* must be treated in a special way *)
	| SOptModifier of type_def_or_ref * ilsig (* 0x20 *)
		(* modopt: optional custom modifier *)
	| SSentinel (* 0x41 *)
		(* ... - signifies the beginning of optional arguments supplied for a vararg method call *)
		(* This can only appear at call site, since varargs optional parameters are not specified *)
		(* when a method is declared *)
	| SPinned of ilsig (* 0x45 *)
		(* pinned reference: it's only applicable to local variables only *)
	
and nativesig =
	| NVoid (* 0x01 *)
		(* obsolete *)
	| NBool (* 0x02 *)
	| NInt8 (* 0x03 *)
	| NUInt8 (* 0x4 *)
	| NInt16 (* 0x5 *)
	| NUInt16 (* 0x6 *)
	| NInt32 (* 0x7 *)
	| NUInt32 (* 0x8 *)
	| NInt64 (* 0x9 *)
	| NUInt64 (* 0xA *)
	| NFloat32 (* 0xB *)
	| NFloat64 (* 0xC *)
	| NSysChar (* 0xD *)
		(* obsolete *)
	| NVariant (* 0xE *)
		(* obsolete *)
	| NCurrency (* 0xF *)
	| NPointer (* 0x10 *)
		(* obsolete - use NativeInt *)
	| NDecimal (* 0x11 *)
		(* obsolete *)
	| NDate (* 0x12 *)
		(* obsolete *)
	| NBStr (* 0x13 *)
		(* unicode VB-style: used in COM operations *)
	| NLPStr (* 0x14 *)
		(* pointer to a zero-terminated ANSI string *)
	| NLPWStr (* 0x15 *)
		(* pointer to a zero-terminated Unicode string *)
	| NLPTStr (* 0x16 *)
		(* pointer to a zero-terminated ANSI or Unicode string - depends on platform *)
	| NFixedString of int (* 0x17 *)
		(* fixed-size system string of size <size> bytes; applicable to field marshalling only *)
	| NObjectRef (* 0x18 *)
		(* obsolete *)
	| NUnknown (* 0x19 *)
		(* IUnknown interface pointer *)
	| NDispatch (* 0x1A *)
		(* IDispatch interface pointer *)
	| NStruct (* 0x1B *)
		(* C-style structure, for marshaling the formatted managed types *)
	| NInterface (* 0x1C *)
		(* interface pointer *)
	| NSafeArray of variantsig (* 0x1D *)
		(* safe array of type <variant-type> *)
	| NFixedArray of int (* 0x1E *)
		(* fixed-size array, of size <size> bytes *)
	| NIntPointer (* 0x1F *)
		(* signed pointer-size integer *)
	| NUIntPointer (* 0x20 *)
		(* unsigned pointer-sized integer *)
	| NNestedStruct (* 0x21 *)
		(* obsolete *)
	| NByValStr (* 0x22 *)
		(* VB-style string in a fixed-length buffer *)
	| NAnsiBStr (* 0x23 *)
		(* ansi bstr - ANSI VB-style string *)
	| NTBStr (* 0x24 *)
		(* tbstr - bstr or ansi bstr, depending on the platform *)
	| NVariantBool (* 0x25 *)
		(* variant bool - 2-byte Boolean: true = -1; false = 0 *)
	| NFunctionPtr (* 0x26 *)
	| NAsAny (* 0x28 *)
		(* as any - object: type defined at run time (?) *)
	| NArray of int option * nativesig (* 0x2A *)
		(* fixed-size array of a native type *)
		(* if size is empty, the size of the native array is derived from the size  *)
		(* of the managed type being marshaled *)
	| NLPStruct (* 0x2B *)
		(* pointer to a c-style structure *)
	| NCustomMarshaler of string * string (* 0x2C *)
		(* custom (<class_str>, <cookie_str>) *)
	| NError (* 0x2D *)
		(* maps in32 to VT_HRESULT *)

and variantsig =
	| VT_EMPTY (* 0x00 *)
		(* No <empty> *)
	| VT_NULL (* 0x01 *)
		(* No null *)
	| VT_I2 (* 0x02 *)
		(* Yes int16 *)
	| VT_I4 (* 0x03 *)
		(* Yes int32 *)
	| VT_R4 (* 0x04 *)
		(* Yes float32 *)
	| VT_R8 (* 0x05 *)
		(* Yes float64 *)
	| VT_CY (* 0x06 *)
		(* Yes currency *)
	| VT_DATE (* 0x07 *)
		(* Yes date *)
	| VT_BSTR (* 0x08 *)
		(* Yes bstr *)
	| VT_DISPATCH (* 0x09 *)
		(* Yes idispatch *)
	| VT_ERROR (* 0x0A *)
		(* Yes error *)
	| VT_BOOL (* 0x0B *)
		(* Yes bool *)
	| VT_VARIANT (* 0x0C *)
		(* Yes variant *)
	| VT_UNKNOWN (* 0x0D *)
		(* Yes iunknown *)
	| VT_DECIMAL (* 0x0E *)
		(* Yes decimal *)
	| VT_I1 (* 0x10 *)
		(* Yes int8 *)
	| VT_UI1 (* 0x11 *)
		(* Yes unsigned int8, uint8 *)
	| VT_UI2 (* 0x12 *)
		(* Yes unsigned int16, uint16 *)
	| VT_UI4 (* 0x13 *)
		(* Yes unsigned int32, uint32 *)
	| VT_I8 (* 0x14 *)
		(* No int64 *)
	| VT_UI8 (* 0x15 *)
		(* No unsigned int64, uint64 *)
	| VT_INT (* 0x16 *)
		(* Yes int *)
	| VT_UINT (* 0x17 *)
		(* Yes unsigned int, uint *)
	| VT_VOID (* 0x18 *)
		(* No void *)
	| VT_HRESULT (* 0x19 *)
		(* No hresult *)
	| VT_PTR (* 0x1A *)
		(* No * *)
	| VT_SAFEARRAY (* 0x1B *)
		(* No safearray *)
	| VT_CARRAY (* 0x1C *)
		(* No carray *)
	| VT_USERDEFINED (* 0x1D *)
		(* No userdefined *)
	| VT_LPSTR (* 0x1E *)
		(* No lpstr *)
	| VT_LPWSTR (* 0x1F *)
		(* No lpwstr *)
	| VT_RECORD (* 0x24 *)
		(* Yes record *)
	| VT_FILETIME (* 0x40 *)
		(* No filetime *)
	| VT_BLOB (* 0x41 *)
		(* No blob *)
	| VT_STREAM (* 0x42 *)
		(* No stream *)
	| VT_STORAGE (* 0x43 *)
		(* No storage *)
	| VT_STREAMED_OBJECT (* 0x44 *)
		(* No streamed_object *)
	| VT_STORED_OBJECT (* 0x45 *)
		(* No stored_object *)
	| VT_BLOB_OBJECT (* 0x46 *)
		(* No blob_object *)
	| VT_CF (* 0x47 *)
		(* No cf *)
	| VT_CLSID (* 0x48 *)
		(* No clsid *)
	| VT_VECTOR of variantsig (* 0x1000 *)
		(* Yes <v_type> vector *)
	| VT_ARRAY of variantsig (* 0x2000 *)
		(* Yes <v_type> [ ] *)
	| VT_BYREF of variantsig (* 0x4000 *)
		(* Yes <v_type> & *)
