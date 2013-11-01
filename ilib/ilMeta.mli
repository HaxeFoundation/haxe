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

(* useful types for describing CLI metadata *)
type guid = string
	(* reference from the #GUID stream *)
type stringref = string
	(* reference from the #Strings stream *)
type blobref = string
	(* reference from the #Blob stream *)
type id = stringref
	(* a stringref that references an identifier. *)
	(* must begin with an alphabetic character, or the following characters: *)
		(* #, $, @, _ *)
	(* and continue with alphanumeric characters or one of the following: *)
		(* ?, $, @, _, ` *)

type ns = id list

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
	| MethodPtr of meta_method_ptr
		(* a class-to-methods lookup table - does not exist in optimized metadatas *)
	| Method of meta_method
		(* method definition descriptors *)
	| ParamPtr of meta_param_ptr
		(* a method-to-parameters lookup table - does not exist in optimized metadatas *)
	| Param of meta_param
		(* parameter definition descriptors *)
	| InterfaceImpl of meta_interface_impl
		(* interface implementation descriptors *)
	| MemberRef of meta_member_ref
		(* member (field or method) reference descriptors *)
	| Constant of meta_constant
		(* constant value that map the default values stored in the #Blob stream to *)
		(* respective fields, parameters and properties *)
	| CustomAttribute of meta_custom_attribute
		(* custom attribute descriptors *)
	| FieldMarshal of meta_field_marshal
		(* field or parameter marshaling descriptors for managed/unmanaged interop *)
	| DeclSecurity of meta_decl_security
		(* security descriptors *)
	| ClassLayout of meta_class_layout	
		(* class layout descriptors that hold information about how the loader should lay out respective classes *)
	| FieldLayout of meta_field_layout
		(* field layout descriptors that specify the offset or oridnal of individual fields *)
	| StandAloneSig of meta_stand_alone_sig
		(* stand-alone signature descriptors. used in two capacities: *)
		(* as composite signatures of local variables of methods *)
		(* and as parameters of the call indirect (calli) IL instruction *)
	| EventMap of meta_event_map
		(* a class-to-events mapping table. exists also in optimized metadatas *)
	| EventPtr of meta_event_ptr
		(* an event map-to-events lookup table - does not exist in optimized metadata *)
	| Event of meta_event
		(* event descriptors *)
	| PropertyMap of meta_property_map
		(* a class-to-properties mapping table. exists also in optimized metadatas *)
	| PropertyPtr of meta_property_ptr
		(* a property map-to-properties lookup table - does not exist in optimized metadata *)
	| Property of meta_property
		(* property descriptors *)
	| MethodSemantics of meta_method_semantics
		(* method semantics descriptors that hold information about which method is associated *)
		(* with a specific property or event and in what capacity *)
	| MethodImpl of meta_method_impl
		(* method implementation descriptors *)
	| ModuleRef of meta_module_ref
		(* module reference descriptors *)
	| TypeSpec of meta_type_spec
		(* Type specification descriptors *)
	| ImplMap of meta_impl_map
		(* implementation map descriptors used for platform invocation (P/Invoke) *)
	| FieldRVA of meta_field_rva
		(* field-to-data mapping descriptors *)
	| ENCLog of meta_enc_log
		(* edit-and-continue log descriptors that hold information about what changes *)
		(* have been made to specific metadata items during in-memory editing *)
		(* this table does not exist on optimized metadata *)
	| ENCMap of meta_enc_map
		(* edit-and-continue mapping descriptors. does not exist on optimized metadata *)
	| Assembly of meta_assembly
		(* the current assembly descriptor, which should appear only in the prime module metadata *)
	| AssemblyProcessor of meta_assembly_processor | AssemblyOS of meta_assembly_os
		(* unused *)
	| AssemblyRef of meta_assembly_ref
		(* assembly reference descriptors *)
	| AssemblyRefProcessor of meta_assembly_ref_processor | AssemblyRefOS of meta_assembly_ref_os
		(* unused *)
	| File of meta_file
		(* file descriptors that contain information about other files in the current assembly *)
	| ExportedType of meta_exported_type
		(* exported type descriptors that contain information about public classes *)
		(* exported by the current assembly, which are declared in other modules of the assembly *)
		(* only the prime module of the assembly should carry this table *)
	| ManifestResource of meta_manifest_resource
		(* managed resource descriptors *)
	| NestedClass of meta_nested_class
		(* nested class descriptors that provide mapping of nested classes to their respective enclosing classes *)
	| GenericParam of meta_generic_param
		(* type parameter descriptors for generic classes and methods *)
	| MethodSpec of meta_method_spec
		(* generic method instantiation descriptors *)
	| GenericParamConstraint of meta_generic_param_constraint
		(* descriptors of constraints specified for type parameters of generic classes and methods *)
	| UnknownMeta of int

(* all fields here need to be mutable, as they will first be initialized empty *)

and meta_root = {
	root_id : int;
}

and meta_root_ptr = {
	ptr_id : int;
	ptr_to : meta_root;
}

and meta_module = {
	mutable md_id : int;
	mutable md_generation : int;
	mutable md_name : id;
	mutable md_vid : guid;
	mutable md_encid : guid;
	mutable md_encbase_id : guid;
}

and meta_type_ref = {
	mutable tr_id : int;
	mutable tr_resolution_scope : resolution_scope;
	mutable tr_name : id;
	mutable tr_namespace : ns;
}

and meta_type_def = {
	mutable td_id : int;
	mutable td_flags : type_def_flags;
	mutable td_name : id;
	mutable td_namespace : ns;
	mutable td_extends : type_def_or_ref option;
	mutable td_field_list : meta_field list;
	mutable td_method_list : meta_method list;

	(* extra field *)
	mutable td_extra_enclosing : meta_type_def option;
}

and meta_field_ptr = {
	mutable fp_id : int;
	mutable fp_field : meta_field;
}

and meta_field = {
	mutable f_id : int;
	mutable f_flags : field_flags;
	mutable f_name : id;
	mutable f_signature : ilsig;
}

and meta_method_ptr = {
	mutable mp_id : int;
	mutable mp_method : meta_method;
}

and meta_method = {
	mutable m_id : int;
	mutable m_rva : rva;
	mutable m_flags : method_flags;
	mutable m_name : id;
	mutable m_signature : ilsig;
	mutable m_param_list : meta_param list; (* rid: Param *)

	(* extra field *)
	mutable m_declaring : meta_type_def option;
}

and meta_param_ptr = {
	mutable pp_id : int;
	mutable pp_param : meta_param;
}

and meta_param = {
	mutable p_id : int;
	mutable p_flags : param_flags;
	mutable p_sequence : int;
		(* 0 means return value *)
	mutable p_name : id;
}

and meta_interface_impl = {
	mutable ii_id : int;
	mutable ii_class : meta_type_def; (* TypeDef rid *)
	mutable ii_interface : type_def_or_ref;
}

and meta_member_ref = {
	mutable memr_id : int;
	mutable memr_class : member_ref_parent;
	mutable memr_name : id;
	mutable memr_signature : ilsig;
}

and meta_constant = {
	mutable c_id : int;
	mutable c_type : constant_type;
	mutable c_parent : has_const;
	mutable c_value : constant;
}

and named_attribute = bool * string * instance (* is_property * name * instance *)

and meta_custom_attribute = {
	mutable ca_id : int;
	mutable ca_parent : has_custom_attribute;
	mutable ca_type : custom_attribute_type;
	mutable ca_value : (instance list * named_attribute list) option;
		(* can be 0 *)
}

and meta_field_marshal = {
	mutable fm_id : int;
	mutable fm_parent : has_field_marshal;
	mutable fm_native_type : nativesig;
}

and meta_decl_security = {
	mutable ds_id : int;
	mutable ds_action : action_security;
	mutable ds_parent : has_decl_security;
	mutable ds_permission_set : blobref;
		(* an xml with the permission set *)
}

and meta_class_layout = {
	mutable cl_id : int;
	mutable cl_packing_size : int;
		(* power of two; from 1 through 128 *)
	mutable cl_class_size : int;
	mutable cl_parent : meta_type_def; (* TypeDef rid *)
}

and meta_field_layout = {
	mutable fl_id : int;
	mutable fl_offset : int;
		(* offset in bytes or ordinal *)
	mutable fl_field : meta_field; (* Field rid *)
}

and meta_stand_alone_sig = {
	mutable sa_id : int;
	mutable sa_signature : ilsig;
}

and meta_event_map = {
	mutable em_id : int;
	mutable em_parent : meta_type_def; (* TypeDef rid *)
	mutable em_event_list : meta_event list; (* Event rid *)
}

and meta_event_ptr = {
	mutable ep_id : int;
	mutable ep_event : meta_event; (* Event rid *)
}

and meta_event = {
	mutable e_id : int;
	mutable e_flags : event_flags;
	mutable e_name : stringref;
	mutable e_event_type : type_def_or_ref;
}

and meta_property_map = {
	mutable pm_id : int;
	mutable pm_parent : meta_type_def; (* TypeDef rid *)
	mutable pm_property_list : meta_property list; (* Property rid *)
}

and meta_property_ptr = {
	mutable prp_id : int;
	mutable prp_property : meta_property; (* Property rid *)
}

and meta_property = {
	mutable prop_id : int;
	mutable prop_flags : property_flags;
	mutable prop_name : stringref;
	mutable prop_type : ilsig;
}

and meta_method_semantics = {
	mutable ms_id : int;
	mutable ms_semantic : semantic_flags;
	mutable ms_method : meta_method; (* Method rid *)
	mutable ms_association : has_semantics;
}

and meta_method_impl = {
	mutable mi_id : int;
	mutable mi_class : meta_type_def; (* TypeDef rid *)
	mutable mi_method_body : method_def_or_ref;
		(* overriding method *)
	mutable mi_method_declaration : method_def_or_ref;
		(* overriden method *)
}

and meta_module_ref = {
	mutable modr_id : int;
	mutable modr_name : stringref;
}

and meta_type_spec = {
	mutable ts_id : int;
	mutable ts_signature : ilsig;
}

(* reserved ? *)
and meta_enc_log = {
	mutable el_id : int;
	mutable el_token : to_det;
	mutable el_func_code : to_det;
}

and meta_impl_map = {
	mutable im_id : int;
	mutable im_flags : impl_flags; (* mapping_flags *)
	mutable im_forwarded : member_forwarded; (* method only *)
	mutable im_import_name : stringref;
	mutable im_import_scope : meta_module_ref; (* ModuleRef rid *)
}

(* reserved ? *)
and meta_enc_map = {
	mutable encm_id : int;
	mutable encm_token : to_det;
}

and meta_field_rva = {
	mutable fr_id : int;
	mutable fr_rva : rva;
	mutable fr_field : meta_field; (* Field rid *)
}

and meta_assembly = {
	mutable a_id : int;
	mutable a_hash_algo : hash_algo;
	mutable a_major : int;
	mutable a_minor : int;
	mutable a_build : int;
	mutable a_rev : int;
	mutable a_flags : assembly_flags; (* assembly_flags *)
	mutable a_public_key : blobref;
	mutable a_name : stringref;
	mutable a_locale : stringref;
}

(* unused *)
and meta_assembly_processor = {
	mutable ap_id : int;
	mutable ap_processor : to_det;
}

(* unused *)
and meta_assembly_os = {
	mutable aos_id : int;
	mutable aos_platform_id : to_det;
	mutable aos_major_version : to_det;
	mutable aos_minor_version : to_det;
}

and meta_assembly_ref = {
	mutable ar_id : int;
	mutable ar_major : int;
	mutable ar_minor : int;
	mutable ar_build : int;
	mutable ar_rev : int;
	mutable ar_flags : assembly_flags;
	mutable ar_public_key : blobref;
	mutable ar_name : stringref; (* no path, no extension *)
	mutable ar_locale : stringref;
	mutable ar_hash_value : blobref;
}

(* unused *)
and meta_assembly_ref_processor = {
	mutable arp_id : int;
	mutable arp_processor : to_det;
	mutable arp_assembly_ref : meta_assembly_ref; (* AssemblyRef rid *)
}

(* unused *)
and meta_assembly_ref_os = {
	mutable aros_id : int;
	mutable aros_platform_id : to_det;
	mutable aros_major : int;
	mutable aros_minor : int;
	mutable aros_assembly_ref : meta_assembly_ref; (* AssemblyRef rid *)
}

and meta_file = {
	mutable file_id : int;
	mutable file_flags : file_flag; (* file_flags *)
	mutable file_name : stringref; (* no path; only file name *)
	mutable file_hash_value : blobref;
}

and meta_exported_type = {
	mutable et_id : int;
	mutable et_flags : type_def_flags;
	mutable et_type_def_id : int;
		(* TypeDef token in another module *)
	mutable et_type_name : stringref;
	mutable et_type_namespace : ns;
	mutable et_implementation : implementation;
}

and meta_manifest_resource = {
	mutable mr_id : int;
	mutable mr_offset : int;
	mutable mr_flags : manifest_resource_flag; (* manifest_resource_flags *)
	mutable mr_name : stringref;
	mutable mr_implementation : implementation option;
}

and meta_nested_class = {
	mutable nc_id : int;
	mutable nc_nested : meta_type_def; (* TypeDef rid *)
	mutable nc_enclosing : meta_type_def; (* TypeDef rid *)
}

and meta_generic_param = {
	mutable gp_id : int;
	mutable gp_number : int; (* ordinal *)
	mutable gp_flags : generic_flags;
	mutable gp_owner : type_or_method_def;
		(* generic type or method *)
	mutable gp_name : stringref option;
}

and meta_method_spec = {
	mutable mspec_id : int;
	mutable mspec_method : method_def_or_ref;
		(* instantiated method *)
	mutable mspec_instantiation : ilsig;
		(* instantiated signature *)
}

and meta_generic_param_constraint = {
	mutable gc_id : int;
	mutable gc_owner : meta_generic_param; (* GenericParam rid *)
		(* constrained parameter *)
	mutable gc_constraint : type_def_or_ref;
		(* type the parameter must extend or implement *)
}

and to_det = int

and not_implemented = int

and constant =
	| IBool of bool
	| IChar of int
	| IByte of int
	| IShort of int
	| IInt of int32
	| IInt64 of int64
	| IFloat32 of float
	| IFloat64 of float
	| IString of string
	| INull

and instance =
	| InstConstant of constant
	| InstBoxed of instance
	| InstType of string
	| InstArray of instance list
	| InstEnum of int

and constant_type =
	| CBool (* 0x2 *)
	| CChar (* 0x3 *)
	| CInt8 (* 0x4 *)
	| CUInt8 (* 0x5 *)
	| CInt16 (* 0x6 *)
	| CUInt16 (* 0x7 *)
	| CInt32 (* 0x8 *)
	| CUInt32 (* 0x9 *)
	| CInt64 (* 0xA *)
	| CUInt64 (* 0xB *)
	| CFloat32 (* 0xC *)
	| CFloat64 (* 0xD *)
	| CString (* 0xE *)
	| CNullRef (* 0x12 *)
		(* null object reference - the value of the constant *)
		(* of this type must be a 4-byte integer containing 0 *)

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

and method_contract =
	(* contract flags - mask 0xF0 *)
	| CMStatic (* 0x10 *)
	| CMFinal (* 0x20 *)
		(* must be paired with the virtual flag - otherwise it is meaningless *)
	| CMVirtual (* 0x40 *)
	| CMHideBySig (* 0x80 *)
		(* the method hides all methods of the parent classes that have a matching *)
		(* signature and name (as opposed to having a matching name only). ignored by the CLR *)

and method_vtable =
	(* vtable flags - mask 0x300 *)
	| VNewSlot (* 0x100 *)
		(* a new vtable slot is created, so it doesn't override the old implementation *)
	| VStrict (* 0x200 *)
		(* virtual method can be overriden only if it is accessible from the overriding class *)

and method_impl =
	(* implementation flags - mask 0x2C08 *)
	| IAbstract (* 0x0400 *)
	| ISpecialName (* 0x0800 *)
	| IPInvokeImpl (* 0x2000 *)
		(* the method has an unmanaged implementation and is called through the platform *)
		(* invocation mechanism. the rva field must be 0, since the method is implemented externally *)
	| IUnmanagedExp (* 0x0008 *)
		(* the managed method is exposed as an unmanaged export. not used by the CLR currently *)

and method_reserved =
	(* reserved flags - cannot be set explicitly. mask 0xD000 *)
	| RTSpecialName (* 0x1000 *)
		(* has a special name: .ctor, .cctor, _VtblGap* and _Deleted* *)
	| RHasSecurity (* 0x4000 *)
		(* either has an associated DeclSecurity metadata or the custom attribte *)
		(* System.Security.SuppressUnmanagedCodeSecurityAttribute *)
	| RReqSecObj (* 0x8000 *)
		(* this method calls another method containing security code, so it requires *)
		(* an additional stack slot for a security object. *)

and method_code_type =
	(* code type - mask 0x3 *)
	| CCil (* 0x0 *)
	| CNative (* 0x1 *)
		(* implemented in native platform-specific code *)
	| COptIl (* 0x2 *)
		(* optimized il - not supported; must not be set *)
	| CRuntime (* 0x3 *)
		(* automatically generated by the runtime itself (intrinsic) *)

and method_code_mngmt =
	(* code management - mask 0x4 *)
	| MManaged (* 0x0 *)
	| MUnmanaged (* 0x4 *)
		(* must be paired with the native flag *)

and method_interop =
	(* method implementation and interop - mask 0x10D8 *)
	| OForwardRef (* 0x10 *)
		(* managed object fiels and edit-and-continue scenarios only *)
	| OPreserveSig (* 0x80 *)
		(* method signature must not be mangled during interop with classic COM code *)
	| OInternalCall (* 0x1000 *)
		(* reserved for internal use. if set, RVA must be 0 *)
	| OSynchronized (* 0x20 *)
		(* automatically insert code to take a lock on entry to the method and release it *)
		(* on exit from the method. Value types cannot have this flag set *)
	| ONoInlining (* 0x08 *)
		(* the runtime is not allowed to inline the method *)

and method_flags = {
	mf_access : field_access;
	mf_contract : method_contract list;
	mf_vtable : method_vtable list;
	mf_impl : method_impl list;
	mf_reserved : method_reserved list;
	mf_code_type : method_code_type;
	mf_code_mngmt : method_code_mngmt;
	mf_interop : method_interop list;
}

and param_io =
	(* input/output flags - mask 0x13 *)
	| PIn (* 0x1 *)
	| POut (* 0x2 *)
	| POpt (* 0x10 *)

and param_reserved =
	(* reserved flags - mask 0xF000 *)
	| PHasConstant (* 0x1000 *)
		(* the parameter has an associated Constant record *)
	| PMarshal (* 0x2000 *)
		(* the parameter has an associated FieldMarshal record specifying how the parameter *)
		(* must be marshaled when consumed by unmanaged code *)

and param_flags = {
	pf_io : param_io list;
	pf_reserved : param_reserved list;
}

and event_flag =
	| ESpecialName (* 0x0200 *)
		(* event is special *)
	| ERTSpecialName (* 0x0400 *)
		(* CLI provides special behavior, depending on the name of the event *)

and event_flags = event_flag list

and property_flag =
	| PSpecialName (* 0x0200 *)
		(* property is special *)
	| PRTSpecialName (* 0x0400 *)
		(* runtime (intrinsic) should check name encoding *)
	| PHasDefault (* 0x1000 *)
		(* property has default *)
	| PUnused (* 0xE9FF *)
		(* reserved *)

and property_flags = property_flag list

and semantic_flag =
	| SSetter (* 0x0001 *)
		(* setter for property *)
	| SGetter (* 0x0002 *)
		(* getter for property *)
	| SOther (* 0x0004 *)
		(* other method for property or event *)
	| SAddOn (* 0x0008 *)
		(* addon method for event - refers to the required add_ method for events *)
	| SRemoveOn (* 0x0010 *)
		(* removeon method for event - refers to the required remove_ method for events *)
	| SFire (* 0x0020 *)
		(* fire method for event. this refers to the optional raise_ method for events *)

and semantic_flags = semantic_flag list

and action_security =
	| SecNull
	| SecRequest (* 0x1 *)
	| SecDemand (* 0x2 *)
	| SecAssert (* 0x3 *)
	| SecDeny (* 0x4 *)
	| SecPermitOnly (* 0x5 *)
	| SecLinkCheck (* 0x6 *)
	| SecInheritCheck (* 0x7 *)
	| SecReqMin (* 0x8 *)
	| SecReqOpt (* 0x9 *)
	| SecReqRefuse (* 0xA *)
	| SecPreJitGrant (* 0xB *)
	| SecPreJitDeny (* 0xC *)
	| SecNonCasDemand (* 0xD *)
	| SecNonCasLinkDemand (* 0xE *)
	| SecNonCasInheritance (* 0xF *)

and impl_charset =
	| IDefault (* 0x0 *)
	| IAnsi (* 0x2 *)
		(* method parameters of type string must be marshaled as ANSI zero-terminated *)
		(* strings unless explicitly specified otherwise *)
	| IUnicode (* 0x4 *)
		(* method parameters of type string must be marshaled as Unicode strings *)
	| IAutoChar (* 0x6 *)
		(* method parameters of type string must be marshaled as ANSI or Unicode strings *)
		(* depending on the platform *)

and impl_callconv =
	| IDefaultCall (* 0x0 *)
	| IWinApi (* 0x100 *)
		(* the native method uses the calling convention standard for the underlying platform *)
	| ICDecl (* 0x200 *)
		(* the native method uses the C/C++ style calling convention *)
	| IStdCall (* 0x300 *)
		(* native method uses the standard Win32 API calling convention *)
	| IThisCall (* 0x400 *)
		(* native method uses the C++ member method (non-vararg) calling convention *)
	| IFastCall (* 0x500 *)

and impl_flag =
	| INoMangle (* 0x1 *)
		(* exported method's name must be matched literally *)
	| IBestFit (* 0x10 *)
		(* allow "best fit" guessing when converting the strings *)
	| IBestFitOff (* 0x20 *)
		(* disallow "best fit" guessing *)
	| ILastErr (* 0x40 *)
		(* the native method supports the last error querying by the Win32 API GetLastError *)
	| ICharMapError (* 0x1000 *)
		(* throw an exception when an unmappable character is encountered in a string *)
	| ICharMapErrorOff (* 0x2000 *)
		(* don't throw an exception when an unmappable character is encountered *)
	
and impl_flags = {
	if_charset : impl_charset;
	if_callconv : impl_callconv;
	if_flags : impl_flag list;
}

and hash_algo =
	| HNone (* 0x0 *)
	| HReserved (* 0x8003 *)
		(* MD5 ? *)
	| HSha1 (* 0x8004 *)
		(* SHA1 *)

and assembly_flag =
	| APublicKey (* 0x1 *)
		(* assembly reference holds the full (unhashed) public key *)
	| ARetargetable (* 0x100 *)
		(* implementation of this assembly used at runtime is not expected to match *)
		(* the version seen at compile-time *)
	| ADisableJitCompileOptimizer (* 0x4000 *)
		(* Reserved *)
	| AEnableJitCompileTracking (* 0x8000 *)
		(* Reserved *)

and assembly_flags = assembly_flag list

and file_flag =
	| ContainsMetadata (* 0x0 *)
	| ContainsNoMetadata (* 0x1 *)

and manifest_resource_flag =
	(* mask 0x7 *)
	| RNone (* 0x0 *)
	| RPublic (* 0x1 *)
	| RPrivate (* 0x2 *)

and generic_variance =
	(* mask 0x3 *)
	| VNone (* 0x0 *)
	| VCovariant (* 0x1 *)
	| VContravariant (* 0x2 *)

and generic_constraint =
	(* mask 0x1C *)
	| CInstanceType (* 0x4 *)
		(* generic parameter has the special class constraint *)
	| CValueType (* 0x8 *)
		(* generic parameter has the special valuetype constraint *)
	| CDefaultCtor (* 0x10 *)
		(* has the special .ctor constraint *)

and generic_flags = {
	gf_variance : generic_variance;
	gf_constraint : generic_constraint list;
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
	| SArray of ilsig * (int option * int option) array (* 0x14 *)
		(* ilsig * ( bound * size ) *)
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
	| SFunPtr of callconv list * ilsig * (ilsig list) (* 0x1B *)
		(* a pointer to a function, followed by full method signature *)
	| SObject (* 0x1C *)
		(* System.Object *)
	| SVector of ilsig (* 0x1D *)
		(* followed by the encoding of the underlying type *)
	| SMethodTypeParam of int (* 0x1E *)
		(* generic parameter in a generic method definition *)
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
	(* special undocumented (yay) *)
	| SType (* 0x50 *)
	| SBoxed (* 0x51 *)
	| SEnum of string (* 0x55 *)

and callconv =
	| CallDefault (* 0x0 *)
	| CallVararg (* 0x5 *)
	| CallField (* 0x6 *)
		(* field call *)
	| CallLocal (* 0x7 *)
		(* local variable call *)
	| CallProp (* 0x8 *)
		(* property call *)
	| CallUnmanaged (* 0x9 *)
		(* unmanaged calling convention. not used *)
	| CallGenericInst (* 0xA *)
		(* generic instantiation - MethodSpec *)
	| CallGeneric of int (* 0x10 *)
		(* also contains the number of generic arguments *)
	| CallHasThis (* 0x20 *)
		(* instance method that has an instance pointer (this) *)
		(* as an implicit first argument - ilasm 'instance' *)
	| CallExplicitThis (* 0x40 *)
		(* the first explicitly specified parameter is the instance pointer *)
		(* ilasm 'explicit' *)
	
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
	| NFixedArray of int * variantsig (* 0x1E *)
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
	| NArray of nativesig * int * int * int (* 0x2A *)
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
	(* | VT_VECTOR of variantsig (* 0x1000 *) *)
	(* 	(* Yes <v_type> vector *) *)
	(* | VT_ARRAY of variantsig (* 0x2000 *) *)
	(* 	(* Yes <v_type> [ ] *) *)
	(* | VT_BYREF of variantsig (* 0x4000 *) *)
	(* 	(* Yes <v_type> & *) *)
