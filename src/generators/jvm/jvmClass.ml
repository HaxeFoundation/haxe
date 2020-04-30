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

open JvmGlobals
open JvmData
open JvmSignature
open NativeSignatures
open JvmAttribute
open JvmBuilder

(* High-level class builder. *)

class builder path_this path_super = object(self)
	inherit base_builder
	val pool = new JvmConstantPool.constant_pool
	val jsig = TObject(path_this,[])
	val mutable offset_this = 0
	val mutable offset_super = 0
	val mutable type_parameters = []
	val mutable super_type_parameters = []
	val mutable interfaces = []
	val mutable interface_offsets = []
	val fields = DynArray.create ()
	val methods = DynArray.create ()
	val method_sigs = Hashtbl.create 0
	val inner_classes = Hashtbl.create 0
	val mutable spawned_methods = []
	val mutable static_init_method = None
	val mutable source_file = None

	method add_interface (path : jpath) (params : jtype_argument list) =
		interface_offsets <- (pool#add_path path) :: interface_offsets;
		interfaces <- (path,params) :: interfaces

	method set_type_parameters (sl : string list) =
		type_parameters <- sl

	method set_super_parameters (params : jtype_argument list) =
		super_type_parameters <- params

	method add_field (f : jvm_field) =
		DynArray.add fields f

	method get_pool = pool

	method get_this_path = path_this
	method get_super_path = path_super
	method get_jsig = jsig
	method get_offset_this = offset_this
	method get_access_flags = access_flags

	method set_source_file (file : string) =
		source_file <- Some file

	method get_static_init_method = match static_init_method with
		| Some jm -> jm
		| None ->
			let jm = self#spawn_method "<clinit>" (method_sig [] None) [MethodAccessFlags.MStatic] in
			static_init_method <- Some jm;
			jm

	method has_method (name : string) (jsig : jsignature) =
		Hashtbl.mem method_sigs (name,generate_method_signature false jsig)

	method spawn_inner_class (jm : JvmMethod.builder option) (path_super : jpath) (name : string option) =
		let path = match name with
			| None -> (fst path_this,Printf.sprintf "%s$%i" (snd path_this) (Hashtbl.length inner_classes))
			| Some name -> (fst path_this,Printf.sprintf "%s$%s" (snd path_this) name)
		in
		let jc = new builder path path_super in
		jc#add_access_flag 0x01;
		begin match jm with
		| None ->
			()
		| Some jm ->
			let pool = jc#get_pool in
			let offset_class = pool#add_path path_this in
			let offset_name = pool#add_string jm#get_name in
			let offset_desc = pool#add_string (generate_signature false jm#get_jsig) in
			let offset_info = pool#add (ConstNameAndType(offset_name,offset_desc)) in
			jc#add_attribute (JvmAttribute.AttributeEnclosingMethod(offset_class,offset_info));
		end;
		let offset = pool#add_path path in
		Hashtbl.add inner_classes offset jc;
		begin match source_file with
		| None ->
			()
		| Some file ->
			jc#set_source_file file
		end;
		jc

	method spawn_method (name : string) (jsig_method : jsignature) (flags : MethodAccessFlags.t list) =
		let jm = new JvmMethod.builder self name jsig_method in
		let ssig_method = generate_method_signature false jsig_method in
		if Hashtbl.mem method_sigs (name,ssig_method) then
			jerror (Printf.sprintf "Duplicate field on class %s: %s %s" (Globals.s_type_path path_this) name ssig_method);
		Hashtbl.add method_sigs (name,ssig_method) jm;
		List.iter (fun flag ->
			jm#add_access_flag (MethodAccessFlags.to_int flag)
		) flags;
		let pop_scope = jm#push_scope in
		if not (jm#has_method_flag MStatic) then ignore(jm#add_local "this" (if jm#get_name = "<init>" then (TUninitialized None) else jsig) VarArgument);
		spawned_methods <- (jm,Some pop_scope) :: spawned_methods;
		jm

	method spawn_field (name : string) (jsig_method : jsignature) (flags : FieldAccessFlags.t list) =
		let jm = new JvmMethod.builder self name jsig_method in
		List.iter (fun flag ->
			jm#add_access_flag (FieldAccessFlags.to_int flag)
		) flags;
		spawned_methods <- (jm,None) :: spawned_methods;
		jm

	method private commit_inner_classes =
		if Hashtbl.length pool#get_inner_classes > 0 then begin
			let open JvmAttribute in
			let l = Hashtbl.fold (fun (path,name) offset_class acc ->
				(path,name,offset_class) :: acc
			) pool#get_inner_classes [] in
			let l = List.map (fun (path,name,offset_class) ->
				let offset_name = pool#add_string name in
				let flags = try (Hashtbl.find inner_classes offset_class)#get_access_flags with Not_found -> 9 in
				let offset_outer = pool#add_path path in
				{
					ic_inner_class_info_index = offset_class;
					ic_outer_class_info_index = offset_outer;
					ic_inner_name_index = offset_name;
					ic_inner_class_access_flags = flags;
				}
			) l in
			let a = Array.of_list l in
			self#add_attribute (AttributeInnerClasses a)
		end

	method private generate_signature =
		let stl = match type_parameters with
			| [] -> ""
			| params ->
				let stl = String.concat "" (List.map (fun n ->
					Printf.sprintf "%s:Ljava/lang/Object;" n
				) params) in
				Printf.sprintf "<%s>" stl
		in
		let ssuper = generate_method_signature true (TObject(path_super,super_type_parameters)) in
		let sinterfaces = String.concat "" (List.map (fun (path,params) ->
			generate_method_signature true (TObject(path,params))
		) interfaces) in
		let s = Printf.sprintf "%s%s%s" stl ssuper sinterfaces in
		let offset = self#get_pool#add_string s in
		self#add_attribute (AttributeSignature offset)

	method export_class (config : export_config) =
		assert (not was_exported);
		begin match source_file with
		| None ->
			()
		| Some file ->
			self#add_attribute (AttributeSourceFile (self#get_pool#add_string file));
		end;
		begin match static_init_method with
		| None ->
			()
		| Some jm ->
			if not jm#is_terminated then jm#return;
		end;
		self#generate_signature;
		was_exported <- true;
		List.iter (fun (jm,pop_scope) ->
			begin match pop_scope with
			| Some pop_scope ->
				pop_scope();
				DynArray.add methods (jm#export_method config);
			| None ->
				self#add_field jm#export_field
			end;
		) (List.rev spawned_methods);
		self#commit_inner_classes;
		self#commit_annotations pool;
		let attributes = self#export_attributes pool in
		let pool = pool#close in
		{
			class_minor_version = 0;
			class_major_version = 0x34;
			class_constant_pool = pool;
			class_access_flags = access_flags;
			class_this_class = offset_this;
			class_super_class = offset_super;
			class_interfaces = Array.of_list interface_offsets;
			class_fields = DynArray.to_array fields;
			class_methods = DynArray.to_array methods;
			class_attributes = attributes;
		}

	initializer
		offset_this <- pool#add_path path_this;
		offset_super <- pool#add_path path_super;
end