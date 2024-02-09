open Globals
open Ast
open TType
open TFunctions
open TUnification

type method_type =
	| MStatic
	| MInstance
	| MConstructor

let is_extern_abstract a = match a.a_impl with
	| Some c -> has_class_flag c CExtern
	| _ -> match a.a_path with
		| ([],("Void" | "Float" | "Int" | "Single" | "Bool" | "Null")) -> true
		| _ -> false

type field_generation_info = {
	mutable has_this_before_super : bool;
	(* This is an ordered list of fields that are targets of super() calls which is determined during
	   pre-processing. The generator can pop from this list assuming that it processes the expression
	   in the same order (which it should). *)
	mutable super_call_fields : (tclass * tclass_field) list;
}

module Info = struct
	type 'a tclass_info = {
		mutable typedef_implements : tclass list option;
		mutable implicit_ctors : ((path * 'a),(tclass * tclass_field)) PMap.t;
	}

	class ['a] info_context = object(self)
		val class_infos : 'a tclass_info DynArray.t = DynArray.create ()

		method get_class_info (c : tclass) =
			let rec loop ml = match ml with
			| (Meta.Custom ":jvm.classInfo",[(EConst (Int (s, _)),_)],_) :: _ ->
				DynArray.get class_infos (int_of_string s)
			| _ :: ml ->
				loop ml
			| [] ->
				let index = DynArray.length class_infos in
				let infos = {
					typedef_implements = None;
					implicit_ctors = PMap.empty;
				} in
				DynArray.add class_infos infos;
				c.cl_meta <- (Meta.Custom ":jvm.classInfo",[(EConst (Int (string_of_int index, None)),null_pos)],null_pos) :: c.cl_meta;
				infos
			in
			loop c.cl_meta
	end
end

open Info
open OverloadResolution
open Tanon_identification

class ['a] preprocessor (basic : basic_types) (convert : Type.t -> 'a) =
	let make_native cf =
		cf.cf_meta <- (Meta.NativeGen,[],null_pos) :: cf.cf_meta
	in
	let make_haxe cf =
		cf.cf_meta <- (Meta.HxGen,[],null_pos) :: cf.cf_meta
	in
	let rec get_constructor c =
		match c.cl_constructor, c.cl_super with
		| Some cf, _ -> cf
		| None, None -> raise Not_found
		| None, Some (csup,cparams) -> get_constructor csup
	in

object(self)
	val infos = new info_context
	val field_infos : field_generation_info DynArray.t = DynArray.create()

	method get_infos = infos

	method get_implicit_ctor (c : tclass) =
		(infos#get_class_info c).implicit_ctors

	method get_field_info (ml : metadata) =
		let rec loop ml = match ml with
		| (Meta.Custom ":jvm.fieldInfo",[(EConst (Int (s, _)),_)],_) :: _ ->
			Some (DynArray.get field_infos (int_of_string s))
		| _ :: ml ->
			loop ml
		| [] ->
			None
		in
		loop ml

	method add_implicit_ctor (c : tclass) (c' : tclass) (cf : tclass_field) =
		let jsig = convert cf.cf_type in
		let info = infos#get_class_info c in
		info.implicit_ctors <- (PMap.add (c'.cl_path,jsig) (c',cf)) info.implicit_ctors;

	method preprocess_constructor_expr (c : tclass) (cf : tclass_field) (e : texpr) =
		let used_this = ref false in
		let this_before_super = ref false in
		let super_call_fields = DynArray.create () in
		let is_on_current_class cf = PMap.mem cf.cf_name c.cl_fields in
		let find_super_ctor el =
			let csup,map_type = match c.cl_super with
				| Some(c,tl) -> c,apply_params c.cl_params tl
				| _ -> die "" __LOC__
			in
			match resolve_instance_overload true map_type csup "new" el with
			| Some(c,cf,_) ->
				let rec loop csup =
					if c != csup then begin
						match csup.cl_super with
						| Some(c',_) ->
							self#add_implicit_ctor csup c' cf;
							loop c'
						| None -> die "" __LOC__
					end
				in
				loop csup;
				(c,cf)
			| None -> Error.raise_typing_error "Could not find overload constructor" e.epos
		in
		let find_super_ctor el =
			let _,cf = find_super_ctor el in
			(* This is a bit hacky: We always want the direct super class, not the one that actually holds
			   the ctor. It will be implicitly copied to it anyway. *)
			match c.cl_super with
			| None -> die "" __LOC__
			| Some(c,_) -> c,cf
		in
		let rec promote_this_before_super c cf p = match self#get_field_info cf.cf_meta with
			| None ->
				Error.raise_typing_error (Printf.sprintf "Could not determine field information for %s in a this-before-super case, please report this" cf.cf_name) p
			| Some info ->
				if not info.has_this_before_super then begin
					make_haxe cf;
					(* print_endline (Printf.sprintf "promoted this_before_super to %s.new : %s" (s_type_path c.cl_path) (s_type (print_context()) cf.cf_type)); *)
					info.has_this_before_super <- true;
					List.iter (fun (c,cf) -> promote_this_before_super c cf p) info.super_call_fields
				end
		in
		let rec loop e =
			begin match e.eexpr with
			| TBinop(OpAssign,{eexpr = TField({eexpr = TConst TThis},FInstance(_,_,cf))},e2) when is_on_current_class cf->
				(* Assigning this.field = value is fine if field is declared on our current class *)
				loop e2;
			| TConst TThis ->
				used_this := true
			| TCall({eexpr = TConst TSuper},el) ->
				List.iter loop el;
				if !used_this then begin
					this_before_super := true;
					make_haxe cf;
					(* print_endline (Printf.sprintf "inferred this_before_super on %s.new : %s" (s_type_path c.cl_path) (s_type (print_context()) cf.cf_type)); *)
				end;
				let c,cf = find_super_ctor el in
				if !this_before_super then promote_this_before_super c cf e.epos;
				DynArray.add super_call_fields (c,cf);
			| _ ->
				Type.iter loop e
			end;
		in
		loop e;
		{
			has_this_before_super = !this_before_super;
			super_call_fields = DynArray.to_list super_call_fields;
		}

	method preprocess_class (c : tclass) =
		let has_dynamic_instance_method = ref false in
		let has_field_init = ref false in
		let field mtype cf =
			match mtype with
			| MConstructor ->
				()
			| MInstance ->
				begin match cf.cf_kind with
					| Method MethDynamic -> has_dynamic_instance_method := true
					| Var _ when cf.cf_expr <> None && not !has_field_init && c.cl_constructor = None && c.cl_super = None ->
						has_field_init := true;
						self#add_implicit_ctor c c (mk_field "new" (tfun [] basic.tvoid) null_pos null_pos)
					| _ -> ()
				end;
			| MStatic ->
				()
		in
		List.iter (field MStatic) c.cl_ordered_statics;
		List.iter (field MInstance) c.cl_ordered_fields;
		match c.cl_constructor with
		| None ->
			begin try
				let cf = get_constructor c in
				let csup = match c.cl_super with
					| Some(c,_) -> c
					| _ -> die "" __LOC__
				in
				List.iter (fun cf -> self#add_implicit_ctor c csup cf) (cf :: cf.cf_overloads)
			with Not_found ->
				()
			end;
		| Some cf ->
			let field cf =
				if !has_dynamic_instance_method then make_haxe cf;
				begin match cf.cf_expr with
				| None ->
					()
				| Some e ->
					let info = self#preprocess_constructor_expr c cf e in
					let index = DynArray.length field_infos in
					DynArray.add field_infos info;
					cf.cf_meta <- (Meta.Custom ":jvm.fieldInfo",[(EConst (Int (string_of_int index, None)),null_pos)],null_pos) :: cf.cf_meta;
					if not (Meta.has Meta.HxGen cf.cf_meta) then begin
						let loop next c =
							if (has_class_flag c CExtern) then make_native cf
							else match c.cl_constructor with
								| Some cf' when Meta.has Meta.HxGen cf'.cf_meta -> make_haxe cf
								| Some cf' when Meta.has Meta.NativeGen cf'.cf_meta -> make_native cf
								| _ -> next c
						in
						let rec up c = match c.cl_super with
							| None -> ()
							| Some(c,_) -> loop up c
						in
						let rec down c = List.iter (fun c -> loop down c) c.cl_descendants in
						loop up c;
						loop down c
					end;
				end
			in
			List.iter field (cf :: cf.cf_overloads)
end

class ['a] typedef_interfaces (infos : 'a info_context) (anon_identification : 'a tanon_identification) = object(self)

	val interfaces = Hashtbl.create 0
	val interface_rewrites = Hashtbl.create 0

	method add_interface_rewrite (path_from : path) (path_to : path) (is_extern : bool) =
		Hashtbl.replace interface_rewrites path_from (path_to,is_extern)

	method get_interface_class (path : path) =
		try Some (Hashtbl.find interfaces path)
		with Not_found -> None

	method get_interfaces = interfaces

	method process_class (c : tclass) =
		let info = infos#get_class_info c in
		match info.typedef_implements with
		| Some _ ->
			()
		| None ->
			self#do_process_class c info

	method private implements (c : tclass) (path_interface : path) =
		let info = infos#get_class_info c in
		List.exists (fun (c,_) ->
			c.cl_path = path_interface
		) c.cl_implements
		|| match info.typedef_implements with
		| None ->
			false
		| Some l ->
			List.exists (fun c -> c.cl_path = path_interface) l

	method private implements_recursively (c : tclass) (path : path) =
		self#implements c path || match c.cl_super with
			| Some (c,_) -> self#implements_recursively c path
			| None -> false

	method private make_interface_class (pfm : 'a path_field_mapping) (path : path) (is_extern : bool) =
		let path_inner = (fst pfm.pfm_path,snd pfm.pfm_path ^ "$Interface") in
		try
			Hashtbl.find interfaces path_inner
		with Not_found ->
			let fields = PMap.foldi (fun name cf acc -> match cf.cf_kind with
				| Method (MethNormal | MethInline) ->
					PMap.add name cf acc
				| _ ->
					acc
			) pfm.pfm_fields PMap.empty in
			if PMap.is_empty fields then raise (Unify_error [Unify_custom "no fields"]);
			let c = mk_class null_module path null_pos null_pos in
			add_class_flag c CInterface;
			c.cl_fields <- fields;
			c.cl_ordered_fields <- PMap.fold (fun cf acc -> cf :: acc) fields [];
			if is_extern then add_class_flag c CExtern;
			Hashtbl.replace interfaces pfm.pfm_path c;
			c

	method private do_process_class (c : tclass) (info : 'a tclass_info) =
		begin match c.cl_super with
			| Some(c,_) -> self#process_class c
			| None -> ()
		end;
		let tc = TInst(c,extract_param_types c.cl_params) in
		(* TODO: this entire architecture looks slightly retarded because typedef_implements is only modified at the end of the
		   loop, which I think could cause items to be missed. *)
		let l = Hashtbl.fold (fun _ pfm acc ->
			let path = pfm.pfm_path in
			let path_inner = (fst path,snd path ^ "$Interface") in
			try
				let path_inner,is_extern = try Hashtbl.find interface_rewrites pfm.pfm_path with Not_found -> path_inner,false in
				if self#implements_recursively c path_inner then raise (Unify_error [Unify_custom "already implemented"]);
				anon_identification#unify ~strict:false tc pfm;
				let ci = self#make_interface_class pfm path_inner is_extern in
				c.cl_implements <- (ci,[]) :: c.cl_implements;
				(* print_endline (Printf.sprintf "%s IMPLEMENTS %s" (s_type_path c.cl_path) (s_type_path path_inner)); *)
				(ci :: acc)
			with Unify_error _ ->
				acc
		) anon_identification#get_pfms [] in
		info.typedef_implements <- Some l
end
