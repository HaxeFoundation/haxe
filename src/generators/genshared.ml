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

open OverloadResolution

type 'a path_field_mapping = {
	pfm_path : path;
	pfm_params : type_params;
	pfm_fields : (string,tclass_field) PMap.t;
	mutable pfm_converted : (string * 'a) list option;
	pfm_arity : int;
}

let count_fields pm =
	PMap.fold (fun _ i -> i + 1) pm 0

let pfm_of_typedef td = match follow td.t_type with
	| TAnon an -> {
		pfm_path = td.t_path;
		pfm_params = td.t_params;
		pfm_fields = an.a_fields;
		pfm_converted = None;
		pfm_arity = count_fields an.a_fields;
	}
	| _ ->
		die "" __LOC__

class ['a] tanon_identification (empty_path : string list * string) =
	let is_normal_anon an = match !(an.a_status) with
		| Closed | Const -> true
		| _ -> false
	in
object(self)

	val pfms = Hashtbl.create 0
	val pfm_by_arity = DynArray.create ()
	val mutable num = 0

	method get_pfms = pfms

	method add_pfm (path : path) (pfm : 'a path_field_mapping) =
		while DynArray.length pfm_by_arity <= pfm.pfm_arity do
			DynArray.add pfm_by_arity (DynArray.create ())
		done;
		DynArray.add (DynArray.get pfm_by_arity pfm.pfm_arity) pfm;
		Hashtbl.replace pfms path pfm

	method unify (tc : Type.t) (pfm : 'a path_field_mapping) =
		let check () =
			let pair_up fields =
				PMap.fold (fun cf acc ->
					let cf' = PMap.find cf.cf_name fields in
					(cf,cf') :: acc
				) pfm.pfm_fields []
			in
			let monos = match follow tc with
				| TInst(c,tl) ->
					let pairs = pair_up c.cl_fields in
					let monos = List.map (fun _ -> mk_mono()) pfm.pfm_params in
					let map = apply_params pfm.pfm_params monos in
					List.iter (fun (cf,cf') ->
						if not (unify_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
						Type.unify (apply_params c.cl_params tl (monomorphs cf'.cf_params cf'.cf_type)) (map (monomorphs cf.cf_params cf.cf_type))
					) pairs;
					monos
				| TAnon an1 ->
					let fields = ref an1.a_fields in
					let pairs = pair_up an1.a_fields in
					let monos = List.map (fun _ -> mk_mono()) pfm.pfm_params in
					let map = apply_params pfm.pfm_params monos in
					List.iter (fun (cf,cf') ->
						if not (unify_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
						fields := PMap.remove cf.cf_name !fields;
						Type.type_eq EqDoNotFollowNull cf'.cf_type (map (monomorphs cf.cf_params cf.cf_type))
					) pairs;
					if not (PMap.is_empty !fields) then raise (Unify_error [Unify_custom "not enough fields"]);
					monos
				| _ ->
					raise (Unify_error [Unify_custom "bad type"])
			in
			(* Check if we applied Void to a return type parameter... (#3463) *)
			List.iter (fun t -> match follow t with
				| TMono r ->
					Monomorph.bind r t_dynamic
				| t ->
					if Type.ExtType.is_void t then raise(Unify_error [Unify_custom "return mono"])
			) monos
		in
		try
			check()
		with Not_found ->
			raise (Unify_error [])

	method find_compatible (arity : int) (tc : Type.t) =
		if arity >= DynArray.length pfm_by_arity then
			raise Not_found;
		let d = DynArray.get pfm_by_arity arity in
		let l = DynArray.length d in
		let rec loop i =
			if i >= l then
				raise Not_found;
			let pfm = DynArray.unsafe_get d i in
			try
				self#unify tc pfm;
				pfm
			with Unify_error _ ->
				loop (i + 1)
		in
		loop 0

	method identify_typedef (td : tdef) =
		let rec loop t = match t with
			| TAnon an when is_normal_anon an && not (PMap.is_empty an.a_fields) ->
				self#add_pfm td.t_path (pfm_of_typedef td)
			| TMono {tm_type = Some t} ->
				loop t
			| TLazy f ->
				loop (lazy_type f)
			| t ->
				()
		in
		loop td.t_type

	method identify (accept_anons : bool) (t : Type.t) =
		match t with
		| TType(td,tl) ->
			begin try
				Some (Hashtbl.find pfms td.t_path)
			with Not_found ->
				self#identify accept_anons (apply_params td.t_params tl td.t_type)
			end
		| TMono {tm_type = Some t} ->
			self#identify accept_anons t
		| TAbstract(a,tl) when not (Meta.has Meta.CoreType a.a_meta) ->
			self#identify accept_anons (Abstract.get_underlying_type a tl)
		| TAbstract({a_path=([],"Null")},[t]) ->
			self#identify accept_anons t
		| TLazy f ->
			self#identify accept_anons (lazy_type f)
		| TAnon an when accept_anons && not (PMap.is_empty an.a_fields) ->
			let arity = PMap.fold (fun cf i ->
				Gencommon.replace_mono cf.cf_type;
				i + 1
			) an.a_fields 0 in
			begin try
				Some (self#find_compatible arity t)
			with Not_found ->
				let id = num in
				num <- num + 1;
				let path = (["haxe";"generated"],Printf.sprintf "Anon%i" id) in
				let pfm = {
					pfm_path = path;
					pfm_params = [];
					pfm_fields = an.a_fields;
					pfm_converted = None;
					pfm_arity = count_fields an.a_fields;
				} in
				self#add_pfm path pfm;
				Some pfm
			end;
		| _ ->
			None
end

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
			| (Meta.Custom ":jvm.classInfo",[(EConst (Int s),_)],_) :: _ ->
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
				c.cl_meta <- (Meta.Custom ":jvm.classInfo",[(EConst (Int (string_of_int index)),null_pos)],null_pos) :: c.cl_meta;
				infos
			in
			loop c.cl_meta
	end
end

open Info


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
		| (Meta.Custom ":jvm.fieldInfo",[(EConst (Int s),_)],_) :: _ ->
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
			| None -> Error.typing_error "Could not find overload constructor" e.epos
		in
		let find_super_ctor el =
			let _,cf = find_super_ctor el in
			(* This is a bit hacky: We always want the direct super class, not the one that actually holds
			   the ctor. It will be implicitly copied to it anyway. *)
			match c.cl_super with
			| None -> die "" __LOC__
			| Some(c,_) -> c,cf
		in
		let rec promote_this_before_super c cf = match self#get_field_info cf.cf_meta with
			| None -> failwith "Something went wrong"
			| Some info ->
				if not info.has_this_before_super then begin
					make_haxe cf;
					(* print_endline (Printf.sprintf "promoted this_before_super to %s.new : %s" (s_type_path c.cl_path) (s_type (print_context()) cf.cf_type)); *)
					info.has_this_before_super <- true;
					List.iter (fun (c,cf) -> promote_this_before_super c cf) info.super_call_fields
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
				if !this_before_super then promote_this_before_super c cf;
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
					cf.cf_meta <- (Meta.Custom ":jvm.fieldInfo",[(EConst (Int (string_of_int index)),null_pos)],null_pos) :: cf.cf_meta;
					if not (Meta.has Meta.HxGen cf.cf_meta) then begin
						let rec loop next c =
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
		match info.typedef_implements with
		| None ->
			false
		| Some l ->
			List.exists (fun c -> c.cl_path = path_interface) l

	method private implements_recursively (c : tclass) (path : path) =
		self#implements c path || match c.cl_super with
			| Some (c,_) -> self#implements_recursively c path
			| None -> false

	method private make_interface_class (pfm : 'a path_field_mapping) =
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
			let path,is_extern = try Hashtbl.find interface_rewrites pfm.pfm_path with Not_found -> path_inner,false in
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
		let tc = TInst(c,List.map snd c.cl_params) in
		let l = Hashtbl.fold (fun _ pfm acc ->
			let path = pfm.pfm_path in
			let path_inner = (fst path,snd path ^ "$Interface") in
			try
				if self#implements_recursively c path_inner then raise (Unify_error [Unify_custom "already implemented"]);
				anon_identification#unify tc pfm;
				let ci = self#make_interface_class pfm in
				c.cl_implements <- (ci,[]) :: c.cl_implements;
				(* print_endline (Printf.sprintf "%s IMPLEMENTS %s" (s_type_path c.cl_path) (s_type_path path_inner)); *)
				(ci :: acc)
			with Unify_error _ ->
				acc
		) anon_identification#get_pfms [] in
		info.typedef_implements <- Some l
end