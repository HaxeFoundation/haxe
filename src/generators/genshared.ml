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
	| Some {cl_extern = true} -> true
	| _ -> match a.a_path with
		| ([],("Void" | "Float" | "Int" | "Single" | "Bool" | "Null")) -> true
		| _ -> false

let unify_cf map_type c cf el =
	let monos = List.map (fun _ -> mk_mono()) cf.cf_params in
	match follow (apply_params cf.cf_params monos (map_type cf.cf_type)) with
		| TFun(tl'',_) as tf ->
			let rec loop2 acc el tl = match el,tl with
				| e :: el,(_,o,t) :: tl ->
					begin try
						Type.unify e.etype t;
						loop2 ((e,o) :: acc) el tl
					with _ ->
						match t,tl with
						| TAbstract({a_path=["haxe";"extern"],"Rest"},[t]),[] ->
							begin try
								let el = List.map (fun e -> unify t e.etype; e,o) el in
								Some ((List.rev acc) @ el,tf,(c,cf,monos))
							with _ ->
								None
							end
						| _ ->
							None
					end
				| [],[] ->
					Some ((List.rev acc),tf,(c,cf,monos))
				| _ ->
					None
			in
			loop2 [] el tl''
		| t ->
			None

let unify_cf_with_fallback map_type c cf el =
	match unify_cf map_type c cf el with
	| Some(_,_,r) -> r
	| None -> (c,cf,List.map snd cf.cf_params)

let find_overload map_type c cf el =
	let matches = ref [] in
	let rec loop cfl = match cfl with
		| cf :: cfl ->
			begin match unify_cf map_type c cf el with
			| Some r -> matches := r :: !matches;
			| None -> ()
			end;
			loop cfl
		| [] ->
			List.rev !matches
	in
	loop (cf :: cf.cf_overloads)

let filter_overloads candidates =
	match Overloads.Resolution.reduce_compatible candidates with
	| [_,_,(c,cf,tl)] -> Some(c,cf,tl)
	| [] -> None
	| ((_,_,(c,cf,tl)) :: _) (* as resolved *) ->
		(* let st = s_type (print_context()) in
		print_endline (Printf.sprintf "Ambiguous overload for %s(%s)" name (String.concat ", " (List.map (fun e -> st e.etype) el)));
		List.iter (fun (_,t,(c,cf)) ->
			print_endline (Printf.sprintf "\tCandidate: %s.%s(%s)" (s_type_path c.cl_path) cf.cf_name (st t));
		) resolved; *)
		Some(c,cf,tl)

let find_overload_rec' is_ctor map_type c name el =
	let candidates = ref [] in
	let has_function t1 (_,t2,_) =
		begin match follow t1,t2 with
		| TFun(tl1,_),TFun(tl2,_) -> type_iseq (TFun(tl1,t_dynamic)) (TFun(tl2,t_dynamic))
		| _ -> false
		end
	in
	let rec loop map_type c =
		begin try
			let cf = if is_ctor then
				(match c.cl_constructor with Some cf -> cf | None -> raise Not_found)
			else
				PMap.find name c.cl_fields
			in
			begin match find_overload map_type c cf el with
			| [] -> raise Not_found
			| l ->
				List.iter (fun ((_,t,_) as ca) ->
					if not (List.exists (has_function t) !candidates) then candidates := ca :: !candidates
				) l
			end;
			if Meta.has Meta.Overload cf.cf_meta || cf.cf_overloads <> [] then raise Not_found
		with Not_found ->
			if c.cl_interface then
				List.iter (fun (c,tl) -> loop (fun t -> apply_params c.cl_params (List.map map_type tl) t) c) c.cl_implements
			else match c.cl_super with
			| None -> ()
			| Some(c,tl) -> loop (fun t -> apply_params c.cl_params (List.map map_type tl) t) c
		end;
	in
	loop map_type c;
	filter_overloads (List.rev !candidates)

let find_overload_rec is_ctor map_type c cf el =
	if Meta.has Meta.Overload cf.cf_meta || cf.cf_overloads <> [] then
		find_overload_rec' is_ctor map_type c cf.cf_name el
	else match unify_cf map_type c cf el with
		| Some (_,_,(c,cf,tl)) -> Some (c,cf,tl)
		| None -> Some(c,cf,List.map snd cf.cf_params)

type path_field_mapping = {
	pfm_path : path;
	pfm_params : type_params;
	pfm_fields : (string,tclass_field) PMap.t;
}

let pfm_of_typedef td = match follow td.t_type with
	| TAnon an -> {
		pfm_path = td.t_path;
		pfm_params = td.t_params;
		pfm_fields = an.a_fields;
	}
	| _ ->
		die "" __LOC__

exception Typedef_result of path_field_mapping

class ['a] tanon_identification (empty_path : string list * string) =
	let is_normal_anon an = match !(an.a_status) with
		| Closed | Const -> true
		| _ -> false
	in
object(self)

	val td_anons = Hashtbl.create 0
	val mutable num = 0

	method get_anons = td_anons

	method unify (tc : Type.t) (pfm : path_field_mapping) =
		let check () =
			let monos = List.map (fun _ -> mk_mono()) pfm.pfm_params in
			let map = apply_params pfm.pfm_params monos in
			begin match follow tc with
			| TInst(c,tl) ->
				PMap.iter (fun _ cf ->
					let cf' = PMap.find cf.cf_name c.cl_fields in
					if not (unify_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
					Type.unify (apply_params c.cl_params tl (monomorphs cf'.cf_params cf'.cf_type)) (map (monomorphs cf.cf_params cf.cf_type))
				) pfm.pfm_fields
			| TAnon an1 ->
				let fields = ref an1.a_fields in
				PMap.iter (fun _ cf ->
					let cf' = PMap.find cf.cf_name an1.a_fields in
					if not (unify_kind cf'.cf_kind cf.cf_kind) then raise (Unify_error [Unify_custom "kind mismatch"]);
					fields := PMap.remove cf.cf_name !fields;
					Type.type_eq EqDoNotFollowNull cf'.cf_type (map (monomorphs cf.cf_params cf.cf_type))
				) pfm.pfm_fields;
				if not (PMap.is_empty !fields) then raise (Unify_error [Unify_custom "not enough fields"])
			| _ ->
				raise (Unify_error [Unify_custom "bad type"])
			end;
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

	method find_compatible (tc : Type.t) =
		try
			Hashtbl.iter (fun _ td ->
				try
					self#unify tc td;
					raise (Typedef_result td)
				with Unify_error _ ->
					()
			) td_anons;
			raise Not_found
		with Typedef_result td ->
			td

	method identify_typedef (td : tdef) =
		let rec loop t = match t with
			| TAnon an when is_normal_anon an && not (PMap.is_empty an.a_fields) ->
				Hashtbl.replace td_anons td.t_path (pfm_of_typedef td);
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
				Some (Hashtbl.find td_anons td.t_path)
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
			PMap.iter (fun _ cf ->
				Gencommon.replace_mono cf.cf_type
			) an.a_fields;
			begin try
				Some (self#find_compatible t)
			with Not_found ->
				let id = num in
				num <- num + 1;
				let path = (["haxe";"generated"],Printf.sprintf "Anon%i" id) in
				let pfm = {
					pfm_path = path;
					pfm_params = [];
					pfm_fields = an.a_fields;
				} in
				Hashtbl.replace td_anons path pfm;
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

	val implicit_ctors : (path,((path * 'a),(tclass * tclass_field)) PMap.t) Hashtbl.t = Hashtbl.create 0
	val field_infos : field_generation_info DynArray.t = DynArray.create()

	method get_implicit_ctor (path : path) =
		Hashtbl.find implicit_ctors path

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
		try
			let sm = Hashtbl.find implicit_ctors c.cl_path in
			Hashtbl.replace implicit_ctors c.cl_path (PMap.add (c'.cl_path,jsig) (c',cf) sm);
		with Not_found ->
			Hashtbl.add implicit_ctors c.cl_path (PMap.add (c'.cl_path,jsig) (c',cf) PMap.empty)

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
			match find_overload_rec' true map_type csup "new" el with
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
			| None -> Error.error "Could not find overload constructor" e.epos
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

	method check_overrides c = match List.filter (fun cf -> has_class_field_flag cf CfOverride) c.cl_ordered_fields with
		| [] ->
			()
		| fields ->
			let csup,map_type = match c.cl_super with
				| Some(c,tl) -> c,apply_params c.cl_params tl
				| None -> die "" __LOC__
			in
			let fix_covariant_return cf =
				let tl = match follow cf.cf_type with
					| TFun(tl,_) -> tl
					| _ -> die "" __LOC__
				in
				match find_overload_rec' false map_type csup cf.cf_name (List.map (fun (_,_,t) -> Texpr.Builder.make_null t null_pos) tl) with
				| Some(_,cf',_) ->
					let tr = match follow cf'.cf_type with
						| TFun(_,tr) -> tr
						| _ -> die "" __LOC__
					in
					cf.cf_type <- TFun(tl,tr);
					cf.cf_expr <- begin match cf.cf_expr with
						| Some ({eexpr = TFunction tf} as e) ->
							Some {e with eexpr = TFunction {tf with tf_type = tr}}
						| e ->
							e
					end;
				| None ->
					()
					(* TODO: this should never happen if we get the unification right *)
					(* Error.error "Could not find overload" cf.cf_pos *)
			in
			List.iter (fun cf ->
				fix_covariant_return cf;
				List.iter fix_covariant_return cf.cf_overloads
			) fields

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
		self#check_overrides c;
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
							if c.cl_extern then make_native cf
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

class ['a] typedef_interfaces (anon_identification : 'a tanon_identification) = object(self)

	val lut = Hashtbl.create 0
	val interfaces = Hashtbl.create 0
	val interface_rewrites = Hashtbl.create 0

	method add_interface_rewrite (path_from : path) (path_to : path) (is_extern : bool) =
		Hashtbl.replace interface_rewrites path_from (path_to,is_extern)

	method get_interface_class (path : path) =
		try Some (Hashtbl.find interfaces path)
		with Not_found -> None

	method get_interfaces = interfaces

	method process_class (c : tclass) =
		if not (Hashtbl.mem lut c.cl_path) then
			self#do_process_class c

	method private implements (path_class : path) (path_interface : path) =
		try
			let l = Hashtbl.find lut path_class in
			List.exists (fun c -> c.cl_path = path_interface) l
		with Not_found ->
			false

	method private implements_recursively (c : tclass) (path : path) =
		self#implements c.cl_path path || match c.cl_super with
			| Some (c,_) -> self#implements_recursively c path
			| None -> false

	method private make_interface_class (pfm : path_field_mapping) =
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
			c.cl_interface <- true;
			c.cl_fields <- fields;
			c.cl_ordered_fields <- PMap.fold (fun cf acc -> cf :: acc) fields [];
			if is_extern then c.cl_extern <- true;
			Hashtbl.replace interfaces pfm.pfm_path c;
			c

	method private do_process_class (c : tclass) =
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
		) anon_identification#get_anons [] in
		Hashtbl.add lut c.cl_path l
end