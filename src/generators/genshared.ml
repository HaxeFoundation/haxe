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
				| e :: el,(n,o,t) :: tl ->
					begin try
						Type.unify e.etype t;
						loop2 ((e,o) :: acc) el tl
					with _ ->
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

class ['a] tanon_identification (empty_path : string list * string) (convert : Type.t -> 'a) = object(self)
	val lut = Hashtbl.create 0
	val path_lut = Hashtbl.create 0
	val mutable num = 0

	method get_lut = lut

	method convert_fields (fields : (string,tclass_field) PMap.t) =
		let l = PMap.fold (fun cf acc -> cf :: acc) fields [] in
		let l = List.sort (fun cf1 cf2 -> compare cf1.cf_name cf2.cf_name) l in
		List.map (fun cf -> cf.cf_name,convert cf.cf_type) l

	method identify (fields : (string,tclass_field) PMap.t) =
		if PMap.is_empty fields then
			empty_path,[]
		else begin
			let l = self#convert_fields fields in
			try
				Hashtbl.find lut l,l
			with Not_found ->
				let id = num in
				num <- num + 1;
				let path = (["haxe";"generated"],Printf.sprintf "Anon%i" id) in
				Hashtbl.add lut l path;
				path,l
		end

	method identify_as path (fields : (string,tclass_field) PMap.t) =
		if not (PMap.is_empty fields) && not (Hashtbl.mem path_lut path) then begin
			let fields = self#convert_fields fields in
			Hashtbl.add lut fields path;
			Hashtbl.add path_lut path path;
		end
end

type field_generation_info = {
	mutable has_this_before_super : bool;
	(* This is an ordered list of fields that are targets of super() calls which is determined during
	   pre-processing. The generator can pop from this list assuming that it processes the expression
	   in the same order (which it should). *)
	mutable super_call_fields : (tclass * tclass_field) list;
}

class ['a] preprocessor (basic : basic_types) (anon_identification : 'a tanon_identification) (convert : Type.t -> 'a) =
	let is_normal_anon an = match !(an.a_status) with
		| Closed | Const | Opened -> true
		| _ -> false
	in
	let check_anon e = match e.etype,follow e.etype with
		| TType(td,_),TAnon an when is_normal_anon an ->
			ignore(anon_identification#identify_as td.t_path an.a_fields)
		| _ ->
			()
	in
	let make_native cf =
		cf.cf_meta <- (Meta.NativeGen,[],null_pos) :: cf.cf_meta
	in
	let make_haxe cf =
		cf.cf_meta <- (Meta.HxGen,[],null_pos) :: cf.cf_meta
	in
	let rec get_constructor c =
		match c.cl_constructor, c.cl_super with
		| Some cf, _ -> c,cf
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
				| _ -> assert false
			in
			match find_overload_rec' true map_type csup "new" el with
			| Some(c,cf,_) ->
				let rec loop csup =
					if c != csup then begin
						match csup.cl_super with
						| Some(c',_) ->
							self#add_implicit_ctor csup c' cf;
							loop c'
						| None -> assert false
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
			| None -> assert false
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
			check_anon e;
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

	method preprocess_expr e =
		let rec loop e =
			check_anon e;
			Type.iter loop e
		in
		loop e

	method check_overrides c = match c.cl_overrides with
		| []->
			()
		| fields ->
			let csup,map_type = match c.cl_super with
				| Some(c,tl) -> c,apply_params c.cl_params tl
				| None -> assert false
			in
			let fix_covariant_return cf =
				let tl = match follow cf.cf_type with
					| TFun(tl,_) -> tl
					| _ -> assert false
				in
				match find_overload_rec' false map_type csup cf.cf_name (List.map (fun (_,_,t) -> Texpr.Builder.make_null t null_pos) tl) with
				| Some(_,cf',_) ->
					let tr = match follow cf'.cf_type with
						| TFun(_,tr) -> tr
						| _ -> assert false
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
		let field cf = match cf.cf_expr with
			| None ->
				()
			| Some e ->
				self#preprocess_expr e
		in
		let has_dynamic_instance_method = ref false in
		let has_field_init = ref false in
		let field mtype cf =
			List.iter field (cf :: cf.cf_overloads);
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
				let csup,cf = get_constructor c in
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
