open Globals
open Ast
open Type
open Typecore

let expr_to_target e =
	let rec loop (e,p) =
		match e with
		| EConst (Ident s) when s <> "" -> [s]
		| EField (e,s,_) -> s :: loop e
		| _ -> Error.typing_error "Invalid target expression for @:inheritDoc" p
	in
	match loop e with
	| sub_name :: type_name :: pack when not (is_lower_ident type_name) ->
		(List.rev pack, type_name), Some sub_name
	| type_name :: pack ->
		(List.rev pack, type_name), None
	| [] ->
		Error.typing_error "Invalid target path for @:inheritDoc" (snd e)

let rec get_constructor c =
	match c.cl_constructor, c.cl_super with
	| Some ctor, _ -> Some c, ctor
	| None, None -> raise Not_found
	| None, Some (csup,_) -> get_constructor csup

let rec get_class_field c field_name =
	try
		let cf =
			try PMap.find field_name c.cl_fields
			with Not_found -> PMap.find field_name c.cl_statics
		in
		Some c, cf
	with Not_found ->
		match c.cl_super with
		| None -> raise Not_found
		| Some (csup, _) -> get_class_field csup field_name

let find_type ctx tp allow_no_params =
	try Typeload.load_instance' ctx tp allow_no_params
	with _ -> raise Not_found

(**
	Finds `@:inheritDoc` meta in `meta` and populates `doc_inherited` field of `doc`
	with found docs.
*)
let rec build_doc ctx ?no_args_cb doc meta =
	let add d =
		match d with
		| None -> ()
		| Some d ->
			match gen_doc_text d with
			| "" -> ()
			| s ->
				match !doc with
				| None -> doc := Some { doc_own = None; doc_inherited = [s]; }
				| Some doc -> doc.doc_inherited <- s :: doc.doc_inherited
	in
	List.iter (fun m ->
		match m with
		| (Meta.InheritDoc,[],_) ->
			(match no_args_cb with
			| Some fn -> fn add
			| None -> ())
		| (Meta.InheritDoc,targets,_) ->
			List.iter (fun t -> add (get_target_doc ctx t)) targets
		| _ -> ()
	) meta

(**
	Populates `doc_inherited` field of `c.cl_doc`
*)
and build_class_doc ctx c =
	(match c.cl_doc with
	| None | Some { doc_inherited = [] } -> ()
	| Some d -> d.doc_inherited <- []
	);
	let doc = ref c.cl_doc in
	let no_args_cb add =
		match c.cl_super with
		| None -> ()
		| Some (csup,_) ->
			build_class_doc ctx csup;
			add csup.cl_doc
	in
	build_doc ctx ~no_args_cb doc c.cl_meta;
	c.cl_doc <- !doc

(**
	Populates `doc_inherited` field of `enm.e_doc`
*)
and build_enum_doc ctx enm =
	(match enm.e_doc with
	| None | Some { doc_inherited = [] } -> ()
	| Some d -> d.doc_inherited <- []
	);
	let doc = ref enm.e_doc in
	build_doc ctx doc enm.e_meta;
	enm.e_doc <- !doc

(**
	Populates `doc_inherited` field of `a.a_doc`
*)
and build_abstract_doc ctx a =
	(match a.a_doc with
	| None | Some { doc_inherited = [] } -> ()
	| Some d -> d.doc_inherited <- []
	);
	let doc = ref a.a_doc in
	build_doc ctx doc a.a_meta;
	a.a_doc <- !doc

(**
	Populates `doc_inherited` field of `cf.cf_doc`
*)
and build_class_field_doc ctx c_opt cf =
	(match cf.cf_doc with
	| None | Some { doc_inherited = [] } -> ()
	| Some d -> d.doc_inherited <- []
	);
	let doc = ref cf.cf_doc in
	let no_args_cb add =
		match c_opt with
		| Some { cl_super = Some (csup,_) } ->
			(try
				let c_opt, cf_sup =
					if cf.cf_name = "new" then get_constructor csup
					else get_class_field csup cf.cf_name
				in
				build_class_field_doc ctx c_opt cf_sup;
				add cf_sup.cf_doc
			with Not_found -> ())
		| _ -> ()
	in
	build_doc ctx ~no_args_cb doc cf.cf_meta;
	cf.cf_doc <- !doc

(**
	Populates `doc_inherited` field of `ef.ef_doc`
*)
and build_enum_field_doc ctx ef =
	(match ef.ef_doc with
	| None | Some { doc_inherited = [] } -> ()
	| Some d -> d.doc_inherited <- []
	);
	let doc = ref ef.ef_doc in
	build_doc ctx doc ef.ef_meta;
	ef.ef_doc <- !doc

(**
	Collects `Ast.documentation` for a provided `target`
	The `target` is an AST expr representing a dot path for a type or a field.
	E.g. `my.pack.MyType` or `my.pack.MyType.field`
*)
and get_target_doc ctx e_target =
	let path,sub = expr_to_target e_target in
	let resolve_field field_name =
		let tp =
			match List.rev (fst path) with
			| module_name :: pack_rev when not (is_lower_ident module_name) ->
				mk_type_path ~sub:(snd path) (List.rev pack_rev,module_name)
			| _ ->
				mk_type_path path
		in
		let t = (find_type ctx (tp,snd e_target) true) in
		try
			match follow t with
			| TInst (c, _) ->
				let c_opt, cf =
					if field_name = "new" then get_constructor c
					else get_class_field c field_name
				in
				build_class_field_doc ctx c_opt cf;
				cf.cf_doc
			| TAnon a ->
				let cf = PMap.find field_name a.a_fields in
				build_class_field_doc ctx None cf;
				cf.cf_doc
			| TEnum (enm, _) ->
				let ef = PMap.find field_name enm.e_constrs in
				build_enum_field_doc ctx ef;
				ef.ef_doc
			| TAbstract ({ a_impl = Some c }, _) ->
				let c_opt, cf =
					let field_name =
						if field_name = "new" then "_new"
						else field_name
					in
					get_class_field c field_name
				in
				build_class_field_doc ctx c_opt cf;
				cf.cf_doc
			| _ -> raise Not_found
		with Not_found ->
			None
	in
	let rec resolve_type_t t =
		match follow t with
		| TInst (c, _) ->
			build_class_doc ctx c;
			c.cl_doc
		| TAbstract (a, _) ->
			build_abstract_doc ctx a;
			a.a_doc
		| TEnum (enm, _) ->
			build_enum_doc ctx enm;
			enm.e_doc
		| _ -> raise Not_found
	in
	let resolve_type () =
		let tp = mk_type_path path, snd e_target in
		resolve_type_t (find_type ctx tp true)
	in
	let resolve_sub_type sub =
		let tp = mk_type_path ~sub path, snd e_target in
		resolve_type_t (find_type ctx tp true)
	in
	try
		match sub with
		(* type *)
		| None ->
			resolve_type()
		(* field or sub type *)
		| Some s ->
			if is_lower_ident s then
				resolve_field s
			else
				(try resolve_sub_type s
				with Not_found -> resolve_field s)
	with Not_found ->
		None
