open Globals
open Common
open Type
open Typecore
open Typer
open Resolution
open Error

let load_std_types ctx =
	let std_types = try
		TypeloadModule.load_module ctx ([],"StdTypes") null_pos
	with
		Error { err_message = Module_not_found ([],"StdTypes") } ->
			Error.raise_std_not_found ()
	in
	List.iter (fun t ->
		ctx.m.import_resolution#add (module_type_resolution t None null_pos);
		match t with
		| TAbstractDecl a ->
			(match snd a.a_path with
			| "Void" ->
				let t = TAbstract (a,[]) in
				Type.unify t ctx.t.tvoid;
				ctx.t.tvoid <- t;
			| "Float" ->
				let t = (TAbstract (a,[])) in
				Type.unify t ctx.t.tfloat;
				ctx.t.tfloat <- t
			| "Int" ->
				let t = (TAbstract (a,[])) in
				Type.unify t ctx.t.tint;
				ctx.t.tint <- t
			| "Bool" ->
				let t = (TAbstract (a,[])) in
				Type.unify t ctx.t.tbool;
				ctx.t.tbool <- t
			| "Dynamic" ->
				ctx.g.t_dynamic_def <- TAbstract(a,extract_param_types a.a_params);
			| "Null" ->
				let mk_null t =
					try
						if not (is_null ~no_lazy:true t || is_explicit_null t) then TAbstract (a,[t]) else t
					with Exit ->
						(* don't force lazy evaluation *)
						let r = ref (lazy_available t_dynamic) in
						r := lazy_wait (fun() ->
							let t = (if not (is_null t) then TAbstract (a,[t]) else t) in
							r := lazy_available t;
							t
						);
						TLazy r
				in
				ctx.t.tnull <- mk_null;
			| _ -> ())
		| TTypeDecl td ->
			begin match snd td.t_path with
			| "Iterator" ->
				ctx.t.titerator <- (fun t -> TType(td,[t]))
			| _ ->
				()
			end
		| TEnumDecl _ | TClassDecl _ ->
			()
	) (List.rev std_types.m_types)

let load_string ctx =
	let m = TypeloadModule.load_module ctx ([],"String") null_pos in
	List.iter (fun mt -> match mt with
		| TClassDecl ({cl_path = ([],"String")} as c) ->
			let t = (TInst (c,[])) in
			Type.unify t ctx.t.tstring;
			ctx.t.tstring <- t
		| _ -> ()
	) m.m_types

let load_std ctx =
	let m = TypeloadModule.load_module ctx ([],"Std") null_pos in
	List.iter (fun mt -> match mt with
		| TClassDecl ({cl_path = ([],"Std")} as c) -> ctx.com.std <- c;
		| _ -> ()
	) m.m_types

let load_any ctx =
	let m = TypeloadModule.load_module ctx ([],"Any") null_pos in
	List.iter (fun mt -> match mt with
		| TAbstractDecl a ->
			(match snd a.a_path with
			| "Any" ->
				let t = TAbstract (a,[]) in
				Type.unify t ctx.t.tany;
				ctx.t.tany <- t;
			| _ -> ())
		| _ -> ()
	) m.m_types

let load_array ctx =
	let m = TypeloadModule.load_module ctx ([],"Array") null_pos in
	try
		List.iter (fun t -> (
			match t with
			| TClassDecl ({cl_path = ([],"Array")} as c) ->
				ctx.t.tarray <- (fun t -> TInst (c,[t]));
				raise Exit
			| _ ->
				()
		)) m.m_types;
		die "" __LOC__
	with Exit ->
		()

let load_unit ctx =
	let m = TypeloadModule.load_module ctx (["haxe"],"Unit") null_pos in
	List.iter (fun mt -> match mt with
		| TEnumDecl en ->
			(match snd en.e_path with
			| "Unit" ->
				ctx.m.import_resolution#add (module_type_resolution mt None null_pos);
			| _ -> ())
		| _ -> ()
	) m.m_types

let load_enum_tools ctx =
	let m = TypeloadModule.load_module ctx (["haxe"],"EnumTools") null_pos in
	match m.m_types with
	| [TClassDecl c1;TClassDecl c2] ->
		ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
	| [TClassDecl c1] ->
		let m = TypeloadModule.load_module ctx (["haxe"],"EnumValueTools") null_pos in
		begin match m.m_types with
		| [TClassDecl c2 ] ->
			ctx.g.global_using <- (c1,c1.cl_pos) :: (c2,c2.cl_pos) :: ctx.g.global_using
		| _ ->
			die "" __LOC__
		end;
	| _ ->
		die "" __LOC__

let load_local_wrapper ctx =
	let t = ctx.t in
	match ctx.com.platform with
	(* optimized version for Java - use native arrays *)
	| Jvm ->
		let cnativearray =
			let m = TypeloadModule.load_module ctx (["jvm"],"NativeArray") null_pos in
			let mt = List.find (fun md -> match md with
					| TClassDecl ({ cl_path = ["jvm"],"NativeArray" }) -> true
					| _ -> false
				) m.m_types
			in
			match mt with
			| TClassDecl cl -> cl
			| _ -> die "" __LOC__
		in

		object
			method captured_type t = TInst (cnativearray,[t])

			method mk_ref v ve p =
				match ve with
				| None ->
					let eone = mk (TConst (TInt (Int32.of_int 1))) t.tint p in
					let t = match v.v_type with TInst (_, [t]) -> t | _ -> die "" __LOC__ in
					mk (TNew (cnativearray,[t],[eone])) v.v_type p
				| Some e ->
					{ (Inline.mk_untyped_call "__array__" p [e]) with etype = v.v_type }

			method mk_ref_access e v =
				mk (TArray ({ e with etype = v.v_type }, mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos

			method mk_init av v pos =
				let elocal = mk (TLocal v) v.v_type pos in
				let earray = { (Inline.mk_untyped_call "__array__" pos [elocal]) with etype = av.v_type } in
				mk (TVar (av,Some earray)) t.tvoid pos
		end
	(* default implementation - use haxe array *)
	| _ ->
		object
			method captured_type = t.tarray
			method mk_ref v ve p =
				mk (TArrayDecl (match ve with None -> [] | Some e -> [e])) v.v_type p
			method mk_ref_access e v =
				mk (TArray ({ e with etype = v.v_type }, mk (TConst (TInt 0l)) t.tint e.epos)) e.etype e.epos
			method mk_init av v pos =
				mk (TVar (av,Some (mk (TArrayDecl [mk (TLocal v) v.v_type pos]) av.v_type pos))) t.tvoid pos
		end

let create com macros =
	let rec ctx = {
		com = com;
		t = com.basic;
		g = {
			core_api = None;
			macros = macros;
			module_check_policies = [];
			delayed = Array.init TyperPass.all_typer_passes_length (fun _ -> { tasks = []});
			delayed_min_index = 0;
			debug_delayed = [];
			retain_meta = Common.defined com Define.RetainUntypedMeta;
			global_using = [];
			complete = false;
			type_hints = [];
			load_only_cached_modules = false;
			return_partial_type = false;
			build_count = 0;
			t_dynamic_def = t_dynamic;
			do_macro = MacroContext.type_macro;
			do_load_macro = MacroContext.load_macro';
			do_load_module = TypeloadModule.load_module;
			do_load_type_def = Typeload.load_type_def;
			get_build_info = InstanceBuilder.get_build_info;
			do_format_string = format_string;
			do_load_core_class = Typeload.load_core_class;
			delayed_display = None;
			root_typer = ctx;
		};
		m = {
			curmod = null_module;
			import_resolution = new resolution_list ["import";"typer"];
			own_resolution = None;
			enum_with_type = None;
			module_using = [];
			import_statements = [];
			is_display_file = false;
		};
		c = {
			curclass = null_class;
			tthis = t_dynamic;
			get_build_infos = (fun() -> None);
		};
		f = TyperManager.create_ctx_f null_field;
		e = TyperManager.create_ctx_e FunStatic FunFunction;
		pass = PBuildModule;
		allow_inline = true;
		allow_transform = true;
		type_params = [];
		memory_marker = Typecore.memory_marker;
	} in
	load_std_types ctx;
	load_string ctx;
	load_std ctx;
	load_any ctx;
	(* load_unit ctx; *)
	load_array ctx;
	load_enum_tools ctx;
	ignore(TypeloadModule.load_module ctx (["haxe"],"Exception") null_pos);
	ctx.com.local_wrapper <- load_local_wrapper ctx;
	ctx.g.complete <- true;
	ctx

;;
create_context_ref := create;
Inline.maybe_reapply_overload_call_ref := CallUnification.maybe_reapply_overload_call;
