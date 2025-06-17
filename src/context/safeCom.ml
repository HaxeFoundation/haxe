open Globals
open Type
open PlatformConfig

type saved_warning = {
	w_module : module_def;
	w_warning : WarningList.warning;
	w_options : warning_option list list;
	w_msg : string;
	w_pos : pos;
}

type t = {
	basic : basic_types;
	platform : platform;
	defines : Define.define;
	platform_config : platform_config;
	debug : bool;
	is_macro_context : bool;
	foptimize : bool;
	doinline : bool;
	dump_config : DumpConfig.t;
	exceptions : exn list ref;
	exceptions_mutex : Mutex.t;
	warnings : saved_warning list ref;
	warnings_mutex : Mutex.t;
	errors : Error.error list ref;
	errors_mutex : Mutex.t;
	timer_ctx : Timer.timer_context;
	find_module : path -> module_def;
	find_module_by_type : path -> module_def;
	curclass : tclass;
	curfield : tclass_field;
}

let of_com (com : Common.context) = {
	basic = com.basic;
	platform = com.platform;
	defines = com.defines;
	platform_config = com.config;
	debug = com.debug;
	is_macro_context = com.is_macro_context;
	foptimize = com.foptimize;
	doinline = com.doinline;
	dump_config = com.dump_config;
	exceptions = ref [];
	exceptions_mutex = Mutex.create ();
	warnings = ref [];
	warnings_mutex = Mutex.create ();
	errors = ref [];
	errors_mutex = Mutex.create ();
	timer_ctx = com.timer_ctx;
	find_module = com.module_lut#find;
	find_module_by_type = com.module_lut#find_by_type;
	curclass = null_class;
	curfield = null_field;
}

let of_typer (ctx : Typecore.typer) = {
	(of_com ctx.com) with
	curclass = ctx.c.curclass;
	curfield = ctx.f.curfield;
}

let finalize scom com =
	let warnings = !(scom.warnings) in
	let errors = !(scom.errors) in
	let exns = !(scom.exceptions) in
	scom.warnings := [];
	scom.errors := [];
	scom.exceptions := [];
	List.iter (fun warning ->
		Common.module_warning com warning.w_module warning.w_warning warning.w_options warning.w_msg warning.w_pos
	) warnings;
	List.iter (fun err ->
		Common.display_error_ext com err
	) errors;
	match exns with
	| x :: _ ->
		raise x
	| [] ->
		()

let run_with_scom com scom f =
	Std.finally (fun() -> finalize scom com) f ()

let add_error scom err =
	Mutex.protect scom.errors_mutex (fun () -> scom.errors := err :: !(scom.errors))

let add_exn scom exn = match exn with
	| Error.Error err ->
		add_error scom err
	| _ ->
		Mutex.protect scom.exceptions_mutex (fun () -> scom.exceptions := exn :: !(scom.exceptions))

let add_warning scom w msg p =
	let options = (Warning.from_meta scom.curfield.cf_meta) @ (Warning.from_meta scom.curclass.cl_meta) in
	match Warning.get_mode w options with
	| WMEnable ->
		Mutex.protect scom.warnings_mutex (fun () ->
			let warning = {
				w_module = scom.curclass.cl_module;
				w_warning = w;
				w_options = options;
				w_msg = msg;
				w_pos = p;
			} in
			scom.warnings := warning :: !(scom.warnings)
		)
	| WMDisable ->
		()

let run_expression_filters_safe ?(ignore_processed_status=false) scom detail_times filters t =
	let run scom identifier e =
		try
			List.fold_left (fun e (filter_name,f) ->
				try
					FilterContext.with_timer scom.timer_ctx detail_times filter_name identifier (fun () -> f scom e)
				with Failure msg ->
					Error.raise_typing_error msg e.epos
			) e filters
		with exc ->
			add_exn scom exc;
			e
	in
	match t with
	| TClassDecl c when FilterContext.is_removable_class c -> ()
	| TClassDecl c ->
		let scom = {scom with curclass = c} in
		let rec process_field cf =
			if ignore_processed_status || not (has_class_field_flag cf CfPostProcessed) then begin
				let scom = {scom with curfield = cf} in
				(match cf.cf_expr with
				| Some e when not (FilterContext.is_removable_field scom.is_macro_context cf) ->
					let identifier = Printf.sprintf "%s.%s" (s_type_path c.cl_path) cf.cf_name in
					cf.cf_expr <- Some (run scom (Some identifier) e);
				| _ -> ());
			end;
			List.iter process_field cf.cf_overloads
		in
		List.iter process_field c.cl_ordered_fields;
		List.iter process_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> process_field f);
		(match TClass.get_cl_init c with
		| None -> ()
		| Some e ->
			let identifier = Printf.sprintf "%s.__init__" (s_type_path c.cl_path) in
			TClass.set_cl_init c (run scom (Some identifier) e))
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()

let adapt_scom_to_mt scom mt = match mt with
	| TClassDecl c ->
		{scom with curclass = c}
	| _ ->
		scom

let run_type_filters_safe scom filters types =
	List.iter (fun t ->
		let scom = adapt_scom_to_mt scom t in
		List.iter (fun f -> f scom t) filters
	) types

let needs_inline scom c cf =
	cf.cf_kind = Method MethInline && (scom.doinline || Typecore.is_forced_inline c cf)