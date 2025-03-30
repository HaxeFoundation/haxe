open Globals
open Type
open PlatformConfig

type saved_warning = {
	w_module : module_def;
	w_warning : WarningList.warning;
	w_options : Warning.warning_option list list;
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
	exceptions : exn list ref;
	exceptions_mutex : Mutex.t;
	warnings : saved_warning list ref;
	warnings_mutex : Mutex.t;
	curclass : tclass;
	curfield : tclass_field;
}

let add_exn com exn =
	Mutex.protect com.exceptions_mutex (fun () -> com.exceptions := exn :: !(com.exceptions))

let add_warning com w msg p =
	let options = (Warning.from_meta com.curfield.cf_meta) @ (Warning.from_meta com.curclass.cl_meta) in
	match Warning.get_mode w options with
	| WMEnable ->
		Mutex.protect com.warnings_mutex (fun () ->
			let warning = {
				w_module = com.curclass.cl_module;
				w_warning = w;
				w_options = options;
				w_msg = msg;
				w_pos = p;
			} in
			com.warnings := warning :: !(com.warnings)
		)
	| WMDisable ->
		()

let run_expression_filters_safe (com : t) detail_times filters t =
	let run com identifier e =
		List.fold_left (fun e (filter_name,f) ->
			try
				FilterContext.with_timer detail_times filter_name identifier (fun () -> f com e)
			with exc ->
				add_exn com exc;
				e
		) e filters
	in
	match t with
	| TClassDecl c when FilterContext.is_removable_class c -> ()
	| TClassDecl c ->
		let com = {com with curclass = c} in
		let rec process_field cf =
			if not (has_class_field_flag cf CfPostProcessed) then begin
				let com = {com with curfield = cf} in
				(match cf.cf_expr with
				| Some e when not (FilterContext.is_removable_field com.is_macro_context cf) ->
					let identifier = Printf.sprintf "%s.%s" (s_type_path c.cl_path) cf.cf_name in
					cf.cf_expr <- Some (run com (Some identifier) e);
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
			TClass.set_cl_init c (run com (Some identifier) e))
	| TEnumDecl _ -> ()
	| TTypeDecl _ -> ()
	| TAbstractDecl _ -> ()