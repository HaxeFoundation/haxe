open Globals
open Type
open Warning

type context_main = {
	mutable main_class : path option;
	mutable main_expr : texpr option;
}

type warning_function = ?depth:int -> ?from_macro:bool -> warning -> Warning.warning_option list list -> string -> pos -> unit
type error_function = ?depth:int -> string -> pos -> unit

type t = {
	platform : platform;
	defines : Define.define;
	class_paths : ClassPaths.class_paths;
	run_command : string -> int;
	run_command_args : string -> string list -> int;
	warning : warning_function;
	error : error_function;
	print : string -> unit;
	basic : basic_types;
	debug : bool;
	file : string;
	version : int;
	features : (string,bool) Hashtbl.t;
	modules : Type.module_def list;
	main : context_main;
	types : Type.module_type list;
	resources : (string,string) Hashtbl.t;
	native_libs : NativeLibraries.native_library_base list;
	include_files : (string * string) list;
	std : tclass; (* TODO: I would prefer to not have this here, have to check default_cast *)
}

let defined com s =
	Define.defined com.defines s

let defined_value com v =
	Define.defined_value com.defines v

let define_value com k v =
	Define.define_value com.defines k v

let defined_value_safe ?default com v =
	match default with
		| Some s -> Define.defined_value_safe ~default:s com.defines v
		| None -> Define.defined_value_safe com.defines v

let raw_defined gctx v =
	Define.raw_defined gctx.defines v

let find_file ctx f =
	(ctx.class_paths#find_file f).file

let add_feature gctx f =
	Hashtbl.replace gctx.features f true

let has_dce gctx =
	try
		Define.defined_value gctx.defines Define.Dce <> "no"
with Not_found ->
	false

let is_directly_used gctx meta =
	not (has_dce gctx) || Meta.has Meta.DirectlyUsed meta

let rec has_feature gctx f =
	try
		Hashtbl.find gctx.features f
	with Not_found ->
		if gctx.types = [] then not (has_dce gctx) else
		match List.rev (ExtString.String.nsplit f ".") with
		| [] -> die "" __LOC__
		| [cl] -> has_feature gctx (cl ^ ".*")
		| field :: cl :: pack ->
			let r = (try
				let path = List.rev pack, cl in
				(match List.find (fun t -> t_path t = path && not (Meta.has Meta.RealPath (t_infos t).mt_meta)) gctx.types with
				| t when field = "*" ->
					not (has_dce gctx) ||
					begin match t with
						| TClassDecl c ->
							has_class_flag c CUsed;
						| TAbstractDecl a ->
							Meta.has Meta.ValueUsed a.a_meta
						| _ -> Meta.has Meta.Used (t_infos t).mt_meta
					end;
				| TClassDecl c when (has_class_flag c CExtern) && (gctx.platform <> Js || cl <> "Array" && cl <> "Math") ->
					not (has_dce gctx) || has_class_field_flag (try PMap.find field c.cl_statics with Not_found -> PMap.find field c.cl_fields) CfUsed
				| TClassDecl c ->
					PMap.exists field c.cl_statics || PMap.exists field c.cl_fields
				| _ ->
					false)
			with Not_found ->
				false
			) in
			Hashtbl.add gctx.features f r;
			r

let get_entry_point gctx =
	Option.map (fun path ->
		let m = List.find (fun m -> m.m_path = path) gctx.modules in
		let c =
			match m.m_statics with
			| Some c when (PMap.mem "main" c.cl_statics) -> c
			| _ -> Option.get (ExtList.List.find_map (fun t -> match t with TClassDecl c when c.cl_path = path -> Some c | _ -> None) m.m_types)
		in
		let e = Option.get gctx.main.main_expr in (* must be present at this point *)
		(snd path, c, e)
	) gctx.main.main_class

let get_es_version defines =
	try int_of_string (Define.defined_value defines Define.JsEs) with _ -> 0

let map_source_header defines f =
	match Define.defined_value_safe defines Define.SourceHeader with
	| "" -> ()
	| s -> f s