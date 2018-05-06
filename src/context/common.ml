(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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

open Ast
open Type
open Globals
open Define

type package_rule =
	| Forbidden
	| Directory of string
	| Remap of string

type pos = Globals.pos

let const_type basic const default =
	match const with
	| TString _ -> basic.tstring
	| TInt _ -> basic.tint
	| TFloat _ -> basic.tfloat
	| TBool _ -> basic.tbool
	| _ -> default

type stats = {
	s_files_parsed : int ref;
	s_classes_built : int ref;
	s_methods_typed : int ref;
	s_macros_called : int ref;
}

type compiler_message =
	| CMInfo of string * pos
	| CMWarning of string * pos
	| CMError of string * pos

let compiler_message_string msg =
	let (str,p) = match msg with
		| CMInfo(str,p) | CMError(str,p) -> (str,p)
		| CMWarning(str,p) -> ("Warning : " ^ str, p)
	in
	if p = null_pos then
		str
	else begin
		let error_printer file line = Printf.sprintf "%s:%d:" file line in
		let epos = Lexer.get_error_pos error_printer p in
		let str = String.concat ("\n" ^ epos ^ " : ") (ExtString.String.nsplit str "\n") in
		Printf.sprintf "%s : %s" epos str
	end

(**
	The capture policy tells which handling we make of captured locals
	(the locals which are referenced in local functions)

	See details/implementation in Codegen.captured_vars
*)
type capture_policy =
	(** do nothing, let the platform handle it *)
	| CPNone
	(** wrap all captured variables into a single-element array to allow modifications *)
	| CPWrapRef
	(** similar to wrap ref, but will only apply to the locals that are declared in loops *)
	| CPLoopVars

type platform_config = {
	(** has a static type system, with not-nullable basic types (Int/Float/Bool) *)
	pf_static : bool;
	(** has access to the "sys" package *)
	pf_sys : bool;
	(** captured variables handling (see before) *)
	pf_capture_policy : capture_policy;
	(** when calling a method with optional args, do we replace the missing args with "null" constants *)
	pf_pad_nulls : bool;
	(** add a final return to methods not having one already - prevent some compiler warnings *)
	pf_add_final_return : bool;
	(** does the platform natively support overloaded functions *)
	pf_overload : bool;
	(** can the platform use default values for non-nullable arguments *)
	pf_can_skip_non_nullable_argument : bool;
	(** type paths that are reserved on the platform *)
	pf_reserved_type_paths : path list;
}

module DisplayMode = struct
	type t =
		| DMNone
		| DMField
		| DMUsage of bool (* true = also report definition *)
		| DMPosition
		| DMToplevel
		| DMResolve of string
		| DMPackage
		| DMType
		| DMModuleSymbols of string option
		| DMDiagnostics of bool (* true = global, false = only in display file *)
		| DMStatistics
		| DMSignature

	type error_policy =
		| EPIgnore
		| EPCollect
		| EPShow

	type display_file_policy =
		| DFPOnly
		| DFPAlso
		| DFPNo

	type settings = {
		dms_kind : t;
		dms_display : bool;
		dms_full_typing : bool;
		dms_force_macro_typing : bool;
		dms_error_policy : error_policy;
		dms_collect_data : bool;
		dms_check_core_api : bool;
		dms_inline : bool;
		dms_display_file_policy : display_file_policy;
		dms_exit_during_typing : bool;
	}

	let default_display_settings = {
		dms_kind = DMField;
		dms_display = true;
		dms_full_typing = false;
		dms_force_macro_typing = false;
		dms_error_policy = EPIgnore;
		dms_collect_data = false;
		dms_check_core_api = false;
		dms_inline = false;
		dms_display_file_policy = DFPOnly;
		dms_exit_during_typing = true;
	}

	let default_compilation_settings = {
		dms_kind = DMNone;
		dms_display = false;
		dms_full_typing = true;
		dms_force_macro_typing = true;
		dms_error_policy = EPShow;
		dms_collect_data = false;
		dms_check_core_api = true;
		dms_inline = true;
		dms_display_file_policy = DFPNo;
		dms_exit_during_typing = false;
	}

	let create dm =
		let settings = { default_display_settings with dms_kind = dm } in
		match dm with
		| DMNone -> default_compilation_settings
		| DMField | DMPosition | DMResolve _ | DMPackage | DMType | DMSignature -> settings
		| DMUsage _ -> { settings with
				dms_full_typing = true;
				dms_collect_data = true;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false
			}
		| DMToplevel -> { settings with dms_full_typing = true; }
		| DMModuleSymbols filter -> { settings with
				dms_display_file_policy = if filter = None then DFPOnly else DFPNo;
				dms_exit_during_typing = false;
				dms_force_macro_typing = false;
			}
		| DMDiagnostics global -> { settings with
				dms_full_typing = true;
				dms_error_policy = EPCollect;
				dms_collect_data = true;
				dms_inline = true;
				dms_display_file_policy = if global then DFPNo else DFPAlso;
				dms_exit_during_typing = false;
			}
		| DMStatistics -> { settings with
				dms_full_typing = true;
				dms_collect_data = true;
				dms_inline = false;
				dms_display_file_policy = DFPAlso;
				dms_exit_during_typing = false
			}

	let to_string = function
		| DMNone -> "none"
		| DMField -> "field"
		| DMPosition -> "position"
		| DMResolve s -> "resolve " ^ s
		| DMPackage -> "package"
		| DMType -> "type"
		| DMUsage true -> "rename"
		| DMUsage false -> "references"
		| DMToplevel -> "toplevel"
		| DMModuleSymbols None -> "module-symbols"
		| DMModuleSymbols (Some s) -> "workspace-symbols " ^ s
		| DMDiagnostics b -> (if b then "global " else "") ^ "diagnostics"
		| DMStatistics -> "statistics"
		| DMSignature -> "signature"
end

type compiler_callback = {
	mutable after_typing : (module_type list -> unit) list;
	mutable before_dce : (unit -> unit) list;
	mutable after_generation : (unit -> unit) list;
}

module IdentifierType = struct
	type resolution_mode =
		| RMLocalModule
		| RMImport
		| RMUsing
		| RMTypeParameter
		| RMClassPath
		| RMOtherModule of path

	type t =
		| ITLocal of tvar
		| ITMember of tclass_field
		| ITStatic of tclass_field
		| ITEnum of tenum * tenum_field
		| ITEnumAbstract of tabstract * tclass_field
		| ITGlobal of module_type * string * Type.t
		| ITType of module_type * resolution_mode
		| ITPackage of string
		| ITLiteral of string
		| ITTimer of string

	let get_name = function
		| ITLocal v -> v.v_name
		| ITMember cf | ITStatic cf | ITEnumAbstract(_,cf) -> cf.cf_name
		| ITEnum(_,ef) -> ef.ef_name
		| ITGlobal(_,s,_) -> s
		| ITType(mt,_) -> snd (t_infos mt).mt_path
		| ITPackage s -> s
		| ITLiteral s -> s
		| ITTimer s -> s

end

type shared_display_information = {
	mutable import_positions : (pos,bool ref * placed_name list) PMap.t;
	mutable diagnostics_messages : (string * pos * DisplayTypes.DiagnosticsSeverity.t) list;
	mutable type_hints : (pos,Type.t) Hashtbl.t;
	mutable document_symbols : (string * DisplayTypes.SymbolInformation.t DynArray.t) list;
	mutable removable_code : (string * pos * pos) list;
}

type display_information = {
	mutable unresolved_identifiers : (string * pos * (string * IdentifierType.t) list) list;
	mutable interface_field_implementations : (tclass * tclass_field * tclass * tclass_field option) list;
}

(* This information is shared between normal and macro context. *)
type shared_context = {
	shared_display_information : shared_display_information;
}

type context = {
	(* config *)
	version : int;
	args : string list;
	shared : shared_context;
	display_information : display_information;
	mutable sys_args : string list;
	mutable display : DisplayMode.settings;
	mutable debug : bool;
	mutable verbose : bool;
	mutable foptimize : bool;
	mutable platform : platform;
	mutable config : platform_config;
	mutable std_path : string list;
	mutable class_path : string list;
	mutable main_class : Type.path option;
	mutable package_rules : (string,package_rule) PMap.t;
	mutable error : string -> pos -> unit;
	mutable warning : string -> pos -> unit;
	mutable load_extern_type : (path -> pos -> (string * Ast.package) option) list; (* allow finding types which are not in sources *)
	callbacks : compiler_callback;
	defines : Define.define;
	mutable print : string -> unit;
	mutable get_macros : unit -> context option;
	mutable run_command : string -> int;
	file_lookup_cache : (string,string option) Hashtbl.t;
	parser_cache : (string,(type_def * pos) list) Hashtbl.t;
	cached_macros : (path * string,((string * bool * t) list * t * tclass * Type.tclass_field)) Hashtbl.t;
	mutable stored_typed_exprs : (int, texpr) PMap.t;
	(* output *)
	mutable file : string;
	mutable flash_version : float;
	mutable features : (string,bool) Hashtbl.t;
	mutable modules : Type.module_def list;
	mutable main : Type.texpr option;
	mutable types : Type.module_type list;
	mutable resources : (string,string) Hashtbl.t;
	mutable neko_libs : string list;
	mutable include_files : (string * string) list;
	mutable swf_libs : (string * (unit -> Swf.swf) * (unit -> ((string list * string),As3hl.hl_class) Hashtbl.t)) list;
	mutable java_libs : (string * bool * (unit -> unit) * (unit -> (path list)) * (path -> ((JData.jclass * string * string) option))) list; (* (path,std,close,all_files,lookup) *)
	mutable net_libs : (string * bool * (unit -> path list) * (path -> IlData.ilclass option)) list; (* (path,std,all_files,lookup) *)
	mutable net_std : string list;
	net_path_map : (path,string list * string list * string) Hashtbl.t;
	mutable c_args : string list;
	mutable js_gen : (unit -> unit) option;
	(* typing *)
	mutable basic : basic_types;
	memory_marker : float array;
}

exception Abort of string * pos

let display_default = ref DisplayMode.DMNone

module CompilationServer = struct
	type cache = {
		c_haxelib : (string list, string list) Hashtbl.t;
		c_files : ((string * string), float * Ast.package) Hashtbl.t;
		c_modules : (path * string, module_def) Hashtbl.t;
		c_directories : (string, (string * float ref) list) Hashtbl.t;
	}

	type t = {
		cache : cache;
		mutable signs : (string * string) list;
		mutable initialized : bool;
	}

	type context_options =
		| NormalContext
		| MacroContext
		| NormalAndMacroContext

	let instance : t option ref = ref None

	let create_cache () = {
		c_haxelib = Hashtbl.create 0;
		c_files = Hashtbl.create 0;
		c_modules = Hashtbl.create 0;
		c_directories = Hashtbl.create 0;
	}

	let create () =
		let cs = {
			cache = create_cache();
			signs = [];
			initialized = false;
		} in
		instance := Some cs;
		cs

	let get () =
		!instance

	let runs () =
		!instance <> None

	let is_initialized cs =
		cs.initialized = true

	let set_initialized cs =
		cs.initialized <- true

	let get_context_files cs signs =
		Hashtbl.fold (fun (file,sign) (_,data) acc ->
			if (List.mem sign signs) then (file,data) :: acc
			else acc
		) cs.cache.c_files []

	(* signatures *)

	let get_sign cs sign =
		List.assoc sign cs.signs

	let add_sign cs sign =
		let i = string_of_int (List.length cs.signs) in
		cs.signs <- (sign,i) :: cs.signs;
		i

	(* modules *)

	let find_module cs key =
		Hashtbl.find cs.cache.c_modules key

	let cache_module cs key value =
		Hashtbl.replace cs.cache.c_modules key value

	let taint_modules cs file =
		Hashtbl.iter (fun _ m -> if m.m_extra.m_file = file then m.m_extra.m_dirty <- Some m) cs.cache.c_modules

	let iter_modules cs f =
		Hashtbl.iter (fun _ m -> f m) cs.cache.c_modules

	(* files *)

	let find_file cs key =
		Hashtbl.find cs.cache.c_files key

	let cache_file cs key value =
		Hashtbl.replace cs.cache.c_files key value

	let remove_file cs key =
		Hashtbl.remove cs.cache.c_files key

	let remove_files cs file =
		List.iter (fun (sign,_) -> remove_file cs (sign,file)) cs.signs

	(* haxelibs *)

	let find_haxelib cs key =
		Hashtbl.find cs.cache.c_haxelib key

	let cache_haxelib cs key value =
		Hashtbl.replace cs.cache.c_haxelib key value

	(* directories *)

	let find_directories cs key =
		Hashtbl.find cs.cache.c_directories key

	let add_directories cs key value =
		Hashtbl.replace cs.cache.c_directories key value

	let remove_directory cs key value =
		try
			let current = find_directories cs key in
			Hashtbl.replace cs.cache.c_directories key (List.filter (fun (s,_) -> s <> value) current);
		with Not_found ->
			()

	let has_directory cs key value =
		try
			List.mem_assoc value (find_directories cs key)
		with Not_found ->
			false

	let add_directory cs key value =
		try
			let current = find_directories cs key in
			add_directories cs key (value :: current)
		with Not_found ->
			add_directories cs key [value]

	let clear_directories cs key =
		Hashtbl.remove cs.cache.c_directories key
end

(* Defines *)

module Define = Define

let defined com s =
	Define.defined com.defines s

let raw_defined com v =
	Define.raw_defined com.defines v

let defined_value com v =
	Define.defined_value com.defines v

let defined_value_safe ?default com v =
	match default with
		| Some s -> Define.defined_value_safe ~default:s com.defines v
		| None -> Define.defined_value_safe com.defines v

let define com v =
	Define.define com.defines v

let raw_define com v =
	Define.raw_define com.defines v

let define_value com k v =
	Define.define_value com.defines k v

let raw_defined_value com k =
	Define.raw_defined_value com.defines k

let short_platform_name = function
	| Cross -> "x"
	| Js -> "js"
	| Lua -> "lua"
	| Neko -> "n"
	| Flash -> "swf"
	| Php -> "php"
	| Cpp -> "cpp"
	| Cs -> "cs"
	| Java -> "jav"
	| Python -> "py"
	| Hl -> "hl"
	| Eval -> "evl"

let stats =
	{
		s_files_parsed = ref 0;
		s_classes_built = ref 0;
		s_methods_typed = ref 0;
		s_macros_called = ref 0;
	}

let default_config =
	{
		pf_static = true;
		pf_sys = true;
		pf_capture_policy = CPNone;
		pf_pad_nulls = false;
		pf_add_final_return = false;
		pf_overload = false;
		pf_can_skip_non_nullable_argument = true;
		pf_reserved_type_paths = [];
	}

let get_config com =
	let defined f = PMap.mem (fst (Define.infos f)) com.defines.values in
	match com.platform with
	| Cross ->
		default_config
	| Js ->
		{
			default_config with
			pf_static = false;
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_reserved_type_paths = [([],"Object");([],"Error")];
		}
	| Lua ->
		{
			default_config with
			pf_static = false;
			pf_capture_policy = CPLoopVars;
		}
	| Neko ->
		{
			default_config with
			pf_static = false;
			pf_pad_nulls = true;
		}
	| Flash when defined Define.As3 ->
		{
			default_config with
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_add_final_return = true;
			pf_can_skip_non_nullable_argument = false;
		}
	| Flash ->
		{
			default_config with
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_can_skip_non_nullable_argument = false;
			pf_reserved_type_paths = [([],"Object");([],"Error")];
		}
	| Php ->
		{
			default_config with
			pf_static = false;
		}
	| Cpp ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_add_final_return = true;
		}
	| Cs ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_overload = true;
		}
	| Java ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_overload = true;
		}
	| Python ->
		{
			default_config with
			pf_static = false;
			pf_capture_policy = CPLoopVars;
		}
	| Hl ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_can_skip_non_nullable_argument = false;
		}
	| Eval ->
		{
			default_config with
			pf_static = false;
			pf_pad_nulls = true;
		}

let memory_marker = [|Unix.time()|]

let create_callbacks () =
	{
		after_typing = [];
		before_dce = [];
		after_generation = [];
	}

let create version s_version args =
	let m = Type.mk_mono() in
	let defines =
		PMap.add "true" "1" (
		PMap.add "source-header" ("Generated by Haxe " ^ s_version) (
		if !display_default <> DisplayMode.DMNone then PMap.add "display" "1" PMap.empty else PMap.empty))
	in
	{
		version = version;
		args = args;
		shared = {
			shared_display_information = {
				import_positions = PMap.empty;
				diagnostics_messages = [];
				type_hints = Hashtbl.create 0;
				document_symbols = [];
				removable_code = [];
			}
		};
		display_information = {
			unresolved_identifiers = [];
			interface_field_implementations = [];
		};
		sys_args = [];
		debug = false;
		display = DisplayMode.create !display_default;
		verbose = false;
		foptimize = true;
		features = Hashtbl.create 0;
		platform = Cross;
		config = default_config;
		print = (fun s -> print_string s; flush stdout);
		run_command = Sys.command;
		std_path = [];
		class_path = [];
		main_class = None;
		package_rules = PMap.empty;
		file = "";
		types = [];
		callbacks = create_callbacks();
		modules = [];
		main = None;
		flash_version = 10.;
		resources = Hashtbl.create 0;
		swf_libs = [];
		java_libs = [];
		net_libs = [];
		net_std = [];
		net_path_map = Hashtbl.create 0;
		c_args = [];
		neko_libs = [];
		include_files = [];
		js_gen = None;
		load_extern_type = [];
		defines = {
			defines_signature = None;
			values = defines;
		};
		get_macros = (fun() -> None);
		warning = (fun _ _ -> assert false);
		error = (fun _ _ -> assert false);
		basic = {
			tvoid = m;
			tint = m;
			tfloat = m;
			tbool = m;
			tnull = (fun _ -> assert false);
			tstring = m;
			tarray = (fun _ -> assert false);
		};
		file_lookup_cache = Hashtbl.create 0;
		stored_typed_exprs = PMap.empty;
		cached_macros = Hashtbl.create 0;
		memory_marker = memory_marker;
		parser_cache = Hashtbl.create 0;
	}

let log com str =
	if com.verbose then com.print (str ^ "\n")

let clone com =
	let t = com.basic in
	{ com with
		basic = { t with tvoid = t.tvoid };
		main_class = None;
		features = Hashtbl.create 0;
		file_lookup_cache = Hashtbl.create 0;
		parser_cache = Hashtbl.create 0 ;
		callbacks = create_callbacks();
		display_information = {
			unresolved_identifiers = [];
			interface_field_implementations = [];
		};
		defines = {
			values = com.defines.values;
			defines_signature = com.defines.defines_signature;
		}
	}

let file_time file = Extc.filetime file

let file_extension file =
	match List.rev (ExtString.String.nsplit file ".") with
	| e :: _ -> String.lowercase e
	| [] -> ""

let flash_versions = List.map (fun v ->
	let maj = int_of_float v in
	let min = int_of_float (mod_float (v *. 10.) 10.) in
	v, string_of_int maj ^ (if min = 0 then "" else "_" ^ string_of_int min)
) [9.;10.;10.1;10.2;10.3;11.;11.1;11.2;11.3;11.4;11.5;11.6;11.7;11.8;11.9;12.0;13.0;14.0;15.0;16.0;17.0]

let flash_version_tag = function
	| 6. -> 6
	| 7. -> 7
	| 8. -> 8
	| 9. -> 9
	| 10. | 10.1 -> 10
	| 10.2 -> 11
	| 10.3 -> 12
	| 11. -> 13
	| 11.1 -> 14
	| 11.2 -> 15
	| 11.3 -> 16
	| 11.4 -> 17
	| 11.5 -> 18
	| 11.6 -> 19
	| 11.7 -> 20
	| 11.8 -> 21
	| 11.9 -> 22
	| v when v >= 12.0 && float_of_int (int_of_float v) = v -> int_of_float v + 11
	| v -> failwith ("Invalid SWF version " ^ string_of_float v)

let init_platform com pf =
	com.platform <- pf;
	let name = platform_name pf in
	let forbid acc p = if p = name || PMap.mem p acc then acc else PMap.add p Forbidden acc in
	com.package_rules <- List.fold_left forbid com.package_rules (List.map platform_name platforms);
	com.config <- get_config com;
	if com.config.pf_static then define com Define.Static;
	if com.config.pf_sys then define com Define.Sys else com.package_rules <- PMap.add "sys" Forbidden com.package_rules;
	raw_define com name

let add_feature com f =
	Hashtbl.replace com.features f true

let has_dce com =
	(try defined_value com Define.Dce <> "no" with Not_found -> false)

(*
	TODO: The has_dce check is there because we mark types with @:directlyUsed in the DCE filter,
	which is not run in dce=no and thus we can't know if a type is used directly or not,
	so we just assume that they are.

	If we had dce filter always running (even with dce=no), we would have types marked with @:directlyUsed
	and we wouldn't need to generate unnecessary imports in dce=no, but that's good enough for now.
*)
let is_directly_used com meta =
	not (has_dce com) || Meta.has Meta.DirectlyUsed meta

let rec has_feature com f =
	try
		Hashtbl.find com.features f
	with Not_found ->
		if com.types = [] then not (has_dce com) else
		match List.rev (ExtString.String.nsplit f ".") with
		| [] -> assert false
		| [cl] -> has_feature com (cl ^ ".*")
		| meth :: cl :: pack ->
			let r = (try
				let path = List.rev pack, cl in
				(match List.find (fun t -> t_path t = path && not (Meta.has Meta.RealPath (t_infos t).mt_meta)) com.types with
				| t when meth = "*" -> (match t with TAbstractDecl a -> Meta.has Meta.ValueUsed a.a_meta | _ ->
					Meta.has Meta.Used (t_infos t).mt_meta)
				| TClassDecl ({cl_extern = true} as c) when com.platform <> Js || cl <> "Array" && cl <> "Math" ->
					Meta.has Meta.Used (try PMap.find meth c.cl_statics with Not_found -> PMap.find meth c.cl_fields).cf_meta
				| TClassDecl c ->
					PMap.exists meth c.cl_statics || PMap.exists meth c.cl_fields
				| _ ->
					false)
			with Not_found ->
				false
			) in
			let r = r || not (has_dce com) in
			Hashtbl.add com.features f r;
			r

let allow_package ctx s =
	try
		if (PMap.find s ctx.package_rules) = Forbidden then ctx.package_rules <- PMap.remove s ctx.package_rules
	with Not_found ->
		()

let abort msg p = raise (Abort (msg,p))

let platform ctx p = ctx.platform = p

let add_typing_filter ctx f =
	ctx.callbacks.after_typing <- f :: ctx.callbacks.after_typing

let add_filter ctx f =
	ctx.callbacks.before_dce <- f :: ctx.callbacks.before_dce

let add_final_filter ctx f =
	ctx.callbacks.after_generation <- f :: ctx.callbacks.after_generation

let find_file ctx f =
	try
		(match Hashtbl.find ctx.file_lookup_cache f with
		| None -> raise Exit
		| Some f -> f)
	with Exit ->
		raise Not_found
	| Not_found ->
		let rec loop had_empty = function
			| [] when had_empty -> raise Not_found
			| [] -> loop true [""]
			| p :: l ->
				let file = p ^ f in
				if Sys.file_exists file then begin
					(try
						let ext = String.rindex file '.' in
						let file_pf = String.sub file 0 (ext + 1) ^ platform_name ctx.platform ^ String.sub file ext (String.length file - ext) in
						if not (defined ctx Define.CoreApi) && Sys.file_exists file_pf then file_pf else file
					with Not_found ->
						file)
				end else
					loop (had_empty || p = "") l
		in
		let r = (try Some (loop false ctx.class_path) with Not_found -> None) in
		Hashtbl.add ctx.file_lookup_cache f r;
		(match r with
		| None -> raise Not_found
		| Some f -> f)


let mem_size v =
	Objsize.size_with_headers (Objsize.objsize v [] [])

let hash f =
	let h = ref 0 in
	for i = 0 to String.length f - 1 do
		h := !h * 223 + int_of_char (String.unsafe_get f i);
	done;
	if Sys.word_size = 64 then Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int !h) 1) 1) else !h

let url_encode s add_char =
	let hex = "0123456789ABCDEF" in
	for i = 0 to String.length s - 1 do
		let c = String.unsafe_get s i in
		match c with
		| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '-' | '.' ->
			add_char c
		| _ ->
			add_char '%';
			add_char (String.unsafe_get hex (int_of_char c lsr 4));
			add_char (String.unsafe_get hex (int_of_char c land 0xF));
	done

let url_encode_s s =
	let b = Buffer.create 0 in
	url_encode s (Buffer.add_char b);
	Buffer.contents b

let add_diagnostics_message com s p sev =
	let di = com.shared.shared_display_information in
	di.diagnostics_messages <- (s,p,sev) :: di.diagnostics_messages

open Printer

let dump_context com = s_record_fields "" [
	"version",string_of_int com.version;
	"args",s_list ", " (fun s -> s) com.args;
	"debug",string_of_bool com.debug;
	"platform",platform_name com.platform;
	"std_path",s_list ", " (fun s -> s) com.std_path;
	"class_path",s_list ", " (fun s -> s) com.class_path;
	"defines",s_pmap (fun s -> s) (fun s -> s) com.defines.values;
	"defines_signature",s_opt (fun s -> Digest.to_hex s) com.defines.defines_signature;
]
