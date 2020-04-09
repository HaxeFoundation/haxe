(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

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
open CompilationServer
open Type
open Globals
open Define
open NativeLibraries

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

type exceptions_config = {
	(* Base types which may be thrown from Haxe code without wrapping. *)
	ec_native_throws : path list;
	(* Base types which may be caught from Haxe code without wrapping. *)
	ec_native_catches : path list;
	(* Path of a native class or interface, which can be used for wildcard catches. *)
	ec_wildcard_catch : path;
	(*
		Path of a native base class or interface, which can be thrown.
		This type is used to cast `haxe.Exception.thrown(v)` calls to.
		For example `throw 123` is compiled to `throw (cast Exception.thrown(123):ec_base_throw)`
	*)
	ec_base_throw : path;
}

type nested_function_scoping =
	(** each TFunction has its own scope in the output **)
	| Independent
	(** TFunction nodes are nested in the output **)
	| Nested
	(** TFunction nodes are nested in the output and there is var hoisting **)
	| Hoisted

type var_scoping_flags =
	(**
		Local vars cannot have the same name as the current top-level package or
		(if in root package) current class name
	*)
	| ReserveCurrentTopLevelSymbol
	(**
		Local vars cannot have a name used for any top-level symbol
		(packages and classes in the root package)
	*)
	| ReserveAllTopLevelSymbols
	(**
		Reserve all type paths converted to "flat path" with `Path.flat_path`
	*)
	| ReserveAllTypesFlat
	(**
		List of names cannot be taken by local vars
	*)
	| ReserveNames of string list

type var_scoping = {
	vs_flags : var_scoping_flags list;
}

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
	(** supports function == function **)
	pf_supports_function_equality : bool;
	(** uses utf16 encoding with ucs2 api **)
	pf_uses_utf16 : bool;
	(** target supports accessing `this` before calling `super(...)` **)
	pf_this_before_super : bool;
	(** target supports threads **)
	pf_supports_threads : bool;
	(** target supports Unicode **)
	pf_supports_unicode : bool;
	(** exceptions handling config **)
	pf_exceptions : exceptions_config;
	(** the scoping behavior of nested functions **)
	pf_nested_function_scoping : nested_function_scoping;
	(** the scoping of local variables *)
	pf_scoping : var_scoping;
}

class compiler_callbacks = object(self)
	val mutable before_typer_create = [];
	val mutable after_init_macros = [];
	val mutable after_typing = [];
	val mutable before_save = [];
	val mutable after_save = [];
	val mutable after_filters = [];
	val mutable after_generation = [];
	val mutable null_safety_report = [];

	method add_before_typer_create (f : unit -> unit) : unit =
		before_typer_create <- f :: before_typer_create

	method add_after_init_macros (f : unit -> unit) : unit =
		after_init_macros <- f :: after_init_macros

	method add_after_typing (f : module_type list -> unit) : unit =
		after_typing <- f :: after_typing

	method add_before_save (f : unit -> unit) : unit =
		before_save <- f :: before_save

	method add_after_save (f : unit -> unit) : unit =
		after_save <- f :: after_save

	method add_after_filters (f : unit -> unit) : unit =
		after_filters <- f :: after_filters

	method add_after_generation (f : unit -> unit) : unit =
		after_generation <- f :: after_generation

	method add_null_safety_report (f : (string*pos) list -> unit) : unit =
		null_safety_report <- f :: null_safety_report

	method get_before_typer_create = before_typer_create
	method get_after_init_macros = after_init_macros
	method get_after_typing = after_typing
	method get_before_save = before_save
	method get_after_save = after_save
	method get_after_filters = after_filters
	method get_after_generation = after_generation
	method get_null_safety_report = null_safety_report
end

type shared_display_information = {
	mutable diagnostics_messages : (string * pos * DisplayTypes.DiagnosticsKind.t * DisplayTypes.DiagnosticsSeverity.t) list;
}

type display_information = {
	mutable unresolved_identifiers : (string * pos * (string * CompletionItem.t * int) list) list;
	mutable display_module_has_macro_defines : bool;
}

(* This information is shared between normal and macro context. *)
type shared_context = {
	shared_display_information : shared_display_information;
}

type json_api = {
	send_result : Json.t -> unit;
	send_error : Json.t list -> unit;
	jsonrpc : Jsonrpc_handler.jsonrpc_handler;
}

type compiler_stage =
	| CCreated          (* Context was just created *)
	| CInitialized      (* Context was initialized (from CLI args and such). *)
	| CTyperCreated     (* The typer context was just created. *)
	| CInitMacrosStart  (* Init macros are about to run. *)
	| CInitMacrosDone   (* Init macros did run - at this point the signature is locked. *)
	| CTypingDone       (* The typer is done - at this point com.types/modules/main is filled. *)
	| CFilteringStart   (* Filtering just started (nothing changed yet). *)
	| CAnalyzerStart    (* Some filters did run, the analyzer is about to run. *)
	| CAnalyzerDone     (* The analyzer just finished. *)
	| CSaveStart        (* The type state is about to be saved. *)
	| CSaveDone         (* The type state has been saved - at this point we can destroy things. *)
	| CDceStart         (* DCE is about to run - everything is still available. *)
	| CDceDone          (* DCE just finished. *)
	| CFilteringDone    (* Filtering just finished. *)
	| CGenerationStart  (* Generation is about to begin. *)
	| CGenerationDone   (* Generation just finished. *)

type context = {
	mutable stage : compiler_stage;
	mutable cache : context_cache option;
	(* config *)
	version : int;
	args : string list;
	shared : shared_context;
	display_information : display_information;
	mutable sys_args : string list;
	mutable display : DisplayTypes.DisplayMode.settings;
	mutable debug : bool;
	mutable verbose : bool;
	mutable foptimize : bool;
	mutable platform : platform;
	mutable config : platform_config;
	mutable std_path : string list;
	mutable class_path : string list;
	mutable main_class : path option;
	mutable package_rules : (string,package_rule) PMap.t;
	mutable error : string -> pos -> unit;
	mutable info : string -> pos -> unit;
	mutable warning : string -> pos -> unit;
	mutable get_messages : unit -> compiler_message list;
	mutable filter_messages : (compiler_message -> bool) -> unit;
	mutable load_extern_type : (string * (path -> pos -> Ast.package option)) list; (* allow finding types which are not in sources *)
	callbacks : compiler_callbacks;
	defines : Define.define;
	mutable print : string -> unit;
	mutable get_macros : unit -> context option;
	mutable run_command : string -> int;
	file_lookup_cache : (string,string option) Hashtbl.t;
	readdir_cache : (string * string,(string array) option) Hashtbl.t;
	parser_cache : (string,(type_def * pos) list) Hashtbl.t;
	module_to_file : (path,string) Hashtbl.t;
	cached_macros : (path * string,(((string * bool * t) list * t * tclass * Type.tclass_field) * module_def)) Hashtbl.t;
	mutable stored_typed_exprs : (int, texpr) PMap.t;
	pass_debug_messages : string DynArray.t;
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
	mutable native_libs : native_libraries;
	mutable net_std : string list;
	net_path_map : (path,string list * string list * string) Hashtbl.t;
	mutable c_args : string list;
	mutable js_gen : (unit -> unit) option;
	mutable json_out : json_api option;
	(* typing *)
	mutable basic : basic_types;
	memory_marker : float array;
}

exception Abort of string * pos

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

let get_es_version com =
	try int_of_string (defined_value com Define.JsEs) with _ -> 0

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
		pf_supports_function_equality = true;
		pf_uses_utf16 = true;
		pf_this_before_super = true;
		pf_supports_threads = false;
		pf_supports_unicode = true;
		pf_exceptions = {
			ec_native_throws = [];
			ec_native_catches = [];
			ec_wildcard_catch = (["StdTypes"],"Dynamic");
			ec_base_throw = (["StdTypes"],"Dynamic");
		};
		pf_nested_function_scoping = Independent;
		pf_scoping = {
			vs_flags = [];
		}
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
			pf_this_before_super = (get_es_version com) < 6; (* cannot access `this` before `super()` when generating ES6 classes *)
			pf_exceptions = { default_config.pf_exceptions with
				ec_native_throws = [
					["js";"lib"],"Error";
					["haxe"],"Exception";
				];
			};
			pf_nested_function_scoping = Hoisted;
			pf_scoping = {
				vs_flags = [if defined Define.JsUnflatten then ReserveAllTopLevelSymbols else ReserveAllTypesFlat];
			}
		}
	| Lua ->
		{
			default_config with
			pf_static = false;
			pf_capture_policy = CPLoopVars;
			pf_uses_utf16 = false;
		}
	| Neko ->
		{
			default_config with
			pf_static = false;
			pf_pad_nulls = true;
			pf_uses_utf16 = false;
			pf_supports_threads = true;
			pf_supports_unicode = false;
		}
	| Flash ->
		{
			default_config with
			pf_sys = false;
			pf_capture_policy = CPLoopVars;
			pf_can_skip_non_nullable_argument = false;
			pf_reserved_type_paths = [([],"Object");([],"Error")];
			pf_exceptions = { default_config.pf_exceptions with
				ec_native_throws = [
					["flash";"errors"],"Error";
					["haxe"],"Exception";
				];
				ec_native_catches = [
					["flash";"errors"],"Error";
					["haxe"],"Exception";
				];
			}
		}
	| Php ->
		{
			default_config with
			pf_static = false;
			pf_uses_utf16 = false;
			pf_exceptions = {
				ec_native_throws = [
					["php"],"Throwable";
					["haxe"],"Exception";
				];
				ec_native_catches = [
					["php"],"Throwable";
					["haxe"],"Exception";
				];
				ec_wildcard_catch = (["php"],"Throwable");
				ec_base_throw = (["php"],"Throwable");
			};
			pf_nested_function_scoping = Nested;
		}
	| Cpp ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_add_final_return = true;
			pf_supports_threads = true;
			pf_supports_unicode = (defined Define.Cppia) || not (defined Define.DisableUnicodeStrings);
		}
	| Cs ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_overload = true;
			pf_supports_threads = true;
			pf_exceptions = {
				ec_native_throws = [
					["cs";"system"],"Exception";
					["haxe"],"Exception";
				];
				ec_native_catches = [
					["cs";"system"],"Exception";
					["haxe"],"Exception";
				];
				ec_wildcard_catch = (["cs";"system"],"Exception");
				ec_base_throw = (["cs";"system"],"Exception");
			}
		}
	| Java ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_overload = true;
			pf_supports_threads = true;
			pf_this_before_super = false;
			pf_exceptions = {
				ec_native_throws = [
					["java";"lang"],"RuntimeException";
					["haxe"],"Exception";
				];
				ec_native_catches = [
					["java";"lang"],"Throwable";
					["haxe"],"Exception";
				];
				ec_wildcard_catch = (["java";"lang"],"Throwable");
				ec_base_throw = (["java";"lang"],"RuntimeException");
			};
			pf_scoping = {
				vs_flags = [ReserveNames(["_"])];
			}
		}
	| Python ->
		{
			default_config with
			pf_static = false;
			pf_capture_policy = CPLoopVars;
			pf_uses_utf16 = false;
			pf_exceptions = {
				ec_native_throws = [
					["python";"Exceptions"],"BaseException";
				];
				ec_native_catches = [
					["python";"Exceptions"],"BaseException";
				];
				ec_wildcard_catch = ["python";"Exceptions"],"BaseException";
				ec_base_throw = ["python";"Exceptions"],"BaseException";
			};
			pf_nested_function_scoping = Nested;
		}
	| Hl ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_supports_threads = true;
			pf_exceptions = { default_config.pf_exceptions with
				ec_native_throws = [
					["haxe"],"Exception";
				];
				ec_native_catches = [
					["haxe"],"Exception";
				];
			}
		}
	| Eval ->
		{
			default_config with
			pf_static = false;
			pf_pad_nulls = true;
			pf_uses_utf16 = false;
			pf_supports_threads = true;
			pf_capture_policy = CPWrapRef;
		}

let memory_marker = [|Unix.time()|]

let create version s_version args =
	let m = Type.mk_mono() in
	let defines =
		PMap.add "true" "1" (
			PMap.add "source-header" ("Generated by Haxe " ^ s_version) PMap.empty
		)
	in
	{
		cache = None;
		stage = CCreated;
		version = version;
		args = args;
		shared = {
			shared_display_information = {
				diagnostics_messages = [];
			}
		};
		display_information = {
			unresolved_identifiers = [];
			display_module_has_macro_defines = false;
		};
		sys_args = args;
		debug = false;
		display = DisplayTypes.DisplayMode.create !Parser.display_mode;
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
		callbacks = new compiler_callbacks;
		modules = [];
		main = None;
		flash_version = 10.;
		resources = Hashtbl.create 0;
		net_std = [];
		native_libs = create_native_libs();
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
		info = (fun _ _ -> assert false);
		warning = (fun _ _ -> assert false);
		error = (fun _ _ -> assert false);
		get_messages = (fun() -> []);
		filter_messages = (fun _ -> ());
		pass_debug_messages = DynArray.create();
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
		readdir_cache = Hashtbl.create 0;
		module_to_file = Hashtbl.create 0;
		stored_typed_exprs = PMap.empty;
		cached_macros = Hashtbl.create 0;
		memory_marker = memory_marker;
		parser_cache = Hashtbl.create 0;
		json_out = None;
	}

let log com str =
	if com.verbose then com.print (str ^ "\n")

let clone com =
	let t = com.basic in
	{ com with
		cache = None;
		basic = { t with tvoid = t.tvoid };
		main_class = None;
		features = Hashtbl.create 0;
		file_lookup_cache = Hashtbl.create 0;
		readdir_cache = Hashtbl.create 0;
		parser_cache = Hashtbl.create 0;
		module_to_file = Hashtbl.create 0;
		callbacks = new compiler_callbacks;
		display_information = {
			unresolved_identifiers = [];
			display_module_has_macro_defines = false;
		};
		defines = {
			values = com.defines.values;
			defines_signature = com.defines.defines_signature;
		};
		native_libs = create_native_libs();
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
) [9.;10.;10.1;10.2;10.3;11.;11.1;11.2;11.3;11.4;11.5;11.6;11.7;11.8;11.9;12.0;13.0;14.0;15.0;16.0;17.0;18.0;19.0;20.0;21.0;22.0;23.0;24.0;25.0;26.0;27.0;28.0;29.0;31.0;32.0]

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
	com.package_rules <- List.fold_left forbid com.package_rules ("jvm" :: (List.map platform_name platforms));
	com.config <- get_config com;
	if com.config.pf_static then begin
		raw_define_value com.defines "target.static" "true";
		define com Define.Static;
	end;
	if com.config.pf_sys then begin
		raw_define_value com.defines "target.sys" "true";
		define com Define.Sys
	end else
		com.package_rules <- PMap.add "sys" Forbidden com.package_rules;
	if com.config.pf_uses_utf16 then begin
		raw_define_value com.defines "target.utf16" "true";
		define com Define.Utf16;
	end;
	if com.config.pf_supports_threads then begin
		raw_define_value com.defines "target.threaded" "true";
	end;
	if com.config.pf_supports_unicode then begin
		raw_define_value com.defines "target.unicode" "true";
	end;
	raw_define_value com.defines "target.name" name;
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
		| field :: cl :: pack ->
			let r = (try
				let path = List.rev pack, cl in
				(match List.find (fun t -> t_path t = path && not (Meta.has Meta.RealPath (t_infos t).mt_meta)) com.types with
				| t when field = "*" ->
					not (has_dce com) ||
					(match t with TAbstractDecl a -> Meta.has Meta.ValueUsed a.a_meta | _ -> Meta.has Meta.Used (t_infos t).mt_meta)
				| TClassDecl ({cl_extern = true} as c) when com.platform <> Js || cl <> "Array" && cl <> "Math" ->
					not (has_dce com) || Meta.has Meta.Used (try PMap.find field c.cl_statics with Not_found -> PMap.find field c.cl_fields).cf_meta
				| TClassDecl c ->
					PMap.exists field c.cl_statics || PMap.exists field c.cl_fields
				| _ ->
					false)
			with Not_found ->
				false
			) in
			Hashtbl.add com.features f r;
			r

let allow_package ctx s =
	try
		if (PMap.find s ctx.package_rules) = Forbidden then ctx.package_rules <- PMap.remove s ctx.package_rules
	with Not_found ->
		()

let abort msg p = raise (Abort (msg,p))

let platform ctx p = ctx.platform = p

let platform_name_macro com =
	if defined com Define.Macro then "macro" else platform_name com.platform

let normalize_dir_separator path =
	if is_windows then String.map (fun c -> if c = '/' then '\\' else c) path
	else path

let find_file ctx f =
	try
		match Hashtbl.find ctx.file_lookup_cache f with
		| None -> raise Exit
		| Some f -> f
	with Exit ->
		raise Not_found
	| Not_found ->
		let remove_extension file =
			try String.sub file 0 (String.rindex file '.')
			with Not_found -> file
		in
		let extension file =
			try
				let dot_pos = String.rindex file '.' in
				String.sub file dot_pos (String.length file - dot_pos)
			with Not_found -> file
		in
		let f_dir = Filename.dirname f
		and platform_ext = "." ^ (platform_name_macro ctx)
		and is_core_api = defined ctx Define.CoreApi in
		let rec loop had_empty = function
			| [] when had_empty -> raise Not_found
			| [] -> loop true [""]
			| p :: l ->
				let file = p ^ f in
				let dir = Filename.dirname file in
				if Hashtbl.mem ctx.readdir_cache (p,dir) then
					loop (had_empty || p = "") l
				else begin
					let found = ref "" in
					let dir_listing =
						try Some (Sys.readdir dir);
						with Sys_error _ -> None
					in
					Hashtbl.add ctx.readdir_cache (p,dir) dir_listing;
					let normalized_f = normalize_dir_separator f in
					Option.may
						(Array.iter (fun file_name ->
							let current_f = if f_dir = "." then file_name else f_dir ^ "/" ^ file_name in
							let pf,current_f =
								if is_core_api then false,current_f
								else begin
									let ext = extension current_f in
									let pf_ext = extension (remove_extension current_f) in
									if platform_ext = pf_ext then
										true,(remove_extension (remove_extension current_f)) ^ ext
									else
										false,current_f
								end
							in
							let is_cached = Hashtbl.mem ctx.file_lookup_cache current_f in
							if is_core_api || pf || not is_cached then begin
								let full_path = if dir = "." then file_name else dir ^ "/" ^ file_name in
								if is_cached then
									Hashtbl.remove ctx.file_lookup_cache current_f;
								Hashtbl.add ctx.file_lookup_cache current_f (Some full_path);
								if normalize_dir_separator current_f = normalized_f then
									found := full_path;
							end
						))
						dir_listing;
					if !found <> "" then !found
					else loop (had_empty || p = "") l
				end
		in
		let r = (try Some (loop false ctx.class_path) with Not_found -> None) in
		Hashtbl.add ctx.file_lookup_cache f r;
		match r with
		| None -> raise Not_found
		| Some f -> f

(* let find_file ctx f =
	let timer = Timer.timer ["find_file"] in
	Std.finally timer (find_file ctx) f *)

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

(* UTF8 *)

let to_utf8 str p =
	let u8 = try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code ->
			(* ISO to utf8 *)
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UCharExt.of_char c)) str;
			UTF8.Buf.contents b
	in
	let ccount = ref 0 in
	UTF8.iter (fun c ->
		let c = UCharExt.code c in
		if (c >= 0xD800 && c <= 0xDFFF) || c >= 0x110000 then abort "Invalid unicode char" p;
		incr ccount;
		if c > 0x10000 then incr ccount;
	) u8;
	u8, !ccount

let utf16_add buf c =
	let add c =
		Buffer.add_char buf (char_of_int (c land 0xFF));
		Buffer.add_char buf (char_of_int (c lsr 8));
	in
	if c >= 0 && c < 0x10000 then begin
		if c >= 0xD800 && c <= 0xDFFF then failwith ("Invalid unicode char " ^ string_of_int c);
		add c;
	end else if c < 0x110000 then begin
		let c = c - 0x10000 in
		add ((c asr 10) + 0xD800);
		add ((c land 1023) + 0xDC00);
	end else
		failwith ("Invalid unicode char " ^ string_of_int c)

let utf8_to_utf16 str zt =
	let b = Buffer.create (String.length str * 2) in
	(try UTF8.iter (fun c -> utf16_add b (UCharExt.code c)) str with Invalid_argument _ | UCharExt.Out_of_range -> ()); (* if malformed *)
	if zt then utf16_add b 0;
	Buffer.contents b

let utf16_to_utf8 str =
	let b = Buffer.create 0 in
	let add c = Buffer.add_char b (char_of_int (c land 0xFF)) in
	let get i = int_of_char (String.unsafe_get str i) in
	let rec loop i =
		if i >= String.length str then ()
		else begin
			let c = get i in
			if c < 0x80 then begin
				add c;
				loop (i + 2);
			end else if c < 0x800 then begin
				let c = c lor ((get (i + 1)) lsl 8) in
				add c;
				add (c lsr 8);
				loop (i + 2);
			end else
				assert false;
		end
	in
	loop 0;
	Buffer.contents b

let add_diagnostics_message com s p kind sev =
	let di = com.shared.shared_display_information in
	di.diagnostics_messages <- (s,p,kind,sev) :: di.diagnostics_messages

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

let dump_path com =
	Define.defined_value_safe ~default:"dump" com.defines Define.DumpPath

let adapt_defines_to_macro_context defines =
	let values = ref defines.values in
	List.iter (fun p -> values := PMap.remove (Globals.platform_name p) !values) Globals.platforms;
	let to_remove = List.map (fun d -> fst (Define.infos d)) [Define.NoTraces] in
	let to_remove = to_remove @ List.map (fun (_,d) -> "flash" ^ d) flash_versions in
	values := PMap.foldi (fun k v acc -> if List.mem k to_remove then acc else PMap.add k v acc) !values PMap.empty;
	values := PMap.add "macro" "1" !values;
	values := PMap.add (platform_name !Globals.macro_platform) "1" !values;
	{values = !values; defines_signature = None }

let is_legacy_completion com = match com.json_out with
	| None -> true
	| Some api -> !ServerConfig.legacy_completion

let get_entry_point com =
	Option.map (fun path ->
		let m = List.find (fun m -> m.m_path = path) com.modules in
		let c = ExtList.List.find_map (fun t -> match t with TClassDecl c when c.cl_path = path -> Some c | _ -> None) m.m_types in
		let e = Option.get com.main in (* must be present at this point *)
		(snd path, c, e)
	) com.main_class
