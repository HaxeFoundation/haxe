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
	(** supports function == function **)
	pf_supports_function_equality : bool;
	(** uses utf16 encoding with ucs2 api **)
	pf_uses_utf16 : bool;
	(** target supports accessing `this` before calling `super(...)` **)
	pf_this_before_super : bool;
}

class compiler_callbacks = object(self)
	val mutable after_init_macros = [];
	val mutable after_typing = [];
	val mutable before_save = [];
	val mutable after_save = [];
	val mutable after_filters = [];
	val mutable after_generation = [];
	val mutable null_safety_report = [];

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

	method get_after_init_macros = after_init_macros
	method get_after_typing = after_typing
	method get_before_save = before_save
	method get_after_save = after_save
	method get_after_filters = after_filters
	method get_after_generation = after_generation
	method get_null_safety_report = null_safety_report
end

type shared_display_information = {
	mutable import_positions : (pos,bool ref * placed_name list) PMap.t;
	mutable diagnostics_messages : (string * pos * DisplayTypes.DiagnosticsSeverity.t) list;
}

type display_information = {
	mutable unresolved_identifiers : (string * pos * (string * CompletionItem.t * int) list) list;
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
	mutable warning : string -> pos -> unit;
	mutable load_extern_type : (path -> pos -> (string * Ast.package) option) list; (* allow finding types which are not in sources *)
	callbacks : compiler_callbacks;
	defines : Define.define;
	mutable print : string -> unit;
	mutable get_macros : unit -> context option;
	mutable run_command : string -> int;
	file_lookup_cache : (string,string option) Hashtbl.t;
	parser_cache : (string,(type_def * pos) list) Hashtbl.t;
	module_to_file : (path,string) Hashtbl.t;
	cached_macros : (path * string,(((string * bool * t) list * t * tclass * Type.tclass_field) * module_def)) Hashtbl.t;
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
	mutable json_out : ((Json.t -> unit) * (Json.t list -> unit)) option;
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
			pf_uses_utf16 = false;
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
			pf_uses_utf16 = false;
		}
	| Hl ->
		{
			default_config with
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
		}
	| Eval ->
		{
			default_config with
			pf_static = false;
			pf_pad_nulls = true;
			pf_uses_utf16 = false;
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
		version = version;
		args = args;
		shared = {
			shared_display_information = {
				import_positions = PMap.empty;
				diagnostics_messages = [];
			}
		};
		display_information = {
			unresolved_identifiers = [];
			interface_field_implementations = [];
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
		basic = { t with tvoid = t.tvoid };
		main_class = None;
		features = Hashtbl.create 0;
		file_lookup_cache = Hashtbl.create 0;
		parser_cache = Hashtbl.create 0;
		module_to_file = Hashtbl.create 0;
		callbacks = new compiler_callbacks;
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
	if com.config.pf_uses_utf16 then define com Define.Utf16;
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
						let file_pf = String.sub file 0 (ext + 1) ^ platform_name_macro ctx ^ String.sub file ext (String.length file - ext) in
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
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b
	in
	let ccount = ref 0 in
	UTF8.iter (fun c ->
		let c = UChar.code c in
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
	(try UTF8.iter (fun c -> utf16_add b (UChar.code c)) str with Invalid_argument _ | UChar.Out_of_range -> ()); (* if malformed *)
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
