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

(* Parsing of module files into modules. *)

open Globals
open Ast
open Parser
open DisplayTypes.DiagnosticsSeverity
open DisplayTypes.DisplayMode
open Common
open Type
open Typecore
open Error

exception DisplayInMacroBlock

let parse_file_from_lexbuf com file p lexbuf =
	let t = Timer.timer ["parsing"] in
	Lexer.init file;
	incr stats.s_files_parsed;
	let parse_result = try
		ParserEntry.parse com.defines lexbuf file
	with
		| Sedlexing.MalFormed ->
			t();
			error "Malformed file. Source files must be encoded with UTF-8." {pfile = file; pmin = 0; pmax = 0}
		| e ->
			t();
			raise e
	in
	begin match !Parser.display_mode,parse_result with
		| DMModuleSymbols (Some ""),_ -> ()
		| DMModuleSymbols filter,(ParseSuccess(data,_,_)) when filter = None && DisplayPosition.display_position#is_in_file file ->
			let ds = DocumentSymbols.collect_module_symbols (filter = None) data in
			DisplayException.raise_module_symbols (DocumentSymbols.Printer.print_module_symbols com [file,ds] filter);
		| _ ->
			()
	end;
	t();
	Common.log com ("Parsed " ^ file);
	parse_result

let parse_file_from_string com file p string =
	parse_file_from_lexbuf com file p (Sedlexing.Utf8.from_string string)

let current_stdin = ref None (* TODO: we're supposed to clear this at some point *)

let parse_file com file p =
	let use_stdin = (Common.defined com Define.DisplayStdin) && DisplayPosition.display_position#is_in_file file in
	if use_stdin then
		let s =
			match !current_stdin with
			| Some s ->
				s
			| None ->
				let s = Std.input_all stdin in
				close_in stdin;
				current_stdin := Some s;
				s
		in
		parse_file_from_string com file p s
	else
		let ch = try open_in_bin file with _ -> error ("Could not open " ^ file) p in
		Std.finally (fun() -> close_in ch) (parse_file_from_lexbuf com file p) (Sedlexing.Utf8.from_channel ch)

let parse_hook = ref parse_file

let resolve_module_file com m remap p =
	let forbid = ref false in
	let compose_path no_rename =
		(match m with
		| [] , name -> name
		| x :: l , name ->
			let x = (try
				match PMap.find x com.package_rules with
				| Forbidden -> forbid := true; x
				| Directory d -> if no_rename then x else d
				| Remap d -> remap := d :: l; d
				with Not_found -> x
			) in
			String.concat "/" (x :: l) ^ "/" ^ name
		) ^ ".hx"
	in
	let file = try
			Common.find_file com (compose_path false)
		with Not_found ->
			Common.find_file com (compose_path true)
	in
	let file = (match String.lowercase (snd m) with
	| "con" | "aux" | "prn" | "nul" | "com1" | "com2" | "com3" | "lpt1" | "lpt2" | "lpt3" when Sys.os_type = "Win32" ->
		(* these names are reserved by the OS - old DOS legacy, such files cannot be easily created but are reported as visible *)
		if (try (Unix.stat file).Unix.st_size with _ -> 0) > 0 then file else raise Not_found
	| _ -> file
	) in
	(* if we try to load a std.xxxx class and resolve a real std file, the package name is not valid, ignore *)
	(match fst m with
	| "std" :: _ ->
		let file = Path.unique_full_path file in
		if List.exists (fun path -> ExtString.String.starts_with file (try Path.unique_full_path path with _ -> path)) com.std_path then raise Not_found;
	| _ -> ());
	if !forbid then begin
		let parse_result = (!parse_hook) com file p in
		let rec loop decls = match decls with
			| ((EImport _,_) | (EUsing _,_)) :: decls -> loop decls
			| (EClass d,_) :: _ -> d.d_meta
			| (EEnum d,_) :: _ -> d.d_meta
			| (EAbstract d,_) :: _ -> d.d_meta
			| (ETypedef d,_) :: _ -> d.d_meta
			| [] -> []
		in
		let meta =  match parse_result with
			| ParseSuccess((_,decls),_,_) -> loop decls
			| ParseError _ -> []
		in
		if not (Meta.has Meta.NoPackageRestrict meta) then begin
			let x = (match fst m with [] -> die "" | x :: _ -> x) in
			raise (Forbid_package ((x,m,p),[],platform_name_macro com));
		end;
	end;
	file

let resolve_module_file com m remap p =
	try
		Hashtbl.find com.module_to_file m
	with Not_found ->
		let file = resolve_module_file com m remap p in
		Hashtbl.add com.module_to_file m file;
		file

(* let resolve_module_file com m remap p =
	let timer = Timer.timer ["typing";"resolve_module_file"] in
	Std.finally timer (resolve_module_file com m remap) p *)

module ConditionDisplay = struct
	open ParserEntry
	open CompletionItem.CompletionType
	open DisplayPosition

	exception Result of expr

	let t_semver = TInst(mk_class null_module ([],"SemVer") null_pos null_pos,[])

	let convert_small_type com = function
		| TNull -> "null",Type.mk_mono()
		| TBool b -> string_of_bool b,com.basic.tbool
		| TFloat f -> string_of_float f,com.basic.tfloat
		| TString s -> "\"" ^ StringHelper.s_escape s ^ "\"",com.basic.tstring
		| TVersion(r,p) -> Semver.to_string (r,p),t_semver

	let check_condition com e =
		let check_position = (if com.display.dms_kind = DMHover then
			encloses_position_gt
		else
			encloses_position
		) display_position#get in
		let tpair t = (t,CompletionItem.CompletionType.from_type (fun _ -> Imported) t) in
		let check_expr (e,p) =
			match e with
				| ECall ((EConst (Ident "version"),p),_)  ->
					let t = TFun ([("s",false,com.basic.tstring)],t_semver) in
					if check_position p then
						DisplayException.raise_hover (CompletionItem.make_ci_class_field {
							field = {
								cf_name = "version";
								cf_type = t;
								cf_pos = null_pos;
								cf_name_pos = null_pos;
								cf_doc = doc_from_string (
"Allows comparing defines (such as the version of a Haxelib or Haxe) with SemVer semantics.
Both the define and the string passed to `version()` must be valid semantic versions.

Example:
```haxe
#if (haxe >= version(\"4.0.0-rc.3\"))\n
```

Note: this syntax does not parse with Haxe versions earlier than 4.0.0-rc.3,
so it should be avoided if backwards-compatibility with earlier versions is needed.

@see https://semver.org/");
								cf_meta = [];
								cf_kind = Method MethNormal;
								cf_params = [];
								cf_expr = None;
								cf_expr_unoptimized = None;
								cf_overloads = [];
								cf_flags = set_flag 0 (int_of_class_field_flag CfPublic);
							};
							scope = CFSStatic;
							origin = BuiltIn;
							is_qualified = true;
						} (tpair t)) None p
				| EConst(Ident(n)) ->
					let v = eval com.defines (e,p) in
					DisplayException.raise_hover (CompletionItem.make_ci_define n (match v with
						| TNull -> None
						| TString s -> Some (StringHelper.s_escape s)
						| _ -> die ""
					) (tpair com.basic.tstring)) None p
				| _ ->
					()
		in
		let rec loop (e,p) =
			if check_position p then check_expr (e,p);
			Ast.iter_expr loop (e,p);
			if check_position p then
				let v = eval com.defines (e,p) in
				let s,t = convert_small_type com v in
				DisplayException.raise_hover (CompletionItem.make_ci_literal s (tpair t)) None p
		in
		loop e;
end

module PdiHandler = struct
	open Parser
	open DisplayPosition

	let is_true defines e =
		ParserEntry.is_true (ParserEntry.eval defines e)

	let handle_pdi com pdi =
		let macro_defines = adapt_defines_to_macro_context com.defines in
		let check = (if com.display.dms_kind = DMHover then
			encloses_position_gt
		else
			encloses_position
		) display_position#get in
		List.iter (fun (p,e) ->
			let has_macro_define = is_true macro_defines e in
			if has_macro_define then com.display_information.display_module_has_macro_defines <- true;
			if check p then begin
				if has_macro_define then
					raise DisplayInMacroBlock;
				begin match com.display.dms_kind with
				| DMHover ->
					raise (DisplayException.DisplayException(DisplayHover None))
				| _ ->
					()
				end;
			end;
		) pdi.pd_dead_blocks;
		begin match com.display.dms_kind with
		| DMHover ->
			List.iter (ConditionDisplay.check_condition com) pdi.pd_conditions;
		| _ ->
			()
		end;
		()
end

let handle_parser_result com p result =
	let handle_parser_error msg p =
		let msg = Parser.error_msg msg in
		match com.display.dms_error_policy with
			| EPShow -> error msg p
			| EPIgnore -> ()
			| EPCollect -> add_diagnostics_message com msg p DKParserError Error
	in
	match result with
		| ParseSuccess(data,is_display_file,pdi) ->
			if is_display_file then begin
				begin match pdi.pd_errors with
				| (msg,p) :: _ -> handle_parser_error msg p
				| [] -> ()
				end;
				PdiHandler.handle_pdi com pdi;
			end;
			data
		| ParseError(data,(msg,p),_) ->
			handle_parser_error msg p;
			data

let parse_module_file com file p =
	handle_parser_result com p ((!parse_hook) com file p)

let parse_module' com m p =
	let remap = ref (fst m) in
	let file = resolve_module_file com m remap p in
	let pack,decls = parse_module_file com file p in
	file,remap,pack,decls

let parse_module ctx m p =
	let file,remap,pack,decls = parse_module' ctx.com m p in
	if pack <> !remap then begin
		let spack m = if m = [] then "`package;`" else "`package " ^ (String.concat "." m) ^ ";`" in
		if p == null_pos then
			display_error ctx ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m)) p
		else
			display_error ctx (spack pack ^ " in " ^ file ^ " should be " ^ spack (fst m)) {p with pmax = p.pmin}
	end;
	file, if !remap <> fst m then
		(* build typedefs to redirect to real package *)
		List.rev (List.fold_left (fun acc (t,p) ->
			let build f d =
				let priv = List.mem f d.d_flags in
				(ETypedef {
					d_name = d.d_name;
					d_doc = None;
					d_meta = [];
					d_params = d.d_params;
					d_flags = if priv then [EPrivate] else [];
					d_data = begin
						let tp =
							if priv then
								mk_type_path ([],"Dynamic")
							else
								let params =
									List.map (fun tp ->
										TPType (CTPath (mk_type_path ([],fst tp.tp_name)),null_pos)
									) d.d_params
								in
								mk_type_path ~params (!remap,fst d.d_name)
						in
						CTPath (tp),null_pos;
					end
				},p) :: acc
			in
			match t with
			| EClass d -> build HPrivate d
			| EEnum d -> build EPrivate d
			| ETypedef d -> build EPrivate d
			| EAbstract d -> build AbPrivate d
			| EImport _ | EUsing _ -> acc
		) [(EImport (List.map (fun s -> s,null_pos) (!remap @ [snd m]),INormal),null_pos)] decls)
	else
		decls

(* let parse_module ctx m p =
	let timer = Timer.timer ["typing";"parse_module"] in
	Std.finally timer (parse_module ctx m) p *)