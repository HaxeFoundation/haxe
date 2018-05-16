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

(* Parsing of module files into modules. *)

open Globals
open Ast
open DisplayTypes.DisplayMode
open Common
open Typecore
open Error

let parse_file_from_lexbuf com file p lexbuf =
	let t = Timer.timer ["parsing"] in
	Lexer.init file true;
	incr stats.s_files_parsed;
	let data = try
		ParserEntry.parse com.defines lexbuf
	with
		| Sedlexing.MalFormed ->
			t();
			error "Malformed file. Source files must be encoded with UTF-8." {pfile = file; pmin = 0; pmax = 0}
		| e ->
			t();
			raise e
	in
	begin match !Parser.display_mode with
		| DMModuleSymbols filter when filter <> None || Display.is_display_file file ->
			let ds = Display.DocumentSymbols.collect_module_symbols data in
			com.shared.shared_display_information.document_symbols <- (file,ds) :: com.shared.shared_display_information.document_symbols;
		| _ ->
			()
	end;
	t();
	Common.log com ("Parsed " ^ file);
	data

let parse_file_from_string com file p string =
	parse_file_from_lexbuf com file p (Sedlexing.Utf8.from_string string)

let current_stdin = ref None (* TODO: we're supposed to clear this at some point *)

let parse_file com file p =
	let use_stdin = (Common.defined com Define.DisplayStdin) && Display.is_display_file file in
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
		let _, decls = (!parse_hook) com file p in
		let rec loop decls = match decls with
			| ((EImport _,_) | (EUsing _,_)) :: decls -> loop decls
			| (EClass d,_) :: _ -> d.d_meta
			| (EEnum d,_) :: _ -> d.d_meta
			| (EAbstract d,_) :: _ -> d.d_meta
			| (ETypedef d,_) :: _ -> d.d_meta
			| [] -> []
		in
		let meta = loop decls in
		if not (Meta.has Meta.NoPackageRestrict meta) then begin
			let x = (match fst m with [] -> assert false | x :: _ -> x) in
			raise (Forbid_package ((x,m,p),[],if Common.defined com Define.Macro then "macro" else platform_name com.platform));
		end;
	end;
	file

let parse_module ctx m p =
	let remap = ref (fst m) in
	let file = resolve_module_file ctx.com m remap p in
	let pack, decls = (!parse_hook) ctx.com file p in
	if pack <> !remap then begin
		let spack m = if m = [] then "<empty>" else String.concat "." m in
		if p == null_pos then
			display_error ctx ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m)) p
		else
			display_error ctx ("Invalid package : " ^ spack (fst m) ^ " should be " ^ spack pack) p
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
					d_data = CTPath (if priv then { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None; } else
						{
							tpackage = !remap;
							tname = fst d.d_name;
							tparams = List.map (fun tp ->
								TPType (CTPath { tpackage = []; tname = fst tp.tp_name; tparams = []; tsub = None; },null_pos)
							) d.d_params;
							tsub = None;
						}),null_pos;
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