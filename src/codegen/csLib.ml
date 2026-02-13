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

open Globals
open Ast
open IlData
open IlMeta
open NativeLibraries

type net_lib_type = NativeLibraries.net_lib_type

type net_docs = {
	doc_full : (string, string) Hashtbl.t;
	doc_short : (string, string) Hashtbl.t;
}

type net_context = {
	nstd : bool;
	ncom : Common.context;
	nil : IlData.ilctx;
	ndocs : net_docs option;
}

(* Global tracking of types referenced by loaded DLLs but not necessarily defined in them *)
let dll_referenced_types : (path, bool) Hashtbl.t = Hashtbl.create 0

(* Cache of generated stub modules for missing referenced types *)
let stub_cache : (path, Ast.package) Hashtbl.t = Hashtbl.create 0

(* Global mapping of IL paths to Haxe paths — handles generic arity suffix stripping *)
let il_to_hx_path : (IlData.ilpath, path) Hashtbl.t = Hashtbl.create 0

(* --- Path conversion utilities --- *)

let escape_chars s =
	let buf = Buffer.create (String.length s) in
	String.iter (fun chr ->
		if (chr >= 'a' && chr <= 'z') || (chr >= 'A' && chr <= 'Z') || (chr >= '0' && chr <= '9') || chr = '_' then
			Buffer.add_char buf chr
		else begin
			Buffer.add_string buf "_x";
			Buffer.add_string buf (string_of_int (Char.code chr));
			Buffer.add_char buf '_'
		end
	) s;
	Buffer.contents buf

let add_cs = function
	| "haxe" :: _ as ns -> ns
	| "std" :: _ as ns -> ns
	| "cs" :: _ as ns -> ns
	| "system" :: _ as ns -> "cs" :: ns
	| ns -> ns

let capitalize_cl cl =
	if String.length cl > 0 && cl.[0] >= 'a' && cl.[0] <= 'z' then
		Char.escaped (Char.uppercase_ascii cl.[0]) ^ (String.sub cl 1 (String.length cl - 1))
	else
		cl

(** Convert .NET class name to Haxe, always keeping arity suffix (e.g., Dictionary\`2 → Dictionary_2) *)
let netcl_to_hx cl =
	let cl = capitalize_cl cl in
	try
		let i = String.index cl '`' in
		let name = String.sub cl 0 i in
		let nargs = String.sub cl (i + 1) (String.length cl - i - 1) in
		(escape_chars name) ^ "_" ^ nargs
	with | Not_found ->
		escape_chars cl

(** Convert .NET class name to Haxe, stripping arity suffix (e.g., Dictionary\`2 → Dictionary) *)
let netcl_to_hx_stripped cl =
	let cl = capitalize_cl cl in
	try
		let i = String.index cl '`' in
		let name = String.sub cl 0 i in
		escape_chars name
	with | Not_found ->
		escape_chars cl

(** Get the base name key for arity conflict detection: (ns, base_cl_name) *)
let ilpath_base_key (ns, _nested, cl) =
	let base = try
		let i = String.index cl '`' in
		String.sub cl 0 i
	with Not_found -> cl in
	(ns, base)

let netname_to_hx name =
	let len = String.length name in
	let chr = name.[0] in
	String.make 1 (Char.uppercase_ascii chr) ^ (String.sub name 1 (len-1))

(** Convert a .NET IL path to a Haxe type path (always keeps arity suffix).
    Nested types are flattened into the parent namespace (e.g., Dictionary\`2.KeyCollection
    becomes cs.system.collections.generic.Dictionary_2_KeyCollection). *)
let netpath_to_hx std = function
	| [],[], cl -> [], netcl_to_hx cl
	| ns,[], cl ->
		let ns = List.map (fun s -> String.lowercase_ascii (escape_chars s)) ns in
		add_cs ns, netcl_to_hx cl
	| ns,(_nhd :: _ as nested), cl ->
		let nested = List.map netcl_to_hx nested in
		let ns = List.map (fun s -> String.lowercase_ascii (escape_chars s)) ns in
		add_cs ns, String.concat "_" nested ^ "_" ^ netcl_to_hx cl

(** Convert a .NET IL path to a Haxe type path with smart arity suffix stripping.
    When [strip_arity] is true, generic arity suffixes are removed (e.g., Dictionary\`2 → Dictionary).
    Nested types are flattened into the parent namespace. *)
let netpath_to_hx_smart ~strip_arity std = function
	| [],[], cl ->
		let f = if strip_arity then netcl_to_hx_stripped else netcl_to_hx in
		[], f cl
	| ns,[], cl ->
		let f = if strip_arity then netcl_to_hx_stripped else netcl_to_hx in
		let ns = List.map (fun s -> String.lowercase_ascii (escape_chars s)) ns in
		add_cs ns, f cl
	| ns,(_nhd :: _ as nested), cl ->
		let f = if strip_arity then netcl_to_hx_stripped else netcl_to_hx in
		let nested = List.map f nested in
		let ns = List.map (fun s -> String.lowercase_ascii (escape_chars s)) ns in
		add_cs ns, String.concat "_" nested ^ "_" ^ f cl

let ilpath_s = function
	| ns,[], name -> s_type_path (ns,name)
	| [],nested,name -> String.concat "." nested ^ "." ^ name
	| ns, nested, name -> String.concat "." ns ^ "." ^ String.concat "." nested ^ "." ^ name

(** Strip backtick arity suffix from a single name component (e.g., "Dictionary`2" -> "Dictionary") *)
let strip_backtick_arity name =
	match String.index_opt name '`' with
	| Some i -> String.sub name 0 i
	| None -> name

(** Convert ilpath to C#-friendly dotted notation for @:native metadata.
    Strips backtick arity suffixes since they are IL notation, not valid C#. *)
let ilpath_to_cs = function
	| ns,[], name -> s_type_path (ns, strip_backtick_arity name)
	| [],nested,name ->
		String.concat "." (List.map strip_backtick_arity nested) ^ "." ^ strip_backtick_arity name
	| ns, nested, name ->
		String.concat "." ns ^ "." ^ String.concat "." (List.map strip_backtick_arity nested) ^ "." ^ strip_backtick_arity name

let is_haxe_keyword = function
	| "cast" | "extern" | "function" | "in" | "typedef" | "using" | "var" | "untyped" | "inline" -> true
	| _ -> false

(* --- XML Documentation Support --- *)

(** Convert ilpath to .NET dotted notation for doc key construction *)
let ilpath_to_dotnet = function
	| [], [], name -> name
	| ns, [], name -> String.concat "." ns ^ "." ^ name
	| ns, nested, name ->
		let prefix = if ns = [] then "" else String.concat "." ns ^ "." in
		prefix ^ String.concat "." nested ^ "." ^ name

(** Strip XML tags from doc content, keeping readable text *)
let rec xml_text_content = function
	| Xml.PCData s -> s
	| Xml.Element ("see", attrs, _) ->
		(try
			let cref = List.assoc "cref" attrs in
			if String.length cref > 2 && cref.[1] = ':' then
				let full = String.sub cref 2 (String.length cref - 2) in
				(try let i = String.rindex full '.' in
					String.sub full (i + 1) (String.length full - i - 1)
				with Not_found -> full)
			else cref
		with Not_found -> try
			List.assoc "langword" attrs
		with Not_found -> "")
	| Xml.Element ("paramref", attrs, _) ->
		(try List.assoc "name" attrs with Not_found -> "")
	| Xml.Element ("typeparamref", attrs, _) ->
		(try List.assoc "name" attrs with Not_found -> "")
	| Xml.Element (_, _, children) ->
		String.concat "" (List.map xml_text_content children)

(** Extract summary text from member element children *)
let extract_summary children =
	let rec find = function
		| [] -> None
		| Xml.Element ("summary", _, content) :: _ ->
			let text = String.concat "" (List.map xml_text_content content) in
			let text = String.trim text in
			if text = "" then None else Some text
		| _ :: rest -> find rest
	in
	find children

(** Construct short key by stripping method parameters *)
let short_key_of name =
	try
		let i = String.index name '(' in
		String.sub name 0 i
	with Not_found -> name

(** Parse .NET XML documentation file into doc tables *)
let parse_xml_doc file =
	let full = Hashtbl.create 512 in
	let short = Hashtbl.create 512 in
	(try
		let xml = Xml.parse_file file in
		(match xml with
		| Xml.Element ("doc", _, children) ->
			List.iter (function
				| Xml.Element ("members", _, members) ->
					List.iter (function
						| Xml.Element ("member", attrs, content) ->
							(try
								let name = List.assoc "name" attrs in
								match extract_summary content with
								| Some summary ->
									Hashtbl.replace full name summary;
									let sk = short_key_of name in
									if sk <> name && not (Hashtbl.mem short sk) then
										Hashtbl.replace short sk summary
								| None -> ()
							with Not_found -> ())
						| _ -> ()
					) members
				| _ -> ()
			) children
		| _ -> ())
	with _ -> ());
	{ doc_full = full; doc_short = short }

(* --- Binary doc cache (.hxdoc) --- *)

let hxdoc_magic = "HXDC"
let hxdoc_version = 1

let write_int32_le ch v =
	output_byte ch (v land 0xFF);
	output_byte ch ((v lsr 8) land 0xFF);
	output_byte ch ((v lsr 16) land 0xFF);
	output_byte ch ((v lsr 24) land 0xFF)

let read_int32_le ch =
	let b0 = input_byte ch in
	let b1 = input_byte ch in
	let b2 = input_byte ch in
	let b3 = input_byte ch in
	b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

(** Write doc tables to binary .hxdoc file *)
let write_hxdoc file docs =
	let ch = open_out_bin file in
	(try
		output_string ch hxdoc_magic;
		write_int32_le ch hxdoc_version;
		write_int32_le ch (Hashtbl.length docs.doc_full);
		Hashtbl.iter (fun name doc ->
			write_int32_le ch (String.length name);
			output_string ch name;
			write_int32_le ch (String.length doc);
			output_string ch doc;
		) docs.doc_full;
		close_out ch
	with e ->
		close_out_noerr ch;
		raise e)

(** Read doc tables from binary .hxdoc file *)
let read_hxdoc file =
	let full = Hashtbl.create 512 in
	let short = Hashtbl.create 512 in
	let ch = open_in_bin file in
	(try
		let magic = Bytes.create 4 in
		really_input ch magic 0 4;
		if Bytes.to_string magic <> hxdoc_magic then raise Exit;
		let version = read_int32_le ch in
		if version <> hxdoc_version then raise Exit;
		let count = read_int32_le ch in
		for _i = 0 to count - 1 do
			let name_len = read_int32_le ch in
			let name = Bytes.create name_len in
			really_input ch name 0 name_len;
			let doc_len = read_int32_le ch in
			let doc = Bytes.create doc_len in
			really_input ch doc 0 doc_len;
			let name = Bytes.to_string name in
			let doc = Bytes.to_string doc in
			Hashtbl.replace full name doc;
			let sk = short_key_of name in
			if sk <> name && not (Hashtbl.mem short sk) then
				Hashtbl.replace short sk doc
		done
	with _ -> ());
	close_in ch;
	{ doc_full = full; doc_short = short }

(** Get platform-specific cache directory for auto-generated .hxdoc files.
    - macOS: ~/Library/Caches/haxe/net-docs/
    - Linux: $XDG_CACHE_HOME/haxe/net-docs/ (default ~/.cache/haxe/net-docs/)
    - Windows: %LOCALAPPDATA%/Haxe/Cache/net-docs/ *)
let get_cache_dir () =
	let dir = match Sys.os_type with
		| "Win32" | "Cygwin" ->
			let base = try Sys.getenv "LOCALAPPDATA" with Not_found -> Filename.concat (Sys.getenv "USERPROFILE") "AppData/Local" in
			Filename.concat (Filename.concat (Filename.concat base "Haxe") "Cache") "net-docs"
		| _ ->
			if Sys.file_exists "/Library" then
				(* macOS *)
				Filename.concat (Filename.concat (Filename.concat (Sys.getenv "HOME") "Library") "Caches") "haxe/net-docs"
			else
				(* Linux/other *)
				let base = try Sys.getenv "XDG_CACHE_HOME" with Not_found -> Filename.concat (Sys.getenv "HOME") ".cache" in
				Filename.concat (Filename.concat base "haxe") "net-docs"
	in
	(* Create directory if needed *)
	Path.mkdir_from_path (dir ^ "/");
	dir

(** Get cached .hxdoc path for a given .xml file, in the platform cache directory *)
let cache_path_for xml_path =
	let abs_path = if Filename.is_relative xml_path then Filename.concat (Sys.getcwd ()) xml_path else xml_path in
	let hash = Printf.sprintf "%08x" (Hashtbl.hash abs_path) in
	Filename.concat (get_cache_dir ()) (hash ^ ".hxdoc")

(* --- Cache manifest and locking --- *)

let manifest_path () = Filename.concat (get_cache_dir ()) "index"
let lockfile_path () = Filename.concat (get_cache_dir ()) "index.lock"

(** Read the manifest file. Returns a list of (hxdoc_filename, xml_abs_path) pairs. *)
let read_manifest () =
	let path = manifest_path () in
	if not (Sys.file_exists path) then []
	else try
		let ch = open_in path in
		let entries = ref [] in
		(try while true do
			let line = input_line ch in
			match String.split_on_char '\t' line with
			| [hxdoc_name; xml_path] -> entries := (hxdoc_name, xml_path) :: !entries
			| _ -> ()
		done with End_of_file -> ());
		close_in ch;
		List.rev !entries
	with _ -> []

(** Write the manifest file atomically via temp file + rename. *)
let write_manifest entries =
	let path = manifest_path () in
	let tmp = path ^ ".tmp" in
	let ch = open_out tmp in
	(try
		List.iter (fun (hxdoc_name, xml_path) ->
			output_string ch hxdoc_name;
			output_char ch '\t';
			output_string ch xml_path;
			output_char ch '\n';
		) entries;
		close_out ch;
		Sys.rename tmp path
	with e ->
		close_out_noerr ch;
		(try Sys.remove tmp with _ -> ());
		raise e)

(** Acquire the cache lock. Returns Some fd on success, None if lock is held. *)
let acquire_lock () =
	let path = lockfile_path () in
	let try_acquire () =
		try
			let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_EXCL; Unix.O_WRONLY] 0o644 in
			Some fd
		with Unix.Unix_error (Unix.EEXIST, _, _) -> None
	in
	match try_acquire () with
	| Some fd -> Some fd
	| None ->
		(* Stale lock check: if older than 60s, assume crashed owner *)
		(try
			let st = Unix.stat path in
			if Unix.time () -. st.Unix.st_mtime > 60.0 then begin
				(try Sys.remove path with _ -> ());
				try_acquire ()
			end else
				None
		with _ -> None)

(** Release the cache lock. *)
let release_lock fd =
	(try Unix.close fd with _ -> ());
	(try Sys.remove (lockfile_path ()) with _ -> ())

(** Run a function while holding the cache lock. Returns None if lock unavailable. *)
let with_cache_lock f =
	match acquire_lock () with
	| None -> None
	| Some fd ->
		let result = (try Some (f ()) with e -> release_lock fd; raise e) in
		release_lock fd;
		result

(** Clean up stale cache entries. Must be called while holding the lock. *)
let cleanup_cache_dir () =
	let dir = get_cache_dir () in
	let entries = read_manifest () in
	(* Pass 1: validate manifest entries *)
	let valid = List.filter (fun (hxdoc_name, xml_path) ->
		let hxdoc_path = Filename.concat dir hxdoc_name in
		if not (Sys.file_exists hxdoc_path) then
			false (* .hxdoc gone — remove from manifest *)
		else if not (Sys.file_exists xml_path) then begin
			(try Sys.remove hxdoc_path with _ -> ());
			false (* source XML gone — delete .hxdoc and remove from manifest *)
		end else begin
			let hxdoc_mtime = (Unix.stat hxdoc_path).Unix.st_mtime in
			let xml_mtime = (Unix.stat xml_path).Unix.st_mtime in
			if hxdoc_mtime < xml_mtime then begin
				(try Sys.remove hxdoc_path with _ -> ());
				false (* cache outdated — delete and remove *)
			end else
				true (* valid *)
		end
	) entries in
	(* Pass 2: delete orphaned .hxdoc files not in manifest *)
	let valid_set = Hashtbl.create (List.length valid) in
	List.iter (fun (name, _) -> Hashtbl.replace valid_set name true) valid;
	(try
		let files = Sys.readdir dir in
		Array.iter (fun name ->
			if Filename.check_suffix name ".hxdoc" && not (Hashtbl.mem valid_set name) then
				(try Sys.remove (Filename.concat dir name) with _ -> ())
		) files
	with _ -> ());
	(* Rewrite manifest with valid entries only *)
	write_manifest valid

(** Load documentation for a .NET DLL.
    Priority: .hxdoc next to DLL (shipped) → .xml next to DLL (cached in platform dir) → None *)
let load_doc dll_path =
	let base = Filename.chop_extension dll_path in
	let hxdoc_path = base ^ ".hxdoc" in
	let xml_path = base ^ ".xml" in
	if Sys.file_exists hxdoc_path then
		(* Shipped .hxdoc next to DLL — read directly *)
		Some (read_hxdoc hxdoc_path)
	else if Sys.file_exists xml_path then begin
		let cached = cache_path_for xml_path in
		(* Check if cached version exists and is newer than XML *)
		let use_cache = Sys.file_exists cached &&
			(Unix.stat cached).Unix.st_mtime >= (Unix.stat xml_path).Unix.st_mtime in
		if use_cache then
			Some (read_hxdoc cached)
		else begin
			let docs = parse_xml_doc xml_path in
			let abs_xml = if Filename.is_relative xml_path then Filename.concat (Sys.getcwd ()) xml_path else xml_path in
			(try
				ignore (with_cache_lock (fun () ->
					write_hxdoc cached docs;
					let hxdoc_name = Filename.basename cached in
					let entries = read_manifest () in
					let entries = List.filter (fun (name, _) -> name <> hxdoc_name) entries in
					let entries = entries @ [(hxdoc_name, abs_xml)] in
					write_manifest entries;
					cleanup_cache_dir ()
				))
			with _ -> ());
			Some docs
		end
	end else
		None

(** Generate .hxdoc binary cache from a .NET XML doc file.
    Output path is same directory, same basename, .hxdoc extension. *)
let generate_hxdoc xml_path =
	let docs = parse_xml_doc xml_path in
	let output_path = (Filename.chop_extension xml_path) ^ ".hxdoc" in
	write_hxdoc output_path docs;
	output_path

(** Construct a documentation value from a doc lookup *)
let mk_doc text =
	Some { doc_own = Some text; doc_inherited = [] }

(** Look up type-level documentation *)
let lookup_type_doc ctx ilpath =
	match ctx.ndocs with
	| None -> None
	| Some docs ->
		let key = "T:" ^ ilpath_to_dotnet ilpath in
		try mk_doc (Hashtbl.find docs.doc_full key) with Not_found -> None

(** Look up field documentation *)
let lookup_field_doc ctx ilpath fname =
	match ctx.ndocs with
	| None -> None
	| Some docs ->
		let key = "F:" ^ ilpath_to_dotnet ilpath ^ "." ^ fname in
		try mk_doc (Hashtbl.find docs.doc_full key) with Not_found -> None

(** Look up method documentation *)
let lookup_method_doc ctx ilpath mname =
	match ctx.ndocs with
	| None -> None
	| Some docs ->
		let dotnet_name = ilpath_to_dotnet ilpath in
		let mname = if mname = ".ctor" then "#ctor" else mname in
		let key = "M:" ^ dotnet_name ^ "." ^ mname in
		(try mk_doc (Hashtbl.find docs.doc_full key)
		with Not_found ->
			try mk_doc (Hashtbl.find docs.doc_short key)
			with Not_found -> None)

(** Look up property documentation *)
let lookup_prop_doc ctx ilpath pname =
	match ctx.ndocs with
	| None -> None
	| Some docs ->
		let key = "P:" ^ ilpath_to_dotnet ilpath ^ "." ^ pname in
		try mk_doc (Hashtbl.find docs.doc_full key) with Not_found -> None

(** Look up event documentation *)
let lookup_event_doc ctx ilpath ename =
	match ctx.ndocs with
	| None -> None
	| Some docs ->
		let key = "E:" ^ ilpath_to_dotnet ilpath ^ "." ^ ename in
		try mk_doc (Hashtbl.find docs.doc_full key) with Not_found -> None

(* --- IL→Haxe signature conversion --- *)

let mk_type_path ctx path params =
	let pack, name = match path with
		| ns,[], cl ->
			ns, netcl_to_hx cl
		| ns, (_nhd :: _ as nested), cl ->
			let nested = List.map netcl_to_hx nested in
			ns, String.concat "_" nested ^ "_" ^ netcl_to_hx cl
	in
	CTPath (make_ptp {
		tpackage = fst (netpath_to_hx ctx.nstd (pack,[],""));
		tname = name;
		tparams = params;
		tsub = None;
	} null_pos)

let raw_type_path _ctx path params =
	make_ptp {
		tpackage = fst path;
		tname = snd path;
		tparams = params;
		tsub = None;
	} null_pos

let rec convert_signature ctx p = function
	| LVoid ->
		mk_type_path ctx ([],[],"Void") []
	| LBool ->
		mk_type_path ctx ([],[],"Bool") []
	| LChar ->
		mk_type_path ctx (["cs"],[],"Char16") []
	| LInt8 ->
		mk_type_path ctx (["cs"],[],"Int8") []
	| LUInt8 ->
		mk_type_path ctx (["cs"],[],"UInt8") []
	| LInt16 ->
		mk_type_path ctx (["cs"],[],"Int16") []
	| LUInt16 ->
		mk_type_path ctx (["cs"],[],"UInt16") []
	| LInt32 ->
		mk_type_path ctx ([],[],"Int") []
	| LUInt32 ->
		mk_type_path ctx ([],[],"UInt") []
	| LInt64 ->
		mk_type_path ctx (["haxe"],[],"Int64") []
	| LUInt64 ->
		mk_type_path ctx (["cs"],[],"UInt64") []
	| LFloat32 ->
		mk_type_path ctx ([],[],"Single") []
	| LFloat64 ->
		mk_type_path ctx ([],[],"Float") []
	| LString ->
		mk_type_path ctx (["std"],[],"String") []
	| LObject ->
		mk_type_path ctx ([],[],"Dynamic") []
	| LPointer s | LManagedPointer s ->
		mk_type_path ctx (["cs"],[],"Pointer") [ TPType (convert_signature ctx p s,null_pos) ]
	| LTypedReference ->
		mk_type_path ctx (["cs";"system"],[],"TypedReference") []
	| LIntPtr ->
		mk_type_path ctx (["cs";"system"],[],"IntPtr") []
	| LUIntPtr ->
		mk_type_path ctx (["cs";"system"],[],"UIntPtr") []
	| LValueType (s,args) | LClass (s,args) ->
		let params = List.map (fun s -> TPType (convert_signature ctx p s,null_pos)) args in
		(* Look up pre-computed smart name (with arity stripping) first *)
		(match Hashtbl.find_opt il_to_hx_path s with
		| Some hx_path ->
			CTPath (make_ptp {
				tpackage = fst hx_path;
				tname = snd hx_path;
				tparams = params;
				tsub = None;
			} null_pos)
		| None ->
			mk_type_path ctx s params)
	| LTypeParam i ->
		mk_type_path ctx ([],[],"T" ^ string_of_int i) []
	| LMethodTypeParam i ->
		mk_type_path ctx ([],[],"M" ^ string_of_int i) []
	| LVector s ->
		mk_type_path ctx (["cs"],[],"NativeArray") [TPType (convert_signature ctx p s,null_pos)]
	| LMethod (_,ret,args) ->
		CTFunction (List.map (fun v -> convert_signature ctx p v,null_pos) args, (convert_signature ctx p ret,null_pos))
	| _ -> mk_type_path ctx ([],[], "Dynamic") []

let get_type_path _ctx ct = match ct with | CTPath p -> p | _ -> die "" __LOC__

(* --- Attribute helpers --- *)

let has_attr expected_name expected_ns ilcls =
	let check_flag name ns = (name = expected_name && ns = expected_ns) in
	List.exists (fun a ->
		match a.ca_type with
			| TypeRef r ->
				check_flag r.tr_name r.tr_namespace
			| TypeDef d ->
				check_flag d.td_name d.td_namespace
			| Method m ->
				(match m.m_declaring with
					| Some d -> check_flag d.td_name d.td_namespace
					| _ -> false)
			| MemberRef r ->
				(match r.memr_class with
					| TypeRef r -> check_flag r.tr_name r.tr_namespace
					| TypeDef d -> check_flag d.td_name d.td_namespace
					| _ -> false)
			| _ -> false
	) ilcls.cattrs

let is_compiler_generated = has_attr "CompilerGeneratedAttribute" ["System"; "Runtime"; "CompilerServices"]

(* --- Field / method conversion --- *)

let rec has_unmanaged = function
	| LPointer _ -> true
	| LManagedPointer s -> has_unmanaged s
	| LValueType (_,pl) -> List.exists has_unmanaged pl
	| LClass (_,pl) -> List.exists has_unmanaged pl
	| LVector s -> has_unmanaged s
	| LArray (s,_) -> has_unmanaged s
	| LMethod (_,r,args) -> has_unmanaged r || List.exists has_unmanaged args
	| _ -> false

let convert_ilfield ctx p ilpath field =
	if not (Common.raw_defined ctx.ncom "unsafe") && has_unmanaged field.fsig.snorm then raise Exit;
	let p = { p with pfile = p.pfile ^" (" ^field.fname ^")" } in
	let cff_name = match field.fname with
		| name when String.length name > 5 ->
				(match String.sub name 0 5 with
				| "__hx_" -> raise Exit
				| _ -> name)
		| name -> name
	in
	let cff_access = match field.fflags.ff_access with
		| FAFamily | FAFamOrAssem -> (APrivate,null_pos)
		| FAPublic -> (APublic,null_pos)
		| _ -> raise Exit
	in
	let readonly, acc = List.fold_left (fun (readonly,acc) -> function
		| CStatic -> readonly, (AStatic,null_pos) :: acc
		| CInitOnly | CLiteral -> true, acc
		| _ -> readonly,acc
	) (false,[cff_access]) field.fflags.ff_contract in
	let kind = match readonly with
		| true ->
			FProp (("default",null_pos), ("never",null_pos), Some (convert_signature ctx p field.fsig.snorm,null_pos), None)
		| false ->
			FVar (Some (convert_signature ctx p field.fsig.snorm,null_pos), None)
	in
	let cff_name, cff_meta =
		if String.length cff_name > 0 && cff_name.[0] = '%' then
			let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
			"_" ^ name,
			[Meta.Native, [EConst (String (name,SDoubleQuotes) ), p], p]
		else
			cff_name, []
	in
	{
		cff_name = cff_name,null_pos;
		cff_doc = lookup_field_doc ctx ilpath field.fname;
		cff_pos = p;
		cff_meta = cff_meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilevent ctx p ilpath ev =
	let p = { p with pfile = p.pfile ^" (" ^ev.ename ^")" } in
	let name = ev.ename in
	let kind = FVar (Some (convert_signature ctx p ev.esig.snorm,null_pos), None) in
	let meta = [Meta.Custom ":event", [], p; Meta.Keep,[],p; Meta.Custom ":skipReflection",[],p] in
	let acc = [APrivate,null_pos] in
	let add_m acc m = match m with
		| None -> acc
		| Some (_name,flags) ->
			if List.mem (CMStatic) flags.mf_contract then
				(AStatic,null_pos) :: acc
			else
				acc
	in
	let acc = add_m acc ev.eadd in
	let acc = add_m acc ev.eremove in
	let acc = add_m acc ev.eraise in
	{
		cff_name = name,null_pos;
		cff_doc = lookup_event_doc ctx ilpath ev.ename;
		cff_pos = p;
		cff_meta = meta;
		cff_access = acc;
		cff_kind = kind;
	}

let lookup_ilclass std com ilpath =
	let path = match Hashtbl.find_opt il_to_hx_path ilpath with
		| Some p -> p
		| None -> netpath_to_hx std ilpath
	in
	List.fold_right (fun net_lib acc ->
		match acc with
		| None -> net_lib#lookup path
		| Some _ -> acc
	) com.Common.native_libs.net_libs None

let convert_ilmethod ctx p ilpath is_interface m is_explicit_impl =
	if not (Common.raw_defined ctx.ncom "unsafe") && has_unmanaged m.msig.snorm then raise Exit;
	let force_check = Common.raw_defined ctx.ncom "force-lib-check" in
	let p = { p with pfile = p.pfile ^" (" ^m.mname ^")" } in
	let cff_name = match m.mname with
		| ".ctor" -> "new"
		| ".cctor"-> raise Exit
		| "Finalize" -> raise Exit
		| "Equals" | "GetHashCode" -> raise Exit
		| name when String.length name > 5 ->
				(match String.sub name 0 5 with
				| "__hx_" -> raise Exit
				| _ -> name)
		| name -> name
	in
	let meta = [] in
	let acc, meta = match m.mflags.mf_access with
		| FAFamily | FAFamOrAssem ->
			(APrivate,null_pos), ((Meta.Protected, [], p) :: meta)
		| FAPublic -> (APublic,null_pos), meta
		| _ -> raise Exit
	in
	let is_static = ref false in
	let acc, is_final = List.fold_left (fun (acc,is_final) -> function
		| CMStatic when cff_name <> "new" -> is_static := true; (AStatic,null_pos) :: acc, is_final
		| CMVirtual when is_final = None -> acc, Some false
		| CMFinal -> acc, Some true
		| _ -> acc, is_final
	) ([acc],None) m.mflags.mf_contract in
	let acc = (AOverload,p) :: acc in
	let acc = match is_final with
		| None | Some true when not force_check && not !is_static ->
			(AFinal,null_pos) :: acc
		| _ ->
			acc
	in
	let meta = if is_explicit_impl then
			(Meta.NoCompletion,[],p) :: (Meta.Custom ":skipReflection",[],p) :: meta
		else
			meta
	in
	let rec change_sig = function
		| LManagedPointer s -> LManagedPointer (change_sig s)
		| LPointer s -> LPointer (change_sig s)
		| LValueType (p,pl) -> LValueType(p, List.map change_sig pl)
		| LClass (p,pl) -> LClass(p, List.map change_sig pl)
		| LTypeParam _ -> LObject
		| LVector s -> LVector (change_sig s)
		| LArray (s,a) -> LArray (change_sig s, a)
		| LMethod (c,r,args) -> LMethod (c, change_sig r, List.map change_sig args)
		| p -> p
	in
	let change_sig = if !is_static then change_sig else (fun s -> s) in
	let ret =
		if String.length cff_name > 4 && String.sub cff_name 0 4 = "set_" then
			match m.mret.snorm, m.margs with
			| LVoid, [_,_,s] ->
				s.snorm
			| _ -> m.mret.snorm
		else
			m.mret.snorm
	in
	let kind =
		let args = List.map (fun (name,flag,s) ->
			let t = match s.snorm with
				| LManagedPointer s ->
					let is_out = List.mem POut flag.pf_io && not (List.mem PIn flag.pf_io) in
					let rname = if is_out then "Out" else "Ref" in
					mk_type_path ctx (["cs"],[],rname) [ TPType (convert_signature ctx p s,null_pos) ]
				| _ ->
					convert_signature ctx p (change_sig s.snorm)
			in
			(name,null_pos),false,[],Some (t,null_pos),None) m.margs
		in
		let ret = convert_signature ctx p (change_sig ret) in
		let types = List.map (fun t ->
			{
				tp_name = "M" ^ string_of_int t.tnumber,null_pos;
				tp_params = [];
				tp_constraints = None;
				tp_default = None;
				tp_meta = [];
			}
		) m.mtypes in
		FFun {
			f_params = types;
			f_args = args;
			f_type = Some (ret,null_pos);
			f_expr = None;
		}
	in
	let cff_name, cff_meta =
		if String.length cff_name > 0 && cff_name.[0] = '%' then
			let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
			"_" ^ name,
			(Meta.Native, [EConst (String (name,SDoubleQuotes) ), p], p) :: meta
		else
			cff_name, meta
	in
	let acc = match m.moverride with
		| None ->
			if not is_interface && List.mem IAbstract m.mflags.mf_impl then (AAbstract,null_pos) :: acc else acc
		| _ when cff_name = "new" -> acc
		| Some (path,_s) -> (match lookup_ilclass ctx.nstd ctx.ncom path with
			| Some ilcls when not (List.mem SInterface ilcls.cflags.tdf_semantics) ->
				(AOverride,null_pos) :: acc
			| None when ctx.ncom.verbose ->
				print_endline ("(net-lib) A referenced assembly for path " ^ ilpath_s path ^ " was not found");
				acc
			| _ -> acc)
	in
	{
		cff_name = cff_name,null_pos;
		cff_doc = lookup_method_doc ctx ilpath m.mname;
		cff_pos = p;
		cff_meta = cff_meta;
		cff_access = acc;
		cff_kind = kind;
	}

let convert_ilprop ctx p ilpath prop is_explicit_impl =
	if not (Common.raw_defined ctx.ncom "unsafe") && has_unmanaged prop.psig.snorm then raise Exit;
	let p = { p with pfile = p.pfile ^" (" ^prop.pname ^")" } in
	let pmflags = match prop.pget, prop.pset with
		| Some(_,fl1), _ -> Some fl1
		| _, Some(_,fl2) -> Some fl2
		| _ -> None
	in
	let cff_access = match pmflags with
		| Some { mf_access = FAFamily | FAFamOrAssem } -> (APrivate,null_pos)
		| Some { mf_access = FAPublic } -> (APublic,null_pos)
		| _ -> raise Exit
	in
	let access acc = acc.mf_access in
	let cff_access = match pmflags with
		| Some m when List.mem CMStatic m.mf_contract ->
			[AStatic,null_pos;cff_access]
		| _ -> [cff_access]
	in
	let get = match prop.pget with
		| None -> "never"
		| Some(s,_) when String.length s <= 4 || String.sub s 0 4 <> "get_" ->
			raise Exit
		| Some(_,m) when access m <> FAPublic -> (match access m with
			| FAFamily | FAFamOrAssem -> "null"
			| _ -> "never")
		| Some _ -> "default"
	in
	let set = match prop.pset with
		| None -> "never"
		| Some(s,_) when String.length s <= 4 || String.sub s 0 4 <> "set_" ->
			raise Exit
		| Some(_,m) when access m <> FAPublic -> (match access m with
			| FAFamily | FAFamOrAssem -> "never"
			| _ -> "never")
		| Some _ -> "default"
	in
	let ilsig = match prop.psig.snorm with
		| LMethod (_,ret,[]) -> ret
		| _ -> raise Exit
	in
	let meta = if is_explicit_impl then
			[ Meta.NoCompletion,[],p; Meta.Custom ":skipReflection",[],p ]
		else
			[]
	in
	let kind =
		FProp ((get,null_pos), (set,null_pos), Some(convert_signature ctx p ilsig,null_pos), None)
	in
	{
		cff_name = prop.pname,null_pos;
		cff_doc = lookup_prop_doc ctx ilpath prop.pname;
		cff_pos = p;
		cff_meta = meta;
		cff_access = cff_access;
		cff_kind = kind;
	}

(* --- Enum conversion --- *)

let convert_ilenum ctx p ilcls =
	let meta = [
		Meta.Native, [EConst (String (ilpath_to_cs ilcls.cpath,SDoubleQuotes) ), p], p;
	] in
	let fields = ref [] in
	let is_flags = has_attr "FlagsAttribute" ["System"] ilcls in
	List.iter (fun f -> match f.fname with
		| "value__" -> ()
		| _ when not (List.mem CStatic f.fflags.ff_contract) -> ()
		| _ ->
			let int_val = match f.fconstant with
				| Some IChar i | Some IByte i | Some IShort i -> i
				| Some IInt i -> Int32.to_int i
				| Some IInt64 i -> Int64.to_int i
				| _ -> 0
			in
			let field = {
				cff_name = f.fname,null_pos;
				cff_doc = lookup_field_doc ctx ilcls.cpath f.fname;
				cff_pos = p;
				cff_meta = [];
				cff_access = [];
				cff_kind = FVar (None, Some (EConst (Int (string_of_int int_val, None)), p));
			} in
			fields := (field, int_val) :: !fields
	) ilcls.cfields;
	let fields = List.stable_sort (fun (_,i1) (_,i2) -> compare i1 i2) (List.rev !fields) in
	let hx_path = (match Hashtbl.find_opt il_to_hx_path ilcls.cpath with
		| Some p -> p
		| None -> netpath_to_hx ctx.nstd ilcls.cpath
	) in
	let name = netname_to_hx (snd hx_path) in
	if is_flags then begin
		let enum_fields = List.map fst fields in
		let int_type = (CTPath (make_ptp { tpackage = []; tname = "Int"; tparams = []; tsub = None } null_pos), null_pos) in
		let self_ct = (CTPath (make_ptp { tpackage = fst hx_path; tname = name; tparams = []; tsub = None } null_pos), null_pos) in
		let mk_arg aname = ((aname,null_pos), false, [], Some self_ct, None) in
		let mk_op_field op_name op_expr =
			{
				cff_name = op_name,null_pos;
				cff_doc = None;
				cff_pos = p;
				cff_meta = [Meta.Op, [op_expr], p];
				cff_access = [AStatic,null_pos];
				cff_kind = FFun {
					f_params = [];
					f_args = [mk_arg "lhs"; mk_arg "rhs"];
					f_type = Some self_ct;
					f_expr = None;
				};
			}
		in
		let a = (EConst (Ident "A"), p) in
		let b = (EConst (Ident "B"), p) in
		let op_fields = [
			mk_op_field "or" (EBinop (OpOr, a, b), p);
			mk_op_field "and" (EBinop (OpAnd, a, b), p);
			mk_op_field "xor" (EBinop (OpXor, a, b), p);
		] in
		EAbstract {
			d_name = name,null_pos;
			d_doc = lookup_type_doc ctx ilcls.cpath;
			d_params = [];
			d_meta = meta;
			d_flags = [AbEnum; AbExtern; AbOver int_type];
			d_data = enum_fields @ op_fields;
		}
	end else begin
		EEnum {
			d_name = name,null_pos;
			d_doc = lookup_type_doc ctx ilcls.cpath;
			d_params = [];
			d_meta = meta;
			d_flags = [EExtern];
			d_data = List.map (fun (field, _) ->
				{
					ec_name = field.cff_name;
					ec_doc = field.cff_doc;
					ec_meta = [];
					ec_args = [];
					ec_pos = p;
					ec_params = [];
					ec_type = None;
				}
			) fields;
		}
	end

(* --- Explicit interface detection --- *)

let is_explicit ctx ilcls i =
	let s = match i with
		| LClass(path,_) | LValueType(path,_) -> ilpath_s path
		| _ -> die "" __LOC__
	in
	let len = String.length s in
	List.exists (fun m ->
		String.length m.mname > len && String.sub m.mname 0 len = s
	) ilcls.cmethods

(* --- Normalization --- *)

type il_any_field =
	| IlField of ilfield
	| IlMethod of ilmethod
	| IlProp of ilprop

let is_static_field = function
	| IlField f ->
		List.mem CStatic f.fflags.ff_contract
	| IlMethod m ->
		List.mem CMStatic m.mflags.mf_contract
	| IlProp p ->
		List.exists (function
		 | None -> false
		 | Some (_,m) -> List.mem CMStatic m.mf_contract
		) [p.pget;p.pset]

let change_name name = function
	| IlField f -> IlField { f with fname = name }
	| IlMethod m -> IlMethod { m with mname = name }
	| IlProp p -> IlProp { p with pname = name }

let compatible_field f1 f2 = match f1, f2 with
	| IlMethod { msig = { snorm = LMethod(_,_,a1) } },
		IlMethod { msig = { snorm = LMethod(_,_,a2) } } ->
			a1 = a2
	| IlProp _, IlProp _ -> true
	| IlField _, IlField _ -> true
	| _ -> false

let compatible_methods_sig m1 m2 = match m1, m2 with
	| LMethod(_,_r1,a1), LMethod(_,_r2,a2) -> (try
		List.for_all2 (fun a1 a2 ->
			match a1, a2 with
			| LManagedPointer s1, LManagedPointer s2 -> s1 = s2
			| LManagedPointer s1, s2 | s1, LManagedPointer s2 -> s1 = s2
			| _ -> a1 = a2
		) a1 a2
	with | Invalid_argument _ ->
		false)
	| _ -> false

let get_all_fields cls =
	let all_fields = List.map (fun f -> IlField f, cls.cpath, f.fname, List.mem CStatic f.fflags.ff_contract) cls.cfields in
	let all_fields = all_fields @ List.map (fun m -> IlMethod m, cls.cpath, m.mname, List.mem CMStatic m.mflags.mf_contract) cls.cmethods in
	let all_fields = all_fields @ List.map (fun p -> IlProp p, cls.cpath, p.pname, is_static_field (IlProp p)) cls.cprops in
	all_fields

let ilcls_from_ilsig ctx ilsig =
	let path, params = match ilsig with
		| LClass(path, params) | LValueType(path, params) ->
			path, params
		| LObject ->
			(["System"],[],"Object"),[]
		| _ -> raise Not_found
	in
	match lookup_ilclass ctx.nstd ctx.ncom path with
	| None -> raise Not_found
	| Some c ->
		c, params

let rec ilapply_params params = function
	| LManagedPointer s -> LManagedPointer (ilapply_params params s)
	| LPointer s -> LPointer (ilapply_params params s)
	| LValueType (p,pl) -> LValueType(p, List.map (ilapply_params params) pl)
	| LClass (p,pl) -> LClass(p, List.map (ilapply_params params) pl)
	| LTypeParam i -> List.nth params i
	| LVector s -> LVector (ilapply_params params s)
	| LArray (s,a) -> LArray (ilapply_params params s, a)
	| LMethod (c,r,args) -> LMethod (c, ilapply_params params r, List.map (ilapply_params params) args)
	| p -> p

let ilcls_with_params _ctx cls params =
	match cls.ctypes with
	| [] -> cls
	| _ ->
		{ cls with
			cfields = List.map (fun f -> { f with fsig = { f.fsig with snorm = ilapply_params params f.fsig.snorm } }) cls.cfields;
			cmethods = List.map (fun m -> { m with
				msig = { m.msig with snorm = ilapply_params params m.msig.snorm };
				margs = List.map (fun (n,f,s) -> (n,f,{ s with snorm = ilapply_params params s.snorm })) m.margs;
				mret = { m.mret with snorm = ilapply_params params m.mret.snorm };
			}) cls.cmethods;
			cprops = List.map (fun p -> { p with psig = { p.psig with snorm = ilapply_params params p.psig.snorm } }) cls.cprops;
			csuper = Option.map (fun s -> { s with snorm = ilapply_params params s.snorm } ) cls.csuper;
			cimplements = List.map (fun s -> { s with snorm = ilapply_params params s.snorm } ) cls.cimplements;
		}

let normalize_ilcls ctx cls =
	let force_check = Common.raw_defined ctx.ncom "force-lib-check" in
	let rec loop acc = function
		| [] -> acc
		| m :: cmeths ->
			let is_static = List.mem CMStatic m.mflags.mf_contract in
			if List.exists (fun m2 -> m.mname = m2.mname && List.mem CMStatic m2.mflags.mf_contract = is_static && compatible_methods_sig m.msig.snorm m2.msig.snorm) cmeths then
				loop acc cmeths
			else
				loop (m :: acc) cmeths
	in
	let meths = loop [] cls.cmethods in
	let meths = List.map (fun v -> ref v) meths in
	let no_overrides = List.filter (fun m ->
		let m = !m in
		not (List.mem CMStatic m.mflags.mf_contract)
	) meths in
	let no_overrides = ref no_overrides in
	let all_fields = ref [] in
	let all_events_name = Hashtbl.create 0 in
	let add_cls_events_collision cls =
		List.iter (fun m -> if not (List.mem CMStatic m.mflags.mf_contract) then Hashtbl.replace all_events_name m.mname true) cls.cmethods;
		List.iter (fun p -> if not (is_static_field (IlProp p)) then Hashtbl.replace all_events_name p.pname true) cls.cprops;
	in
	let rec loop_super cls = try
		match cls.csuper with
		| Some { snorm = LClass((_,_,"Object"),_) }
		| Some { snorm = LObject } ->
			let cls, params = ilcls_from_ilsig ctx LObject in
			let cls = ilcls_with_params ctx cls params in
			all_fields := get_all_fields cls @ !all_fields
		| None -> ()
		| Some s ->
			let scls, params = ilcls_from_ilsig ctx s.snorm in
			let scls = ilcls_with_params ctx scls params in
			if force_check then no_overrides := List.filter (fun v ->
				let m = !v in
				let is_override_here = List.exists (fun m2 ->
					m2.mname = m.mname && not (List.mem CMStatic m2.mflags.mf_contract) && compatible_methods_sig m.msig.snorm m2.msig.snorm
				) scls.cmethods in
				if is_override_here then v := { m with moverride = Some(scls.cpath, m.mname) };
				not is_override_here
			) !no_overrides;
			all_fields := get_all_fields scls @ !all_fields;
			add_cls_events_collision scls;
			List.iter (fun ev -> Hashtbl.replace all_events_name ev.ename true) scls.cevents;
			loop_super scls
		with | Not_found -> ()
	in
	loop_super cls;
	add_cls_events_collision cls;
	if force_check then List.iter (fun v -> v := { !v with moverride = None }) !no_overrides;
	let added = ref [] in
	let current_all = ref (get_all_fields cls @ !all_fields) in
	let rec loop_interface cls iface = try
		match iface.snorm with
		| LClass((_,_,"Object"),_) | LObject -> ()
		| LClass(path,_) when path = cls.cpath -> ()
		| s ->
			let cif, params = ilcls_from_ilsig ctx s in
			let cif = ilcls_with_params ctx cif params in
			List.iter (function
				| (f,_,name,false) as ff ->
					if not (List.exists (function
						| (f2,_,name2,false) when (name = name2 || (String.length name2 > String.length name + 1 && String.sub name2 (String.length name2 - String.length name - 1) (String.length name + 1) = "." ^ name)) ->
							compatible_field f f2
						| _ -> false
					) !current_all) then begin
						current_all := ff :: !current_all;
						added := ff :: !added
					end else
						List.iter (fun mref -> match !mref with
							| m when m.mname = name && compatible_field f (IlMethod m) ->
								mref := { m with mflags = { m.mflags with mf_access = FAPublic } }
							| _ -> ()
						) meths
				| _ -> ()
			) (get_all_fields cif);
			List.iter (loop_interface cif) cif.cimplements
		with | Not_found -> ()
	in
	if not (List.mem SAbstract cls.cflags.tdf_semantics) then List.iter (loop_interface cls) cls.cimplements;
	let added = List.map (function
		| (IlMethod m,a,name,b) when m.mflags.mf_access <> FAPublic ->
			(IlMethod { m with mflags = { m.mflags with mf_access = FAPublic } },a,name,b)
		| (IlField f,a,name,b) when f.fflags.ff_access <> FAPublic ->
			(IlField { f with fflags = { f.fflags with ff_access = FAPublic } },a,name,b)
		| s -> s
	) !added in
	let props = if force_check then List.filter (function
			| p ->
				let is_static = is_static_field (IlProp p) in
				let name = p.pname in
				not (List.exists (function (IlProp _,_,n,s) -> s = is_static && name = n | _ -> false) !all_fields)
		) cls.cprops
		else
			cls.cprops
	in
	let cls = { cls with cmethods = List.map (fun v -> !v) meths; cprops = props } in
	let clsfields = (get_all_fields cls) @ added in
	let super_fields = !all_fields in
	all_fields := clsfields @ !all_fields;
	let refclsfields = (List.map (fun v -> ref v) clsfields) in
	let fold_field acc v =
		let f, _p, name, field_is_static = !v in
		let change, copy = match name with
		| _ when is_haxe_keyword name ->
			true, false
		| _ ->
			((field_is_static && List.exists (function | (_,_,n,false) -> name = n | _ -> false) !all_fields) ||
			(not field_is_static && match f with
			| IlMethod _ ->
				List.exists (function | ( (IlProp _ | IlField _),_,n,false) -> name = n | _ -> false) super_fields ||
				List.exists (function | ( (IlProp _ | IlField _),_,n,_s) -> name = n | _ -> false) clsfields
			| _ -> false)), true
		in
		if change then begin
			let name = "%" ^ name in
			let changed = change_name name f, _p, name, field_is_static in
			if not copy then
				v := changed;
			if copy then
				v :: ref changed :: acc
			else
				v :: acc
		end else
			v :: acc
	in
	let refclsfields = List.fold_left fold_field [] refclsfields in
	let fold (fields,methods,props) f = match !f with
		| IlField f,_,_,_ -> f :: fields,methods,props
		| IlMethod m,_,_,_ -> fields,m :: methods,props
		| IlProp p,_,_,_ -> fields,methods,p :: props
	in
	let fields, methods, props = List.fold_left fold ([],[],[]) refclsfields in
	{ cls with
		cfields = fields;
		cprops = props;
		cmethods = methods;
		cevents = List.filter (fun ev -> not (Hashtbl.mem all_events_name ev.ename)) cls.cevents;
	}

(* --- Main class conversion --- *)

let convert_ilclass ctx p ilcls = match ilcls.csuper with
	| Some { snorm = LClass ((["System"],[],"Enum"), []) } ->
		convert_ilenum ctx p ilcls
	| _ ->
		let flags = ref [HExtern] in
		let meta = ref [Meta.Native, [EConst (String (ilpath_to_cs ilcls.cpath,SDoubleQuotes) ), p], p] in
		let force_check = Common.raw_defined ctx.ncom "force-lib-check" in
		if not force_check then
			meta := (Meta.LibType,[],p) :: !meta;

		let is_interface = ref false in
		let is_abstract = ref false in
		let is_sealed = ref false in
		List.iter (fun f -> match f with
			| SSealed ->
				flags := HFinal :: !flags;
				is_sealed := true
			| SInterface ->
				is_interface := true;
				flags := HInterface :: !flags
			| SAbstract ->
				meta := (Meta.Custom ":abstract", [], p) :: !meta;
				is_abstract := true;
			| _ -> ()
		) ilcls.cflags.tdf_semantics;

		if !is_abstract && not !is_interface && not !is_sealed then flags := HAbstract :: !flags;

		(match ilcls.csuper with
			| Some { snorm = LClass ( (["System"],[],"Object"), [] ) } -> ()
			| Some ({ snorm = LClass ( (["System"],[],"ValueType"), [] ) } as s) ->
				flags := HExtends (get_type_path ctx (convert_signature ctx p s.snorm)) :: !flags;
				meta := (Meta.Struct,[],p) :: !meta
			| Some { snorm = LClass ( (["haxe";"lang"],[],"HxObject"), [] ) } ->
				meta := (Meta.HxGen,[],p) :: !meta
			| Some s ->
				flags := HExtends (get_type_path ctx (convert_signature ctx p s.snorm)) :: !flags
			| _ -> ());

			let has_explicit_ifaces = ref false in
			List.iter (fun i ->
				match i.snorm with
				| LClass ( (["haxe";"lang"],[], "IHxObject"), _ ) ->
					meta := (Meta.HxGen,[],p) :: !meta
				| i ->
					if is_explicit ctx ilcls i then has_explicit_ifaces := true;
					flags := if !is_interface then
						HExtends (get_type_path ctx (convert_signature ctx p i)) :: !flags
					else
						HImplements (get_type_path ctx (convert_signature ctx p i)) :: !flags
			) ilcls.cimplements;
			if !has_explicit_ifaces && force_check then
				meta := (Meta.LibType,[],p) :: !meta;

			(* ArrayAccess *)
			ignore (List.exists (function
			| { psig = { snorm = LMethod(_,ret,[_v]) } } ->
				flags := if !is_interface then
					(HExtends (raw_type_path ctx ([],"ArrayAccess") [ TPType (convert_signature ctx p ret,null_pos) ]) :: !flags)
				else
					(HImplements (raw_type_path ctx ([],"ArrayAccess") [ TPType (convert_signature ctx p ret,null_pos) ]) :: !flags);
				true
			| _ -> false) ilcls.cprops);

			let fields = ref [] in
			let run_fields fn f =
				List.iter (fun f ->
					try
						fields := fn f :: !fields
					with
						| Exit -> ()
				) f
			in
			let meths = if !is_interface then
					List.filter (fun m -> m.moverride = None) ilcls.cmethods
				else
					ilcls.cmethods
			in
			run_fields (fun m ->
				convert_ilmethod ctx p ilcls.cpath !is_interface m (List.exists (fun m2 -> m != m2 && String.length m2.mname > 0 && m2.mname.[0] <> '.' && String.length m2.mname > String.length m.mname + 1 && String.sub m2.mname (String.length m2.mname - String.length m.mname - 1) (String.length m.mname + 1) = "." ^ m.mname) meths)
			) meths;
			run_fields (convert_ilfield ctx p ilcls.cpath) ilcls.cfields;
			run_fields (fun prop ->
				convert_ilprop ctx p ilcls.cpath prop (List.exists (fun p2 -> prop != p2 && String.length p2.pname > 0 && p2.pname.[0] <> '.' && String.length p2.pname > String.length prop.pname + 1 && String.sub p2.pname (String.length p2.pname - String.length prop.pname - 1) (String.length prop.pname + 1) = "." ^ prop.pname) ilcls.cprops)
			) ilcls.cprops;
			run_fields (convert_ilevent ctx p ilcls.cpath) ilcls.cevents;

			let params = List.map (fun tp ->
				{
					tp_name = "T" ^ string_of_int tp.tnumber,null_pos;
					tp_params = [];
					tp_constraints = None;
					tp_default = None;
					tp_meta = [];
				}) ilcls.ctypes
			in
			let _, c = (match Hashtbl.find_opt il_to_hx_path ilcls.cpath with
				| Some p -> p
				| None -> netpath_to_hx ctx.nstd ilcls.cpath
			) in
			EClass {
				d_name = netname_to_hx c,null_pos;
				d_doc = lookup_type_doc ctx ilcls.cpath;
				d_params = params;
				d_meta = !meta;
				d_flags = !flags;
				d_data = !fields;
			}

(* --- DLL type reference collection for stub generation --- *)

(** Recursively extract ilpath references from an ilsig_norm *)
let rec collect_sig_refs acc = function
	| LClass(path, args) | LValueType(path, args) ->
		let acc = path :: acc in
		List.fold_left collect_sig_refs acc args
	| LVector s | LPointer s | LManagedPointer s ->
		collect_sig_refs acc s
	| LArray(s, _) ->
		collect_sig_refs acc s
	| LMethod(_, ret, args) ->
		let acc = collect_sig_refs acc ret in
		List.fold_left collect_sig_refs acc args
	| _ -> acc

(** Collect all externally-referenced ilpaths from an ilclass *)
let collect_ilclass_refs ilcls =
	let refs = ref [] in
	let add_sig s = refs := collect_sig_refs !refs s.snorm in
	(match ilcls.csuper with Some s -> add_sig s | None -> ());
	List.iter add_sig ilcls.cimplements;
	List.iter (fun f -> add_sig f.fsig) ilcls.cfields;
	List.iter (fun m ->
		add_sig m.msig; add_sig m.mret;
		List.iter (fun (_,_,s) -> add_sig s) m.margs
	) ilcls.cmethods;
	List.iter (fun p -> add_sig p.psig) ilcls.cprops;
	List.iter (fun e -> add_sig e.esig) ilcls.cevents;
	!refs

(* --- .NET library class --- *)

class net_library com name file_path std = object(self)
	inherit [net_lib_type,unit] native_library name file_path

	val mutable ilctx = None
	val cache = Hashtbl.create 0
	val net_path_map : (path, IlData.ilpath) Hashtbl.t = Hashtbl.create 0

	method private netpath_to_hx =
		netpath_to_hx std

	method load =
		let r = PeReader.create_r (open_in_bin file_path) com.Common.defines.Define.values in
		let ctx = PeReader.read r in
		let clr_header = PeReader.read_clr_header ctx in
		let il_cache = IlMetaReader.create_cache () in
		let meta = IlMetaReader.read_meta_tables ctx clr_header il_cache in
		close_in (r.PeReader.ch);
		if Common.raw_defined com "net_loader_debug" then
			print_endline ("for lib " ^ file_path);
		let il_typedefs = Hashtbl.copy meta.il_typedefs in
		Hashtbl.clear meta.il_typedefs;
		(* Pass 1: Detect arity conflicts (same namespace+base name, different arities) *)
		let base_name_counts : (string list * string, int) Hashtbl.t = Hashtbl.create 0 in
		Hashtbl.iter (fun _ td ->
			let path = IlMetaTools.get_path (TypeDef td) in
			let key = ilpath_base_key path in
			let count = try Hashtbl.find base_name_counts key with Not_found -> 0 in
			Hashtbl.replace base_name_counts key (count + 1)
		) il_typedefs;
		(* Pass 2: Build path mappings with smart arity suffix stripping *)
		Hashtbl.iter (fun _ td ->
			let path = IlMetaTools.get_path (TypeDef td) in
			let key = ilpath_base_key path in
			let has_conflict = (try Hashtbl.find base_name_counts key with Not_found -> 0) > 1 in
			let strip_arity = not has_conflict in
			let hx_path = netpath_to_hx_smart ~strip_arity std path in
			if Common.raw_defined com "net_loader_debug" then
				Printf.printf "found %s\n" (s_type_path hx_path);
			Hashtbl.replace net_path_map hx_path path;
			Hashtbl.replace il_to_hx_path path hx_path;
			Hashtbl.replace meta.il_typedefs path td
		) il_typedefs;
		let docs = load_doc file_path in
		let meta = { nstd = std; ncom = com; nil = meta; ndocs = docs } in
		ilctx <- Some meta

	method get_ctx = match ilctx with
		| None ->
			self#load;
			self#get_ctx
		| Some ctx ->
			ctx

	method close =
		()

	method private ilpath_to_hx ilpath =
		match Hashtbl.find_opt il_to_hx_path ilpath with
		| Some hx_path -> hx_path
		| None -> netpath_to_hx std ilpath

	method list_modules =
		Hashtbl.fold (fun path _ acc -> match path with
			| _,_ :: _, _ -> acc
			| _ -> self#ilpath_to_hx path :: acc) (self#get_ctx).nil.il_typedefs []

	method lookup path : net_lib_type =
		try
			Hashtbl.find cache path
		with | Not_found -> try
			let ctx = self#get_ctx in
			let ilpath = Hashtbl.find net_path_map path in
			let cls = IlMetaTools.convert_class ctx.nil ilpath in
			let cls = normalize_ilcls ctx cls in
			(* Collect external type references for stub generation *)
			let ref_ilpaths = collect_ilclass_refs cls in
			List.iter (fun ref_ilpath ->
				let ref_hx_path = self#ilpath_to_hx ref_ilpath in
				if not (Hashtbl.mem net_path_map ref_hx_path) then
					Hashtbl.replace dll_referenced_types ref_hx_path true
			) ref_ilpaths;
			Hashtbl.add cache path (Some cls);
			Some cls
		with | Not_found ->
			Hashtbl.add cache path None;
			None

	method build (path : path) (_p : pos) : Ast.package option =
		let p = { pfile = file_path ^ " @ " ^ s_type_path path; pmin = 0; pmax = 0; } in
		let pack = match fst path with | ["haxe";"root"] -> [] | p -> p in
		let cp = ref [] in
		let rec build path = try
			if Common.raw_defined com "net_loader_debug" then
				Printf.printf "looking up %s\n" (s_type_path path);
			match self#lookup path with
			| Some({csuper = Some{snorm = LClass( (["System"],[],("Delegate"|"MulticastDelegate")),_)}} as cls)
				when List.mem SSealed cls.cflags.tdf_semantics ->
				let ctx = self#get_ctx in
				let hxcls = convert_ilclass ctx p cls in
				cp := (hxcls,p) :: !cp;
				List.iter (fun ilpath ->
					let path = self#ilpath_to_hx ilpath in
					build path
				) cls.cnested
			| Some cls when not (is_compiler_generated cls) ->
				let ctx = self#get_ctx in
				let hxcls = convert_ilclass ctx p cls in
				cp := (hxcls,p) :: !cp;
				List.iter (fun ilpath ->
					let path = self#ilpath_to_hx ilpath in
					build path
				) cls.cnested
			| _ -> ()
		with | Not_found | Exit ->
			()
		in
		build path;
		match !cp with
			| [] -> None
			| cp -> Some (pack,cp)

	method get_data = ()

	initializer
		if std then self#add_flag FlagIsStd
end

let add_net_lib com file is_extern =
	let real_file = if Sys.file_exists file then
		file
	else try Common.find_file com file with
		| Not_found -> try Common.find_file com (file ^ ".dll") with
		| Not_found ->
			failwith (".NET lib " ^ file ^ " not found")
	in
	let net_lib = new net_library com file real_file false in
	if is_extern then net_lib#add_flag FlagIsExtern;
	com.Common.native_libs.net_libs <- (net_lib :> (net_lib_type,unit) native_library) :: com.native_libs.net_libs;
	CommonCache.handle_native_lib com net_lib

(** Generate a typedef = Dynamic stub module for a missing .NET type *)
let generate_stub_module path p =
	let pack = fst path in
	let name = snd path in
	let meta = [
		Meta.Custom ":compilerGenerated", [], p;
	] in
	let decl = ETypedef {
		d_name = name, null_pos;
		d_doc = None;
		d_params = [];
		d_meta = meta;
		d_flags = [];
		d_data = (CTPath (make_ptp { tpackage = []; tname = "Dynamic"; tparams = []; tsub = None } null_pos), null_pos);
	} in
	(pack, [(decl, p)])

(** Register a catch-all stub provider as the last load_extern_type handler.
    Only generates stubs for types that a loaded DLL references but that
    aren't defined in any loaded DLL or .hx file. *)
let register_stub_provider com =
	if com.Common.platform <> Globals.Cs then ()
	else begin
		let suppress_warnings = Common.raw_defined com "no-net-stub-warnings" in
		let build path p =
			try Some (Hashtbl.find stub_cache path)
			with Not_found ->
				if Hashtbl.mem dll_referenced_types path then begin
					if not suppress_warnings && Common.raw_defined com "net_loader_debug" then
						Printf.printf "(net-lib) Stub: %s not found in any loaded DLL, using Dynamic stub\n%!" (s_type_path path);
					let stub = generate_stub_module path p in
					Hashtbl.replace stub_cache path stub;
					Some stub
				end else
					None
		in
		com.load_extern_type <- com.load_extern_type @ ["<net-stubs>", build]
	end

(** Auto-load .NET standard libraries from hxcs.
    Scans class paths for {net_std}/{net_target}-{net_ver}/ directory
    and loads all .dll files found within it.
    Configurable via -D net-target (default: netstandard), -D net-ver (default: 2.1),
    and --net-std (default: netlib).
    Returns a list of init functions to call (like add_net_lib). *)
let maybe_load_net_std com net_std_base =
	if com.Common.platform <> Globals.Cs then []
	else begin
		let net_target = try Define.raw_defined_value com.defines "net_target"
			with Not_found -> "netstandard" in
		let net_ver = try Define.raw_defined_value com.defines "net_ver"
			with Not_found -> "2.1" in
		let base_path = match net_std_base with Some p -> p | None -> "netlib" in
		let dir_rel = Printf.sprintf "%s/%s-%s" base_path net_target net_ver in
		(* Try to find the directory via class paths *)
		let found_dir = ref None in
		com.class_paths#iter (fun cp ->
			if !found_dir = None then begin
				let dir = cp#path ^ dir_rel in
				if Sys.file_exists dir && Sys.is_directory dir then
					found_dir := Some dir
			end
		);
		match !found_dir with
		| None -> []
		| Some dir ->
			if Common.raw_defined com "net_loader_debug" then
				Printf.printf "(net-lib) Auto-loading .NET libs from %s\n" dir;
			let entries = Sys.readdir dir in
			Array.to_list entries
			|> List.filter (fun f -> Filename.check_suffix f ".dll")
			|> List.map (fun f ->
				let file = Filename.concat dir f in
				if Common.raw_defined com "net_loader_debug" then
					Printf.printf "(net-lib)   Loading %s\n" file;
				add_net_lib com file false
			)
	end
