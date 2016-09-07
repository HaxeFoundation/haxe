(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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

(*
	Conventions:
	- e: expression (typed or untyped)
	- c: class
	- en: enum
	- td: typedef (tdef)
	- a: abstract
	- an: anon
	- tf: tfunc
	- cf: class_field
	- ef: enum_field
	- t: type (t)
	- ct: complex_type
	- v: local variable (tvar)
	- m: module (module_def)
	- mt: module_type
	- p: pos

	"param" refers to type parameters
	"arg" refers to function arguments
	leading s_ means function returns string
	trailing l means list (but we also use natural plurals such as "metas")
	semantic suffixes may be used freely (e.g. e1, e_if, e')
*)

open Printf
open Ast
open Genswf
open Common
open Common.DisplayMode
open Type
open Server

exception Abort

let executable_path() =
	Extc.executable_path()

let format msg p =
	if p = Ast.null_pos then
		msg
	else begin
		let error_printer file line = sprintf "%s:%d:" file line in
		let epos = Lexer.get_error_pos error_printer p in
		let msg = String.concat ("\n" ^ epos ^ " : ") (ExtString.String.nsplit msg "\n") in
		sprintf "%s : %s" epos msg
	end

let message ctx msg p =
	ctx.messages <- format msg p :: ctx.messages

let deprecated = [
	"Type not found : IntIter","IntIter was renamed to IntIterator";
	"EReg has no field customReplace","EReg.customReplace was renamed to EReg.map";
	"#StringTools has no field isEOF","StringTools.isEOF was renamed to StringTools.isEof";
	"Type not found : haxe.BaseCode","haxe.BaseCode was moved to haxe.crypto.BaseCode";
	"Type not found : haxe.Md5","haxe.Md5 was moved to haxe.crypto.Md5";
	"Type not found : haxe.SHA1","haxe.SHA1 was moved to haxe.crypto.SHA1";
	"Type not found : Hash","Hash has been removed, use Map instead";
	"Type not found : IntHash","IntHash has been removed, use Map instead";
	"Type not found : haxe.FastList","haxe.FastList was moved to haxe.ds.GenericStack";
	"#Std has no field format","Std.format has been removed, use single quote 'string ${escape}' syntax instead";
	"Identifier 'EType' is not part of enum haxe.macro.ExprDef","EType has been removed, use EField instead";
	"Identifier 'CType' is not part of enum haxe.macro.Constant","CType has been removed, use CIdent instead";
	"Type not found : haxe.rtti.Infos","Use @:rtti instead of implementing haxe.rtti.Infos";
	"Type not found : haxe.rtti.Generic","Use @:generic instead of implementing haxe.Generic";
	"Type not found : flash.utils.TypedDictionary","flash.utils.TypedDictionary has been removed, use Map instead";
	"Type not found : haxe.Stack", "haxe.Stack has been renamed to haxe.CallStack";
	"Type not found : neko.zip.Reader", "neko.zip.Reader has been removed, use haxe.zip.Reader instead";
	"Type not found : neko.zip.Writer", "neko.zip.Writer has been removed, use haxe.zip.Writer instead";
	"Type not found : haxe.Public", "Use @:publicFields instead of implementing or extending haxe.Public";
	"#Xml has no field createProlog", "Xml.createProlog was renamed to Xml.createProcessingInstruction";
	"Duplicate type parameter name: Const", "Multiple Const type parameters are no longer allowed, use @:const T instead"
]

let limit_string s offset =
	let rest = 80 - offset in
	let words = ExtString.String.nsplit s " " in
	let rec loop i words = match words with
		| word :: words ->
			if String.length word + i + 1 > rest then (Printf.sprintf "\n%*s" offset "") :: word :: loop (String.length word) words
			else (if i = 0 then "" else " ") :: word :: loop (i + 1 + String.length word) words
		| [] ->
			[]
	in
	String.concat "" (loop 0 words)

let error ctx msg p =
	let msg = try List.assoc msg deprecated with Not_found -> msg in
	message ctx msg p;
	ctx.has_error <- true

let reserved_flags = [
	"cross";"js";"lua";"neko";"flash";"php";"cpp";"cs";"java";"python";
	"as3";"swc";"macro";"sys"
	]

let unique l =
	let rec _unique = function
		| [] -> []
		| x1 :: x2 :: l when x1 = x2 -> _unique (x2 :: l)
		| x :: l -> x :: _unique l
	in
	_unique (List.sort compare l)

let rec read_type_path com p =
	let classes = ref [] in
	let packages = ref [] in
	let p = (match p with
		| x :: l ->
			(try
				match PMap.find x com.package_rules with
				| Directory d -> d :: l
				| Remap s -> s :: l
				| _ -> p
			with
				Not_found -> p)
		| _ -> p
	) in
	List.iter (fun path ->
		let dir = path ^ String.concat "/" p in
		let r = (try Sys.readdir dir with _ -> [||]) in
		Array.iter (fun f ->
			if (try (Unix.stat (dir ^ "/" ^ f)).Unix.st_kind = Unix.S_DIR with _ -> false) then begin
				if f.[0] >= 'a' && f.[0] <= 'z' then begin
					if p = ["."] then
						match read_type_path com [f] with
						| [] , [] -> ()
						| _ ->
							try
								match PMap.find f com.package_rules with
								| Forbidden -> ()
								| Remap f -> packages := f :: !packages
								| Directory _ -> raise Not_found
							with Not_found ->
								packages := f :: !packages
					else
						packages := f :: !packages
				end;
			end else if file_extension f = "hx" then begin
				let c = Filename.chop_extension f in
				if String.length c < 2 || String.sub c (String.length c - 2) 2 <> "__" then classes := c :: !classes;
			end;
		) r;
	) com.class_path;
	List.iter (fun (_,_,extract) ->
		Hashtbl.iter (fun (path,name) _ ->
			if path = p then classes := name :: !classes else
			let rec loop p1 p2 =
				match p1, p2 with
				| [], _ -> ()
				| x :: _, [] -> packages := x :: !packages
				| a :: p1, b :: p2 -> if a = b then loop p1 p2
			in
			loop path p
		) (extract());
	) com.swf_libs;
	List.iter (fun (path,std,close,all_files,lookup) ->
		List.iter (fun (path, name) ->
			if path = p then classes := name :: !classes else
			let rec loop p1 p2 =
				match p1, p2 with
				| [], _ -> ()
				| x :: _, [] -> packages := x :: !packages
				| a :: p1, b :: p2 -> if a = b then loop p1 p2
			in
			loop path p
		) (all_files())
	) com.java_libs;
	List.iter (fun (path,std,all_files,lookup) ->
		List.iter (fun (path, name) ->
			if path = p then classes := name :: !classes else
			let rec loop p1 p2 =
				match p1, p2 with
				| [], _ -> ()
				| x :: _, [] -> packages := x :: !packages
				| a :: p1, b :: p2 -> if a = b then loop p1 p2
			in
		loop path p
		) (all_files())
	) com.net_libs;
	unique !packages, unique !classes

(** raise field completion listing packages and modules in a given package *)
let complete_type_path ctx p =
	let packs, modules = read_type_path ctx.com p in
	if packs = [] && modules = [] then
		(error ctx ("No classes found in " ^ String.concat "." p) Ast.null_pos;
		None)
	else
		let convert k f = (f,"",Some k,"") in
		let packs = List.map (convert Display.FKPackage) packs in
		let modules = List.map (convert Display.FKType) modules in
		Some (packs @ modules)

(** raise field completion listing module sub-types and static fields *)
let complete_type_path_inner ctx p c cur_package is_import =
	let com = ctx.com in
	try
		let sl_pack,s_module = match List.rev p with
			| s :: sl when s.[0] >= 'A' && s.[0] <= 'Z' -> List.rev sl,s
			| _ -> p,c
		in
		let ctx = Typer.create com in
		let rec lookup p =
			try
				Typeload.load_module ctx (p,s_module) Ast.null_pos
			with e ->
				if cur_package then
					match List.rev p with
					| [] -> raise e
					| _ :: p -> lookup (List.rev p)
				else
					raise e
		in
		let m = lookup sl_pack in
		let statics = ref None in
		let public_types = List.filter (fun t ->
			let tinfos = t_infos t in
			let is_module_type = snd tinfos.mt_path = c in
			if is_import && is_module_type then begin match t with
				| TClassDecl c ->
					ignore(c.cl_build());
					statics := Some c.cl_ordered_statics
				| _ -> ()
			end;
			not tinfos.mt_private
		) m.m_types in
		let types =
			if c <> s_module then
				[]
			else
				List.map (fun t ->
					let t = t_infos t in
					(snd t.mt_path), "", Some Display.FKType, (Option.default "" t.mt_doc)
				) public_types
		in
		let ctx = print_context() in
		let make_field_doc cf =
			cf.cf_name,
			s_type ctx cf.cf_type,
			Some (match cf.cf_kind with Method _ -> Display.FKMethod | Var _ -> Display.FKVar),
			(match cf.cf_doc with Some s -> s | None -> "")
		in
		let fields = match !statics with
			| None -> types
			| Some cfl -> types @ (List.map make_field_doc (List.filter (fun cf -> cf.cf_public) cfl))
		in
		Some fields
	with _ ->
		error ctx ("Could not load module " ^ (Ast.s_type_path (p,c))) Ast.null_pos;
		None

let delete_file f = try Sys.remove f with _ -> ()

let expand_env ?(h=None) path  =
	let r = Str.regexp "%\\([A-Za-z0-9_]+\\)%" in
	Str.global_substitute r (fun s ->
		let key = Str.matched_group 1 s in
		try
			Sys.getenv key
		with Not_found -> try
			match h with
			| None -> raise Not_found
			| Some h -> Hashtbl.find h key
		with Not_found ->
			"%" ^ key ^ "%"
	) path

let add_libs com libs =
	let call_haxelib() =
		let t = Common.timer "haxelib" in
		let cmd = "haxelib path " ^ String.concat " " libs in
		let pin, pout, perr = Unix.open_process_full cmd (Unix.environment()) in
		let lines = Std.input_list pin in
		let err = Std.input_list perr in
		let ret = Unix.close_process_full (pin,pout,perr) in
		if ret <> Unix.WEXITED 0 then failwith (match lines, err with
			| [], [] -> "Failed to call haxelib (command not found ?)"
			| [], [s] when ExtString.String.ends_with (ExtString.String.strip s) "Module not found : path" -> "The haxelib command has been strip'ed, please install it again"
			| _ -> String.concat "\n" (lines@err));
		t();
		lines
	in
	match libs with
	| [] -> []
	| _ ->
		let lines = match !global_cache with
			| Some cache ->
				(try
					(* if we are compiling, really call haxelib since library path might have changed *)
					if not com.display.dms_display then raise Not_found;
					Hashtbl.find cache.c_haxelib libs
				with Not_found ->
					let lines = call_haxelib() in
					Hashtbl.replace cache.c_haxelib libs lines;
					lines)
			| _ -> call_haxelib()
		in
		let extra_args = ref [] in
		let lines = List.fold_left (fun acc l ->
			let l = ExtString.String.strip l in
			if l = "" then acc else
			if l.[0] <> '-' then l :: acc else
			match (try ExtString.String.split l " " with _ -> l, "") with
			| ("-L",dir) ->
				com.neko_libs <- String.sub l 3 (String.length l - 3) :: com.neko_libs;
				acc
			| param, value ->
				extra_args := param :: !extra_args;
				if value <> "" then extra_args := value :: !extra_args;
				acc
		) [] lines in
		com.class_path <- lines @ com.class_path;
		List.rev !extra_args

let run_command ctx cmd =
	let h = Hashtbl.create 0 in
	Hashtbl.add h "__file__" ctx.com.file;
	Hashtbl.add h "__platform__" (platform_name ctx.com.platform);
	let t = Common.timer "command" in
	let cmd = expand_env ~h:(Some h) cmd in
	let len = String.length cmd in
	if len > 3 && String.sub cmd 0 3 = "cd " then begin
		Sys.chdir (String.sub cmd 3 (len - 3));
		0
	end else
	let binary_string s =
		if not Globals.is_windows then s else String.concat "\n" (Str.split (Str.regexp "\r\n") s)
	in
	let pout, pin, perr = Unix.open_process_full cmd (Unix.environment()) in
	let iout = Unix.descr_of_in_channel pout in
	let ierr = Unix.descr_of_in_channel perr in
	let berr = Buffer.create 0 in
	let bout = Buffer.create 0 in
	let tmp = String.create 1024 in
	let result = ref None in
	(*
		we need to read available content on process out/err if we want to prevent
		the process from blocking when the pipe is full
	*)
	let is_process_running() =
		let pid, r = Unix.waitpid [Unix.WNOHANG] (-1) in
		if pid = 0 then
			true
		else begin
			result := Some r;
			false;
		end
	in
	let rec loop ins =
		let (ch,_,_), timeout = (try Unix.select ins [] [] 0.02, true with _ -> ([],[],[]),false) in
		match ch with
		| [] ->
			(* make sure we read all *)
			if timeout && is_process_running() then
				loop ins
			else begin
				Buffer.add_string berr (IO.read_all (IO.input_channel perr));
				Buffer.add_string bout (IO.read_all (IO.input_channel pout));
			end
		| s :: _ ->
			let n = Unix.read s tmp 0 (String.length tmp) in
			if s == iout && n > 0 then
				ctx.com.print (String.sub tmp 0 n)
			else
				Buffer.add_substring (if s == iout then bout else berr) tmp 0 n;
			loop (if n = 0 then List.filter ((!=) s) ins else ins)
	in
	(try loop [iout;ierr] with Unix.Unix_error _ -> ());
	let serr = binary_string (Buffer.contents berr) in
	let sout = binary_string (Buffer.contents bout) in
	if serr <> "" then ctx.messages <- (if serr.[String.length serr - 1] = '\n' then String.sub serr 0 (String.length serr - 1) else serr) :: ctx.messages;
	if sout <> "" then ctx.com.print sout;
	let r = (match (try Unix.close_process_full (pout,pin,perr) with Unix.Unix_error (Unix.ECHILD,_,_) -> (match !result with None -> assert false | Some r -> r)) with
		| Unix.WEXITED e -> e
		| Unix.WSIGNALED s | Unix.WSTOPPED s -> if s = 0 then -1 else s
	) in
	t();
	r

let display_memory ctx =
	let verbose = ctx.com.verbose in
	let print = print_endline in
	let fmt_size sz =
		if sz < 1024 then
			string_of_int sz ^ " B"
		else if sz < 1024*1024 then
			string_of_int (sz asr 10) ^ " KB"
		else
			Printf.sprintf "%.1f MB" ((float_of_int sz) /. (1024.*.1024.))
	in
	let size v =
		fmt_size (mem_size v)
	in
	Gc.full_major();
	Gc.compact();
	let mem = Gc.stat() in
	print ("Total Allocated Memory " ^ fmt_size (mem.Gc.heap_words * (Sys.word_size asr 8)));
	print ("Free Memory " ^ fmt_size (mem.Gc.free_words * (Sys.word_size asr 8)));
	(match !global_cache with
	| None ->
		print "No cache found";
	| Some c ->
		print ("Total cache size " ^ size c);
		print ("  haxelib " ^ size c.c_haxelib);
		print ("  parsed ast " ^ size c.c_files ^ " (" ^ string_of_int (Hashtbl.length c.c_files) ^ " files stored)");
		print ("  typed modules " ^ size c.c_modules ^ " (" ^ string_of_int (Hashtbl.length c.c_modules) ^ " modules stored)");
		let rec scan_module_deps m h =
			if Hashtbl.mem h m.m_id then
				()
			else begin
				Hashtbl.add h m.m_id m;
				PMap.iter (fun _ m -> scan_module_deps m h) m.m_extra.m_deps
			end
		in
		let all_modules = Hashtbl.fold (fun _ m acc -> PMap.add m.m_id m acc) c.c_modules PMap.empty in
		let modules = Hashtbl.fold (fun (path,key) m acc ->
			let mdeps = Hashtbl.create 0 in
			scan_module_deps m mdeps;
			let deps = ref [] in
			let out = ref all_modules in
			Hashtbl.iter (fun _ md ->
				out := PMap.remove md.m_id !out;
				if m == md then () else begin
				deps := Obj.repr md :: !deps;
				List.iter (fun t ->
					match t with
					| TClassDecl c ->
						deps := Obj.repr c :: !deps;
						List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_statics;
						List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_fields;
					| TEnumDecl e ->
						deps := Obj.repr e :: !deps;
						List.iter (fun n -> deps := Obj.repr (PMap.find n e.e_constrs) :: !deps) e.e_names;
					| TTypeDecl t -> deps := Obj.repr t :: !deps;
					| TAbstractDecl a -> deps := Obj.repr a :: !deps;
				) md.m_types;
				end
			) mdeps;
			let chk = Obj.repr Common.memory_marker :: PMap.fold (fun m acc -> Obj.repr m :: acc) !out [] in
			let inf = Objsize.objsize m !deps chk in
			(m,Objsize.size_with_headers inf, (inf.Objsize.reached,!deps,!out)) :: acc
		) c.c_modules [] in
		let cur_key = ref "" and tcount = ref 0 and mcount = ref 0 in
		List.iter (fun (m,size,(reached,deps,out)) ->
			let key = m.m_extra.m_sign in
			if key <> !cur_key then begin
				print (Printf.sprintf ("    --- CONFIG %s ----------------------------") (Digest.to_hex key));
				cur_key := key;
			end;
			let sign md =
				if md.m_extra.m_sign = key then "" else "(" ^ (try Digest.to_hex md.m_extra.m_sign with _ -> "???" ^ md.m_extra.m_sign) ^ ")"
			in
			print (Printf.sprintf "    %s : %s" (Ast.s_type_path m.m_path) (fmt_size size));
			(if reached then try
				incr mcount;
				let lcount = ref 0 in
				let leak l =
					incr lcount;
					incr tcount;
					print (Printf.sprintf "      LEAK %s" l);
					if !lcount >= 3 && !tcount >= 100 && not verbose then begin
						print (Printf.sprintf "      ...");
						raise Exit;
					end;
				in
				if (Objsize.objsize m deps [Obj.repr Common.memory_marker]).Objsize.reached then leak "common";
				PMap.iter (fun _ md ->
					if (Objsize.objsize m deps [Obj.repr md]).Objsize.reached then leak (Ast.s_type_path md.m_path ^ sign md);
				) out;
			with Exit ->
				());
			if verbose then begin
				print (Printf.sprintf "      %d total deps" (List.length deps));
				PMap.iter (fun _ md ->
					print (Printf.sprintf "      dep %s%s" (Ast.s_type_path md.m_path) (sign md));
				) m.m_extra.m_deps;
			end;
			flush stdout
		) (List.sort (fun (m1,s1,_) (m2,s2,_) ->
			let k1 = m1.m_extra.m_sign and k2 = m2.m_extra.m_sign in
			if k1 = k2 then s1 - s2 else if k1 > k2 then 1 else -1
		) modules);
		if !mcount > 0 then print ("*** " ^ string_of_int !mcount ^ " modules have leaks !");
		print "Cache dump complete")

module Initialize = struct
	let set_platform com pf file =
		if com.platform <> Cross then failwith "Multiple targets";
		Common.init_platform com pf;
		com.file <- file;
		if (pf = Flash) && file_extension file = "swc" then Common.define com Define.Swc

	let initialize_target ctx com classes =
		let add_std dir =
			com.class_path <- List.filter (fun s -> not (List.mem s com.std_path)) com.class_path @ List.map (fun p -> p ^ dir ^ "/_std/") com.std_path @ com.std_path
		in
		match com.platform with
			| Cross ->
				(* no platform selected *)
				set_platform com Cross "";
				"?"
			| Flash ->
				let rec loop = function
					| [] -> ()
					| (v,_) :: _ when v > com.flash_version -> ()
					| (v,def) :: l ->
						Common.raw_define com ("flash" ^ def);
						loop l
				in
				loop Common.flash_versions;
				Common.raw_define com "flash";
				com.package_rules <- PMap.remove "flash" com.package_rules;
				add_std "flash";
				"swf"
			| Neko ->
				add_std "neko";
				"n"
			| Js ->
				if not (PMap.exists (fst (Define.infos Define.JqueryVer)) com.defines) then
					Common.define_value com Define.JqueryVer "11204";

				let es_version =
					try
						int_of_string (Common.defined_value com Define.JsEs)
					with
					| Not_found ->
						(Common.define_value com Define.JsEs "5"; 5)
					| _ ->
						0
				in

				if es_version < 3 || es_version = 4 then (* we don't support ancient and there's no 4th *)
					failwith "Invalid -D js-es value";

				if es_version >= 5 then Common.raw_define com "js-es5"; (* backward-compatibility *)

				add_std "js";
				"js"
			| Lua ->
				add_std "lua";
				"lua"
			| Php ->
				add_std "php";
				"php"
			| Cpp ->
				Common.define_value com Define.HxcppApiLevel "330";
				add_std "cpp";
				if Common.defined com Define.Cppia then
					classes := (Path.parse_path "cpp.cppia.HostClasses" ) :: !classes;
				"cpp"
			| Cs ->
				let old_flush = ctx.flush in
				ctx.flush <- (fun () ->
					com.net_libs <- [];
					old_flush()
				);
				Gencs.before_generate com;
				add_std "cs"; "cs"
			| Java ->
				let old_flush = ctx.flush in
				ctx.flush <- (fun () ->
					List.iter (fun (_,_,close,_,_) -> close()) com.java_libs;
					com.java_libs <- [];
					old_flush()
				);
				Genjava.before_generate com;
				add_std "java"; "java"
			| Python ->
				add_std "python";
				"python"
			| Hl ->
				add_std "hl";
				"hl"
end

let generate tctx ext xml_out interp swf_header =
	let com = tctx.Typecore.com in
	(* check file extension. In case of wrong commandline, we don't want
		to accidentaly delete a source file. *)
	if file_extension com.file = ext then delete_file com.file;
	if com.platform = Flash || com.platform = Cpp then List.iter (Codegen.fix_overrides com) com.types;
	if Common.defined com Define.Dump then Codegen.Dump.dump_types com;
	if Common.defined com Define.DumpDependencies then Codegen.Dump.dump_dependencies com;
	begin match com.platform with
		| Neko when interp -> ()
		| Cpp when Common.defined com Define.Cppia -> ()
		| Cpp | Cs | Java | Php -> Common.mkdir_from_path (com.file ^ "/.")
		| _ -> Common.mkdir_from_path com.file
	end;
	if interp then begin
		let ctx = Interp.create com (Typer.make_macro_api tctx Ast.null_pos) in
		Interp.add_types ctx com.types (fun t -> ());
		(match com.main with
		| None -> ()
		| Some e -> ignore(Interp.eval_expr ctx e));
	end else if com.platform = Cross then
		()
	else begin
		let generate,name = match com.platform with
		| Flash when Common.defined com Define.As3 ->
			Genas3.generate,"AS3"
		| Flash ->
			Genswf.generate swf_header,"swf"
		| Neko ->
			Genneko.generate,"neko"
		| Js ->
			Genjs.generate,"js"
		| Lua ->
			Genlua.generate,"lua"
		| Php ->
			Genphp.generate,"php"
		| Cpp ->
			Gencpp.generate,"cpp"
		| Cs ->
			Gencs.generate,"cs"
		| Java ->
			Genjava.generate,"java"
		| Python ->
			Genpy.generate,"python"
		| Hl ->
			Genhl.generate,"hl"
		| Cross ->
			assert false
		in
		Common.log com ("Generating " ^ name ^ ": " ^ com.file);
		let t = Common.timer ("generate " ^ name) in
		generate com;
		t()
	end

let get_std_class_paths () =
	try
		let p = Sys.getenv "HAXE_STD_PATH" in
		let rec loop = function
			| drive :: path :: l ->
				if String.length drive = 1 && ((drive.[0] >= 'a' && drive.[0] <= 'z') || (drive.[0] >= 'A' && drive.[0] <= 'Z')) then
					(drive ^ ":" ^ path) :: loop l
				else
					drive :: loop (path :: l)
			| l ->
				l
		in
		let parts = Str.split_delim (Str.regexp "[;:]") p in
		"" :: List.map Path.add_trailing_slash (loop parts)
	with Not_found ->
		if Sys.os_type = "Unix" then
			[
				"/usr/lib/haxe/std/";
				"/usr/share/haxe/std/";
				"/usr/local/lib/haxe/std/";
				"/usr/lib/haxe/extraLibs/";
				"/usr/local/lib/haxe/extraLibs/";
				""
			]
		else
			let base_path = Path.add_trailing_slash (Path.get_real_path (try executable_path() with _ -> "./")) in
			[
				base_path ^ "std/";
				base_path ^ "extraLibs/";
				""
			]

let rec process_params create pl =
	let each_params = ref [] in
	let rec loop acc = function
		| [] ->
			let ctx = create (!each_params @ (List.rev acc)) in
			init ctx;
			ctx.flush()
		| "--next" :: l when acc = [] -> (* skip empty --next *)
			loop [] l
		| "--next" :: l ->
			let ctx = create (!each_params @ (List.rev acc)) in
			ctx.has_next <- true;
			init ctx;
			ctx.flush();
			loop [] l
		| "--each" :: l ->
			each_params := List.rev acc;
			loop [] l
		| "--cwd" :: dir :: l ->
			(* we need to change it immediately since it will affect hxml loading *)
			(try Unix.chdir dir with _ -> raise (Arg.Bad ("Invalid directory: " ^ dir)));
			loop acc l
		| "--connect" :: hp :: l ->
			(match !global_cache with
			| None ->
				let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
				do_connect host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port")) ((List.rev acc) @ l)
			| Some _ ->
				(* already connected : skip *)
				loop acc l)
		| "--run" :: cl :: args ->
			let acc = cl :: "-main" :: "--interp" :: acc in
			let ctx = create (!each_params @ (List.rev acc)) in
			ctx.com.sys_args <- args;
			init ctx;
			ctx.flush()
		| arg :: l ->
			match List.rev (ExtString.String.nsplit arg ".") with
			| "hxml" :: _ when (match acc with "-cmd" :: _ -> false | _ -> true) ->
				let acc, l = (try acc, parse_hxml arg @ l with Not_found -> (arg ^ " (file not found)") :: acc, l) in
				loop acc l
			| _ -> loop (arg :: acc) l
	in
	(* put --display in front if it was last parameter *)
	let pl = (match List.rev pl with
		| file :: "--display" :: pl when file <> "memory" -> "--display" :: file :: List.rev pl
		| "use_rtti_doc" :: "-D" :: file :: "--display" :: pl -> "--display" :: file :: List.rev pl
		| _ -> pl
	) in
	loop [] pl



and init ctx =
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2016 Haxe Foundation\n Usage : haxe%s -main <class> [-swf|-js|-neko|-php|-cpp|-cppia|-as3|-cs|-java|-python|-hl|-lua] <output> [options]\n Options :"
		Globals.s_version (if Sys.os_type = "Win32" then ".exe" else "")
	in
	let com = ctx.com in
	let classes = ref [([],"Std")] in
try
	let xml_out = ref None in
	let swf_header = ref None in
	let cmds = ref [] in
	let config_macros = ref [] in
	let cp_libs = ref [] in
	let added_libs = Hashtbl.create 0 in
	let no_output = ref false in
	let did_something = ref false in
	let force_typing = ref false in
	let pre_compilation = ref [] in
	let interp = ref false in
	let swf_version = ref false in
	let evals = ref [] in
	Common.define_value com Define.HaxeVer (float_repres (float_of_int Globals.version /. 1000.));
	Common.raw_define com "haxe3";
	Common.define_value com Define.Dce "std";
	com.warning <- (fun msg p -> message ctx ("Warning : " ^ msg) p);
	com.error <- error ctx;
	if !global_cache <> None then com.run_command <- run_command ctx;
	Parser.display_error := (fun e p -> com.error (Parser.error_msg e) p);
	Parser.use_doc := !Common.display_default <> DMNone || (!global_cache <> None);
	com.class_path <- get_std_class_paths ();
	com.std_path <- List.filter (fun p -> ExtString.String.ends_with p "std/" || ExtString.String.ends_with p "std\\") com.class_path;
	let define f = Arg.Unit (fun () -> Common.define com f) in
	let process_ref = ref (fun args -> ()) in
	let process_libs() =
		let libs = List.filter (fun l -> not (Hashtbl.mem added_libs l)) (List.rev !cp_libs) in
		cp_libs := [];
		List.iter (fun l -> Hashtbl.add added_libs l ()) libs;
		(* immediately process the arguments to insert them at the place -lib was defined *)
		match add_libs com libs with
		| [] -> ()
		| args -> (!process_ref) args
	in
	let arg_delays = ref [] in
	let basic_args_spec = [
		("-cp",Arg.String (fun path ->
			process_libs();
			com.class_path <- Path.add_trailing_slash path :: com.class_path
		),"<path> : add a directory to find source files");
		("-js",Arg.String (Initialize.set_platform com Js),"<file> : compile code to JavaScript file");
		("-lua",Arg.String (Initialize.set_platform com Lua),"<file> : compile code to Lua file");
		("-swf",Arg.String (Initialize.set_platform com Flash),"<file> : compile code to Flash SWF file");
		("-as3",Arg.String (fun dir ->
			Initialize.set_platform com Flash dir;
			Common.define com Define.As3;
			Common.define com Define.NoInline;
		),"<directory> : generate AS3 code into target directory");
		("-neko",Arg.String (Initialize.set_platform com Neko),"<file> : compile code to Neko Binary");
		("-php",Arg.String (fun dir ->
			classes := (["php"],"Boot") :: !classes;
			Initialize.set_platform com Php dir;
		),"<directory> : generate PHP code into target directory");
		("-cpp",Arg.String (fun dir ->
			Initialize.set_platform com Cpp dir;
		),"<directory> : generate C++ code into target directory");
		("-cppia",Arg.String (fun file ->
			Initialize.set_platform com Cpp file;
			Common.define com Define.Cppia;
		),"<file> : generate Cppia code into target file");
		("-cs",Arg.String (fun dir ->
			cp_libs := "hxcs" :: !cp_libs;
			Initialize.set_platform com Cs dir;
		),"<directory> : generate C# code into target directory");
		("-java",Arg.String (fun dir ->
			cp_libs := "hxjava" :: !cp_libs;
			Initialize.set_platform com Java dir;
		),"<directory> : generate Java code into target directory");
		("-python",Arg.String (fun dir ->
			Initialize.set_platform com Python dir;
		),"<file> : generate Python code as target file");
		("-hl",Arg.String (fun file ->
			Initialize.set_platform com Hl file;
		),"<file> : compile HL code as target file");
		("-xml",Arg.String (fun file ->
			Parser.use_doc := true;
			xml_out := Some file
		),"<file> : generate XML types description");
		("-main",Arg.String (fun cl ->
			if com.main_class <> None then raise (Arg.Bad "Multiple -main");
			let cpath = Path.parse_type_path cl in
			com.main_class <- Some cpath;
			classes := cpath :: !classes
		),"<class> : select startup class");
		("-lib",Arg.String (fun l ->
			cp_libs := l :: !cp_libs;
			Common.raw_define com l;
		),"<library[:version]> : use a haxelib library");
		("-D",Arg.String (fun var ->
			begin match var with
				| "no_copt" | "no-copt" -> com.foptimize <- false;
				| "use_rtti_doc" | "use-rtti-doc" -> Parser.use_doc := true;
				| _ -> 	if List.mem var reserved_flags then raise (Arg.Bad (var ^ " is a reserved compiler flag and cannot be defined from command line"));
			end;
			Common.raw_define com var;
		),"<var[=value]> : define a conditional compilation flag");
		("-v",Arg.Unit (fun () ->
			com.verbose <- true
		),": turn on verbose mode");
		("-debug", Arg.Unit (fun() ->
			Common.define com Define.Debug;
			com.debug <- true;
		), ": add debug information to the compiled code");
	] in
	let adv_args_spec = [
		("-dce", Arg.String (fun mode ->
			(match mode with
			| "std" | "full" | "no" -> ()
			| _ -> raise (Arg.Bad "Invalid DCE mode, expected std | full | no"));
			Common.define_value com Define.Dce mode
		),"[std|full|no] : set the dead code elimination mode (default std)");
		("-swf-version",Arg.Float (fun v ->
			if not !swf_version || com.flash_version < v then com.flash_version <- v;
			swf_version := true;
		),"<version> : change the SWF version");
		("-swf-header",Arg.String (fun h ->
			try
				swf_header := Some (match ExtString.String.nsplit h ":" with
				| [width; height; fps] ->
					(int_of_string width,int_of_string height,float_of_string fps,0xFFFFFF)
				| [width; height; fps; color] ->
					let color = if ExtString.String.starts_with color "0x" then color else "0x" ^ color in
					(int_of_string width, int_of_string height, float_of_string fps, int_of_string color)
				| _ -> raise Exit)
			with
				_ -> raise (Arg.Bad "Invalid SWF header format, expected width:height:fps[:color]")
		),"<header> : define SWF header (width:height:fps:color)");
		("-swf-lib",Arg.String (fun file ->
			process_libs(); (* linked swf order matters, and lib might reference swf as well *)
			Genswf.add_swf_lib com file false
		),"<file> : add the SWF library to the compiled SWF");
		("-swf-lib-extern",Arg.String (fun file ->
			Genswf.add_swf_lib com file true
		),"<file> : use the SWF library for type checking");
		("-java-lib",Arg.String (fun file ->
			let std = file = "lib/hxjava-std.jar" in
			arg_delays := (fun () -> Genjava.add_java_lib com file std) :: !arg_delays;
		),"<file> : add an external JAR or class directory library");
		("-net-lib",Arg.String (fun file ->
			let file, is_std = match ExtString.String.nsplit file "@" with
				| [file] ->
					file,false
				| [file;"std"] ->
					file,true
				| _ -> raise Exit
			in
			arg_delays := (fun () -> Gencs.add_net_lib com file is_std) :: !arg_delays;
		),"<file>[@std] : add an external .NET DLL file");
		("-net-std",Arg.String (fun file ->
			Gencs.add_net_std com file
		),"<file> : add a root std .NET DLL search path");
		("-c-arg",Arg.String (fun arg ->
			com.c_args <- arg :: com.c_args
		),"<arg> : pass option <arg> to the native Java/C# compiler");
		("-x", Arg.String (fun file ->
			let neko_file = file ^ ".n" in
			Initialize.set_platform com Neko neko_file;
			if com.main_class = None then begin
				let cpath = Path.parse_type_path file in
				com.main_class <- Some cpath;
				classes := cpath :: !classes
			end;
			cmds := ("neko " ^ neko_file) :: !cmds;
		),"<file> : shortcut for compiling and executing a neko file");
		("-resource",Arg.String (fun res ->
			let file, name = (match ExtString.String.nsplit res "@" with
				| [file; name] -> file, name
				| [file] -> file, file
				| _ -> raise (Arg.Bad "Invalid Resource format, expected file@name")
			) in
			let file = (try Common.find_file com file with Not_found -> file) in
			let data = (try
				let s = Std.input_file ~bin:true file in
				if String.length s > 12000000 then raise Exit;
				s;
			with
				| Sys_error _ -> failwith ("Resource file not found : " ^ file)
				| _ -> failwith ("Resource '" ^ file ^ "' excess the maximum size of 12MB")
			) in
			if Hashtbl.mem com.resources name then failwith ("Duplicate resource name " ^ name);
			Hashtbl.add com.resources name data
		),"<file>[@name] : add a named resource file");
		("-prompt", Arg.Unit (fun() -> prompt := true),": prompt on error");
		("-cmd", Arg.String (fun cmd ->
			cmds := unquote cmd :: !cmds
		),": run the specified command after successful compilation");
		("--flash-strict", define Define.FlashStrict, ": more type strict flash API");
		("--no-traces", define Define.NoTraces, ": don't compile trace calls in the program");
		("--gen-hx-classes", Arg.Unit (fun() ->
			force_typing := true;
			pre_compilation := (fun() ->
				List.iter (fun (_,_,extract) ->
					Hashtbl.iter (fun n _ -> classes := n :: !classes) (extract())
				) com.swf_libs;
				List.iter (fun (_,std,_,all_files,_) ->
					if not std then
						List.iter (fun path -> if path <> (["java";"lang"],"String") then classes := path :: !classes) (all_files())
				) com.java_libs;
				List.iter (fun (_,std,all_files,_) ->
					if not std then
						List.iter (fun path -> classes := path :: !classes) (all_files())
				) com.net_libs;
			) :: !pre_compilation;
			xml_out := Some "hx"
		),": generate hx headers for all input classes");
		("--next", Arg.Unit (fun() -> assert false), ": separate several haxe compilations");
		("--each", Arg.Unit (fun() -> assert false), ": append preceding parameters to all haxe compilations separated by --next");
		("--display", Arg.String (fun file_pos ->
			match file_pos with
			| "classes" ->
				pre_compilation := (fun() -> raise (Parser.TypePath (["."],None,true))) :: !pre_compilation;
			| "keywords" ->
				raise (Completion (Display.print_keywords ()))
			| "memory" ->
				did_something := true;
				(try display_memory ctx with e -> prerr_endline (Printexc.get_backtrace ()));
			| "diagnostics" ->
				Common.define com Define.NoCOpt;
				com.display <- DisplayMode.create (DMDiagnostics true);
				Common.display_default := DMDiagnostics true;
			| _ ->
				let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format : " ^ file_pos) in
				let file = unquote file in
				let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
				let mode = match smode with
					| "position" ->
						Common.define com Define.NoCOpt;
						DMPosition
					| "usage" ->
						Common.define com Define.NoCOpt;
						DMUsage false
					(*| "rename" ->
						Common.define com Define.NoCOpt;
						DMUsage true*)
					| "package" ->
						DMPackage
					| "type" ->
						Common.define com Define.NoCOpt;
						DMType
					| "toplevel" ->
						Common.define com Define.NoCOpt;
						DMToplevel
					| "module-symbols" ->
						Common.define com Define.NoCOpt;
						DMModuleSymbols None;
					| "diagnostics" ->
						Common.define com Define.NoCOpt;
						DMDiagnostics false;
					| "statistics" ->
						Common.define com Define.NoCOpt;
						DMStatistics
					| "" ->
						DMDefault
					| _ ->
						let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
						match smode with
							| "resolve" ->
								DMResolve arg
							| "workspace-symbols" ->
								Common.define com Define.NoCOpt;
								DMModuleSymbols (Some arg)
							| _ ->
								DMDefault
				in
				let pos = try int_of_string pos with _ -> failwith ("Invalid format : "  ^ pos) in
				com.display <- DisplayMode.create mode;
				Common.display_default := mode;
				Common.define_value com Define.Display (if smode <> "" then smode else "1");
				Parser.use_doc := true;
				Parser.resume_display := {
					Ast.pfile = Path.unique_full_path file;
					Ast.pmin = pos;
					Ast.pmax = pos;
				};
		),": display code tips");
		("--no-output", Arg.Unit (fun() -> no_output := true),": compiles but does not generate any file");
		("--times", Arg.Unit (fun() -> measure_times := true),": measure compilation times");
		("--no-inline", define Define.NoInline, ": disable inlining");
		("--no-opt", Arg.Unit (fun() ->
			com.foptimize <- false;
			Common.define com Define.NoOpt;
		), ": disable code optimizations");
		("--php-front",Arg.String (fun f ->
			if com.php_front <> None then raise (Arg.Bad "Multiple --php-front");
			com.php_front <- Some f;
		),"<filename> : select the name for the php front file");
		("--php-lib",Arg.String (fun f ->
			if com.php_lib <> None then raise (Arg.Bad "Multiple --php-lib");
			com.php_lib <- Some f;
		),"<filename> : select the name for the php lib folder");
		("--php-prefix", Arg.String (fun f ->
			if com.php_prefix <> None then raise (Arg.Bad "Multiple --php-prefix");
			com.php_prefix <- Some f;
			Common.define com Define.PhpPrefix;
		),"<name> : prefix all classes with given name");
		("--remap", Arg.String (fun s ->
			let pack, target = (try ExtString.String.split s ":" with _ -> raise (Arg.Bad "Invalid remap format, expected source:target")) in
			com.package_rules <- PMap.add pack (Remap target) com.package_rules;
		),"<package:target> : remap a package to another one");
		("--interp", Arg.Unit (fun() ->
			Common.define com Define.Interp;
			Initialize.set_platform com Neko "";
			interp := true;
		),": interpret the program using internal macro system");
		("--macro", Arg.String (fun e ->
			force_typing := true;
			config_macros := e :: !config_macros
		)," : call the given macro before typing anything else");
		("--eval", Arg.String (fun s ->
			force_typing := true;
			evals := s :: !evals;
		), " : evaluates argument as Haxe module code");
		("--wait", Arg.String (fun hp ->
			let accept = match hp with
				| "stdio" ->
					Server.init_wait_stdio()
				| _ ->
					let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
					let port = try int_of_string port with _ -> raise (Arg.Bad "Invalid port") in
					init_wait_socket com.verbose host port
			in
			wait_loop process_params com.verbose accept
		),"<[host:]port> : wait on the given port for commands to run)");
		("--connect",Arg.String (fun _ ->
			assert false
		),"<[host:]port> : connect on the given port and run commands there)");
		("--cwd", Arg.String (fun dir ->
			assert false
		),"<dir> : set current working directory");
		("-version",Arg.Unit (fun() ->
			message ctx Globals.s_version Ast.null_pos;
			did_something := true;
		),": print version and exit");
		("--help-defines", Arg.Unit (fun() ->
			let m = ref 0 in
			let rec loop i =
				let d = Obj.magic i in
				if d <> Define.Last then begin
					let t, doc = Define.infos d in
					if String.length t > !m then m := String.length t;
					((String.concat "-" (ExtString.String.nsplit t "_")),doc) :: (loop (i + 1))
				end else
					[]
			in
			let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" !m n (limit_string doc (!m + 3))) all in
			List.iter (fun msg -> ctx.com.print (msg ^ "\n")) all;
			did_something := true
		),": print help for all compiler specific defines");
		("--help-metas", Arg.Unit (fun() ->
			let all,max_length = MetaInfo.get_documentation_list() in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" max_length n (limit_string doc (max_length + 3))) all in
			List.iter (fun msg -> ctx.com.print (msg ^ "\n")) all;
			did_something := true
		),": print help for all compiler metadatas");
	] in
	let args_callback cl =
		let path,name = Path.parse_path cl in
		if Path.starts_uppercase name then
			classes := (path,name) :: !classes
		else begin
			force_typing := true;
			config_macros := (Printf.sprintf "include('%s', true, null, null, true)" cl) :: !config_macros;
		end
	in
	let all_args_spec = basic_args_spec @ adv_args_spec in
	let process args =
		let current = ref 0 in
		(try
			Arg.parse_argv ~current (Array.of_list ("" :: List.map expand_env args)) all_args_spec args_callback usage;
			List.iter (fun fn -> fn()) !arg_delays
		with (Arg.Bad msg) as exc ->
			let r = Str.regexp "unknown option `\\([-A-Za-z]+\\)'" in
			try
				ignore(Str.search_forward r msg 0);
				let s = Str.matched_group 1 msg in
				let sl = List.map (fun (s,_,_) -> s) all_args_spec in
				let msg = StringError.string_error_raise s sl (Printf.sprintf "Invalid command: %s" s) in
				raise (Arg.Bad msg)
			with Not_found ->
				raise exc);
		arg_delays := []
	in
	process_ref := process;
	process ctx.com.args;
	process_libs();
	if com.display.dms_display then begin
		com.warning <- if com.display.dms_error_policy = EPCollect then (fun s p -> add_diagnostics_message com s p DisplayTypes.DiagnosticsSeverity.Warning) else message ctx;
		com.error <- error ctx;
	end;
	Display.process_display_file com classes;
	let ext = Initialize.initialize_target ctx com classes in
	(* if we are at the last compilation step, allow all packages accesses - in case of macros or opening another project file *)
	if com.display.dms_display then begin
		if not ctx.has_next then com.package_rules <- PMap.foldi (fun p r acc -> match r with Forbidden -> acc | _ -> PMap.add p r acc) com.package_rules PMap.empty;
	end;
	com.config <- get_config com; (* make sure to adapt all flags changes defined after platform *)

	List.iter (fun f -> f()) (List.rev (!pre_compilation));
	if !classes = [([],"Std")] && not !force_typing then begin
		let help_spec = basic_args_spec @ [
			("-help", Arg.Unit (fun () -> ()),": show extended help information");
			("--help", Arg.Unit (fun () -> ()),": show extended help information");
			("--help-defines", Arg.Unit (fun () -> ()),": print help for all compiler specific defines");
			("--help-metas", Arg.Unit (fun () -> ()),": print help for all compiler metadatas");
			("<dot-path>", Arg.Unit (fun () -> ()),": compile the module specified by dot-path");
		] in
		if !cmds = [] && not !did_something then Arg.usage help_spec usage;
	end else begin
		ctx.setup();
		Common.log com ("Classpath : " ^ (String.concat ";" com.class_path));
		Common.log com ("Defines : " ^ (String.concat ";" (PMap.foldi (fun k v acc -> (match v with "1" -> k | _ -> k ^ "=" ^ v) :: acc) com.defines [])));
		let t = Common.timer "typing" in
		Typecore.type_expr_ref := (fun ctx e with_type -> Typer.type_expr ctx e with_type);
		let tctx = Typer.create com in
		List.iter (Typer.call_init_macro tctx) (List.rev !config_macros);
		List.iter (Typer.eval tctx) !evals;
		List.iter (fun cpath -> ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath Ast.null_pos)) (List.rev !classes);
		Typer.finalize tctx;
		t();
		if ctx.has_error then raise Abort;
		if ctx.com.display.dms_exit_during_typing then begin
			if ctx.has_next then raise Abort;
			failwith "No completion point was found";
		end;
		let t = Common.timer "filters" in
		let main, types, modules = Typer.generate tctx in
		com.main <- main;
		com.types <- types;
		com.modules <- modules;
		Display.process_global_display_mode com tctx;
		Filters.run com tctx main;
		t();
		if ctx.has_error then raise Abort;
		(match !xml_out with
		| None -> ()
		| Some "hx" ->
			Genxml.generate_hx com
		| Some file ->
			Common.log com ("Generating xml : " ^ file);
			Common.mkdir_from_path file;
			Genxml.generate com file);
		if not !no_output then generate tctx ext !xml_out !interp !swf_header;
	end;
	Sys.catch_break false;
	List.iter (fun f -> f()) (List.rev com.callbacks.after_generation);
	if not !no_output then begin
		List.iter (fun c ->
			let r = run_command ctx c in
			if r <> 0 then failwith ("Command failed with error " ^ string_of_int r)
		) (List.rev !cmds)
	end
with
	| Abort ->
		()
	| Ast.Error (m,p) ->
		error ctx m p
	| Typecore.Fatal_error (m,p) ->
		error ctx m p
	| Common.Abort (m,p) ->
		error ctx m p
	| Lexer.Error (m,p) ->
		error ctx (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error ctx (Parser.error_msg m) p
	| Typecore.Forbid_package ((pack,m,p),pl,pf)  ->
		if !Common.display_default <> DMNone && ctx.has_next then begin
			ctx.has_error <- false;
			ctx.messages <- [];
		end else begin
			error ctx (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (Ast.s_type_path m) ) p;
			List.iter (error ctx "    referenced here") (List.rev pl);
		end
	| Typecore.Error (m,p) ->
		error ctx (Typecore.error_msg m) p
	| Interp.Error (msg,p :: l) ->
		message ctx msg p;
		List.iter (message ctx "Called from") l;
		error ctx "Aborted" Ast.null_pos;
	| Typeload.Generic_Exception(m,p) ->
		error ctx m p
	| Arg.Bad msg ->
		error ctx ("Error: " ^ msg) Ast.null_pos
	| Failure msg when not (is_debug_run()) ->
		error ctx ("Error: " ^ msg) Ast.null_pos
	| Arg.Help msg ->
		message ctx msg Ast.null_pos
	| Display.DisplayPackage pack ->
		raise (Completion (String.concat "." pack))
	| Display.DisplayFields fields ->
		let fields = List.map (
			fun (name,t,kind,doc) -> name, s_type (print_context()) t, kind, (Option.default "" doc)
		) fields in
		let fields =
			if !measure_times then begin
				close_times();
				(Display.get_timer_fields !start_time) @ fields
			end else
				fields
		in
		raise (Completion (Display.print_fields fields))
	| Display.DisplayType (t,p) ->
		raise (Completion (Display.print_type t p))
	| Display.DisplaySignatures tl ->
		raise (Completion (Display.print_signatures tl))
	| Display.DisplayPosition pl ->
		raise (Completion (Display.print_positions pl))
	| Display.DisplayToplevel il ->
		raise (Completion (Display.print_toplevel il))
	| Parser.TypePath (p,c,is_import) ->
		let fields = 
			match c with
			| None ->
				complete_type_path ctx p
			| Some (c,cur_package) ->
				complete_type_path_inner ctx p c cur_package is_import
		in
		Option.may (fun fields -> raise (Completion (Display.print_fields fields))) fields
	| Display.ModuleSymbols s | Display.Diagnostics s | Display.Statistics s | Display.Metadata s ->
		raise (Completion s)
	| Interp.Sys_exit i ->
		ctx.flush();
		exit i
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" || !global_cache <> None with _ -> true) && not (is_debug_run()) ->
		error ctx (Printexc.to_string e) Ast.null_pos

;;
let other = Common.timer "other" in
Sys.catch_break true;
let args = List.tl (Array.to_list Sys.argv) in
(try
	let server = Sys.getenv "HAXE_COMPILATION_SERVER" in
	let host, port = (try ExtString.String.split server ":" with _ -> "127.0.0.1", server) in
	do_connect host (try int_of_string port with _ -> failwith "Invalid HAXE_COMPILATION_SERVER port") args
with Not_found -> try
	process_params create_context args
with Completion c ->
	prerr_endline c;
	exit 0
| Arg.Bad msg ->
	prerr_endline ("Error: " ^ msg);
	exit 1
);
other();
if !measure_times then report_times prerr_endline
