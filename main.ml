(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
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
open Type

type context = {
	com : Common.context;
	mutable flush : unit -> unit;
	mutable setup : unit -> unit;
	mutable messages : string list;
	mutable has_next : bool;
	mutable has_error : bool;
}

type cache = {
	mutable c_haxelib : (string list, string list) Hashtbl.t;
	mutable c_files : (string, float * Ast.package) Hashtbl.t;
	mutable c_modules : (path * string, module_def) Hashtbl.t;
}

exception Abort
exception Completion of string


let version = 3300
let version_major = version / 1000
let version_minor = (version mod 1000) / 100
let version_revision = (version mod 100)
let version_is_stable = version_minor land 1 = 0

let measure_times = ref false
let prompt = ref false
let start_time = ref (get_time())
let global_cache = ref None

let path_sep = if Sys.os_type = "Unix" then "/" else "\\"

let get_real_path p =
	try
		Extc.get_real_path p
	with _ ->
		p

let executable_path() =
	Extc.executable_path()

let is_debug_run() =
	try Sys.getenv "HAXEDEBUG" = "1" with _ -> false

let s_version =
	Printf.sprintf "%d.%d.%d%s" version_major version_minor version_revision (match Version.version_extra with None -> "" | Some v -> " " ^ v)

let format msg p =
	if p = Ast.null_pos then
		msg
	else begin
		let error_printer file line = sprintf "%s:%d:" file line in
		let epos = Lexer.get_error_pos error_printer p in
		let msg = String.concat ("\n" ^ epos ^ " : ") (ExtString.String.nsplit msg "\n") in
		sprintf "%s : %s" epos msg
	end

let ssend sock str =
	let rec loop pos len =
		if len = 0 then
			()
		else
			let s = Unix.send sock str pos len [] in
			loop (pos + s) (len - s)
	in
	loop 0 (String.length str)

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
	"#Xml has no field createProlog", "Xml.createProlog was renamed to Xml.createProcessingInstruction"
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

let htmlescape s =
	let s = String.concat "&amp;" (ExtString.String.nsplit s "&") in
	let s = String.concat "&lt;" (ExtString.String.nsplit s "<") in
	let s = String.concat "&gt;" (ExtString.String.nsplit s ">") in
	s

let reserved_flags = [
	"cross";"js";"neko";"flash";"php";"cpp";"cs";"java";"python";
	"as3";"swc";"macro";"sys"
	]

let complete_fields com fields =
	let b = Buffer.create 0 in
	let details = Common.raw_defined com "display-details" in
	Buffer.add_string b "<list>\n";
	List.iter (fun (n,t,k,d) ->
		let s_kind = match k with
			| Some k -> (match k with
				| Typer.FKVar -> "var"
				| Typer.FKMethod -> "method"
				| Typer.FKType -> "type"
				| Typer.FKPackage -> "package")
			| None -> ""
		in
		if details then
			Buffer.add_string b (Printf.sprintf "<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n" n s_kind (htmlescape t) (htmlescape d))
		else
			Buffer.add_string b (Printf.sprintf "<i n=\"%s\"><t>%s</t><d>%s</d></i>\n" n (htmlescape t) (htmlescape d))
	) (List.sort (fun (a,_,ak,_) (b,_,bk,_) -> compare (ak,a) (bk,b)) fields);
	Buffer.add_string b "</list>\n";
	raise (Completion (Buffer.contents b))

let report_times print =
	let tot = ref 0. in
	Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
	print (Printf.sprintf "Total time : %.3fs" !tot);
	if !tot > 0. then begin
		print "------------------------------------";
		let timers = List.sort (fun t1 t2 -> compare t1.name t2.name) (Hashtbl.fold (fun _ t acc -> t :: acc) Common.htimers []) in
		List.iter (fun t -> print (Printf.sprintf "  %s : %.3fs, %.0f%%" t.name t.total (t.total *. 100. /. !tot))) timers
	end

let make_path f =
	let f = String.concat "/" (ExtString.String.nsplit f "\\") in
	let cl = ExtString.String.nsplit f "." in
	let cl = (match List.rev cl with
		| ["hx";path] -> ExtString.String.nsplit path "/"
		| _ -> cl
	) in
	let error msg =
		let msg = "Could not process argument " ^ f ^ "\n" ^ msg in
		failwith msg
	in
	let invalid_char x =
		for i = 1 to String.length x - 1 do
			match x.[i] with
			| 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
			| c -> error ("invalid character: " ^ (String.make 1 c))
		done
	in
	let rec loop = function
		| [] ->
			error "empty part"
		| [x] ->
			if String.length x = 0 then
				error "empty part"
			else if not (x.[0] = '_' || (x.[0] >= 'A' && x.[0] <= 'Z')) then
				error "Class name must start with uppercase character";
			invalid_char x;
			[],x
		| x :: l ->
			if String.length x = 0 then
				error "empty part"
			else if x.[0] < 'a' || x.[0] > 'z' then
				error "Package name must start with a lower case character";
			invalid_char x;
			let path,name = loop l in
			x :: path,name
	in
	loop cl

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

let unquote v =
	let len = String.length v in
	if len > 0 && v.[0] = '"' && v.[len - 1] = '"' then String.sub v 1 (len - 2) else v

let parse_hxml_data data =
	let lines = Str.split (Str.regexp "[\r\n]+") data in
	List.concat (List.map (fun l ->
		let l = unquote (ExtString.String.strip l) in
		if l = "" || l.[0] = '#' then
			[]
		else if l.[0] = '-' then
			try
				let a, b = ExtString.String.split l " " in
				[unquote a; unquote (ExtString.String.strip b)]
			with
				_ -> [l]
		else
			[l]
	) lines)

let parse_hxml file =
	let ch = IO.input_channel (try open_in_bin file with _ -> raise Not_found) in
	let data = IO.read_all ch in
	IO.close_in ch;
	parse_hxml_data data

let lookup_classes com spath =
	let rec loop = function
		| [] -> []
		| cp :: l ->
			let cp = (if cp = "" then "./" else cp) in
			let c = normalize_path (get_real_path (Common.unique_full_path cp)) in
			let clen = String.length c in
			if clen < String.length spath && String.sub spath 0 clen = c then begin
				let path = String.sub spath clen (String.length spath - clen) in
				(try
					let path = make_path path in
					(match loop l with
					| [x] when String.length (Ast.s_type_path x) < String.length (Ast.s_type_path path) -> [x]
					| _ -> [path])
				with _ -> loop l)
			end else
				loop l
	in
	loop com.class_path

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
					if com.display = DMNone then raise Not_found;
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
		if Sys.os_type <> "Win32" && Sys.os_type <> "Cygwin" then s else String.concat "\n" (Str.split (Str.regexp "\r\n") s)
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


let default_flush ctx =
	List.iter prerr_endline (List.rev ctx.messages);
	if ctx.has_error && !prompt then begin
		print_endline "Press enter to exit...";
		ignore(read_line());
	end;
	if ctx.has_error then exit 1

let create_context params =
	let ctx = {
		com = Common.create version params;
		flush = (fun()->());
		setup = (fun()->());
		messages = [];
		has_next = false;
		has_error = false;
	} in
	ctx.flush <- (fun() -> default_flush ctx);
	ctx

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
			(try Unix.chdir dir with _ -> raise (Arg.Bad "Invalid directory"));
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

and wait_loop boot_com host port =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.setsockopt sock Unix.SO_REUSEADDR true with _ -> ());
	(try Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't wait on " ^ host ^ ":" ^ string_of_int port));
	Unix.listen sock 10;
	Sys.catch_break false;
	let verbose = boot_com.verbose in
	let has_parse_error = ref false in
	if verbose then print_endline ("Waiting on " ^ host ^ ":" ^ string_of_int port);
	let bufsize = 1024 in
	let tmp = String.create bufsize in
	let cache = {
		c_haxelib = Hashtbl.create 0;
		c_files = Hashtbl.create 0;
		c_modules = Hashtbl.create 0;
	} in
	global_cache := Some cache;
	Typer.macro_enable_cache := true;
	Typeload.parse_hook := (fun com2 file p ->
		let sign = get_signature com2 in
		let ffile = Common.unique_full_path file in
		let ftime = file_time ffile in
		let fkey = ffile ^ "!" ^ sign in
		try
			let time, data = Hashtbl.find cache.c_files fkey in
			if time <> ftime then raise Not_found;
			data
		with Not_found ->
			has_parse_error := false;
			let data = Typeload.parse_file com2 file p in
			if verbose then print_endline ("Parsed " ^ ffile);
			if not !has_parse_error && ffile <> (!Parser.resume_display).Ast.pfile then Hashtbl.replace cache.c_files fkey (ftime,data);
			data
	);
	let cache_module m =
		Hashtbl.replace cache.c_modules (m.m_path,m.m_extra.m_sign) m;
	in
	let check_module_path com m p =
		if m.m_extra.m_file <> Common.unique_full_path (Typeload.resolve_module_file com m.m_path (ref[]) p) then begin
			if verbose then print_endline ("Module path " ^ s_type_path m.m_path ^ " has been changed");
			raise Not_found;
		end
	in
	let compilation_step = ref 0 in
	let compilation_mark = ref 0 in
	let mark_loop = ref 0 in
	Typeload.type_module_hook := (fun (ctx:Typecore.typer) mpath p ->
		let t = Common.timer "module cache check" in
		let com2 = ctx.Typecore.com in
		let sign = get_signature com2 in
		let dep = ref None in
		incr mark_loop;
		let mark = !mark_loop in
		let start_mark = !compilation_mark in
		let rec check m =
			if m.m_extra.m_dirty then begin
				dep := Some m;
				false
			end else if m.m_extra.m_mark = mark then
				true
			else try
				if m.m_extra.m_mark <= start_mark then begin
					(match m.m_extra.m_kind with
					| MFake | MSub -> () (* don't get classpath *)
					| MExtern ->
						(* if we have a file then this will override our extern type *)
						let has_file = (try ignore(Typeload.resolve_module_file com2 m.m_path (ref[]) p); true with Not_found -> false) in
						if has_file then begin
							if verbose then print_endline ("A file is masking the library file " ^ s_type_path m.m_path);
							raise Not_found;
						end;
						let rec loop = function
							| [] ->
								if verbose then print_endline ("No library file was found for " ^ s_type_path m.m_path);
								raise Not_found (* no extern registration *)
							| load :: l ->
								match load m.m_path p with
								| None -> loop l
								| Some (file,_) ->
									if Common.unique_full_path file <> m.m_extra.m_file then begin
										if verbose then print_endline ("Library file was changed for " ^ s_type_path m.m_path);
										raise Not_found;
									end
						in
						loop com2.load_extern_type
					| MCode -> check_module_path com2 m p
					| MMacro when ctx.Typecore.in_macro -> check_module_path com2 m p
					| MMacro ->
						let _, mctx = Typer.get_macro_context ctx p in
						check_module_path mctx.Typecore.com m p
					);
					if file_time m.m_extra.m_file <> m.m_extra.m_time then begin
						if verbose then print_endline ("File " ^ m.m_extra.m_file ^ (if m.m_extra.m_time = -1. then " not cached (macro-in-macro)" else " has been modified"));
						if m.m_extra.m_kind = MFake then Hashtbl.remove Typecore.fake_modules m.m_extra.m_file;
						raise Not_found;
					end;
				end;
				m.m_extra.m_mark <- mark;
				PMap.iter (fun _ m2 -> if not (check m2) then begin dep := Some m2; raise Not_found end) m.m_extra.m_deps;
				true
			with Not_found ->
				m.m_extra.m_dirty <- true;
				false
		in
		let rec add_modules m0 m =
			if m.m_extra.m_added < !compilation_step then begin
				(match m0.m_extra.m_kind, m.m_extra.m_kind with
				| MCode, MMacro | MMacro, MCode ->
					(* this was just a dependency to check : do not add to the context *)
					()
				| _ ->
					if verbose then print_endline ("Reusing  cached module " ^ Ast.s_type_path m.m_path);
					m.m_extra.m_added <- !compilation_step;
					List.iter (fun t ->
						match t with
						| TClassDecl c -> c.cl_restore()
						| TEnumDecl e ->
							let rec loop acc = function
								| [] -> ()
								| (Ast.Meta.RealPath,[Ast.EConst (Ast.String path),_],_) :: l ->
									e.e_path <- Ast.parse_path path;
									e.e_meta <- (List.rev acc) @ l;
								| x :: l -> loop (x::acc) l
							in
							loop [] e.e_meta
						| TAbstractDecl a ->
							a.a_meta <- List.filter (fun (m,_,_) -> m <> Ast.Meta.ValueUsed) a.a_meta
						| _ -> ()
					) m.m_types;
					if m.m_extra.m_kind <> MSub then Typeload.add_module ctx m p;
					PMap.iter (Hashtbl.add com2.resources) m.m_extra.m_binded_res;
					PMap.iter (fun _ m2 -> add_modules m0 m2) m.m_extra.m_deps);
					List.iter (Typer.call_init_macro ctx) m.m_extra.m_macro_calls
			end
		in
		try
			let m = Hashtbl.find cache.c_modules (mpath,sign) in
			if not (check m) then begin
				if verbose then print_endline ("Skipping cached module " ^ Ast.s_type_path mpath ^ (match !dep with None -> "" | Some m -> "(" ^ Ast.s_type_path m.m_path ^ ")"));
				raise Not_found;
			end;
			add_modules m m;
			t();
			Some m
		with Not_found ->
			t();
			None
	);
	let run_count = ref 0 in
	while true do
		let sin, _ = Unix.accept sock in
		let t0 = get_time() in
		Unix.set_nonblock sin;
		if verbose then print_endline "Client connected";
		let b = Buffer.create 0 in
		let rec read_loop count =
			let r = try
				Unix.recv sin tmp 0 bufsize []
			with Unix.Unix_error((Unix.EWOULDBLOCK|Unix.EAGAIN),_,_) ->
				0
			in
			if verbose then begin
				if r > 0 then Printf.printf "Reading %d bytes\n" r else print_endline "Waiting for data...";
			end;
			Buffer.add_substring b tmp 0 r;
			if r > 0 && tmp.[r-1] = '\000' then
				Buffer.sub b 0 (Buffer.length b - 1)
			else begin
				if r = 0 then ignore(Unix.select [] [] [] 0.05); (* wait a bit *)
				if count = 100 then
					failwith "Aborting unactive connection"
				else
					read_loop (count + 1);
			end;
		in
		let rec cache_context com =
			if com.display = DMNone then begin
				List.iter cache_module com.modules;
				if verbose then print_endline ("Cached " ^ string_of_int (List.length com.modules) ^ " modules");
			end;
			match com.get_macros() with
			| None -> ()
			| Some com -> cache_context com
		in
		let create params =
			let ctx = create_context params in
			ctx.flush <- (fun() ->
				incr compilation_step;
				compilation_mark := !mark_loop;
				List.iter (fun s -> ssend sin (s ^ "\n"); if verbose then print_endline ("> " ^ s)) (List.rev ctx.messages);
				if ctx.has_error then ssend sin "\x02\n" else cache_context ctx.com;
			);
			ctx.setup <- (fun() ->
				Parser.display_error := (fun e p -> has_parse_error := true; ctx.com.error (Parser.error_msg e) p);
				if ctx.com.display <> DMNone then begin
					let file = (!Parser.resume_display).Ast.pfile in
					let fkey = file ^ "!" ^ get_signature ctx.com in
					(* force parsing again : if the completion point have been changed *)
					Hashtbl.remove cache.c_files fkey;
					(* force module reloading (if cached) *)
					Hashtbl.iter (fun _ m -> if m.m_extra.m_file = file then m.m_extra.m_dirty <- true) cache.c_modules
				end
			);
			ctx.com.print <- (fun str -> ssend sin ("\x01" ^ String.concat "\x01" (ExtString.String.nsplit str "\n") ^ "\n"));
			ctx
		in
		(try
			let data = parse_hxml_data (read_loop 0) in
			Unix.clear_nonblock sin;
			if verbose then print_endline ("Processing Arguments [" ^ String.concat "," data ^ "]");
			(try
				Common.display_default := DMNone;
				Parser.resume_display := Ast.null_pos;
				Typeload.return_partial_type := false;
				measure_times := false;
				close_times();
				stats.s_files_parsed := 0;
				stats.s_classes_built := 0;
				stats.s_methods_typed := 0;
				stats.s_macros_called := 0;
				Hashtbl.clear Common.htimers;
				let _ = Common.timer "other" in
				incr compilation_step;
				compilation_mark := !mark_loop;
				start_time := get_time();
				process_params create data;
				close_times();
				if !measure_times then report_times (fun s -> ssend sin (s ^ "\n"))
			with Completion str ->
				if verbose then print_endline ("Completion Response =\n" ^ str);
				ssend sin str
			);
			if verbose then begin
				print_endline (Printf.sprintf "Stats = %d files, %d classes, %d methods, %d macros" !(stats.s_files_parsed) !(stats.s_classes_built) !(stats.s_methods_typed) !(stats.s_macros_called));
				print_endline (Printf.sprintf "Time spent : %.3fs" (get_time() -. t0));
			end
		with Unix.Unix_error _ ->
			if verbose then print_endline "Connection Aborted"
		| e ->
			let estr = Printexc.to_string e in
			if verbose then print_endline ("Uncaught Error : " ^ estr);
			(try ssend sin estr with _ -> ());
		);
		Unix.close sin;
		(* prevent too much fragmentation by doing some compactions every X run *)
		incr run_count;
		if !run_count mod 10 = 0 then begin
			let t0 = get_time() in
			Gc.compact();
			if verbose then begin
				let stat = Gc.quick_stat() in
				let size = (float_of_int stat.Gc.heap_words) *. 4. in
				print_endline (Printf.sprintf "Compacted memory %.3fs %.1fMB" (get_time() -. t0) (size /. (1024. *. 1024.)));
			end
		end else Gc.minor();
	done

and do_connect host port args =
	let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	(try Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string host,port)) with _ -> failwith ("Couldn't connect on " ^ host ^ ":" ^ string_of_int port));
	let args = ("--cwd " ^ Unix.getcwd()) :: args in
	ssend sock (String.concat "" (List.map (fun a -> a ^ "\n") args) ^ "\000");
	let has_error = ref false in
	let rec print line =
		match (if line = "" then '\x00' else line.[0]) with
		| '\x01' ->
			print_string (String.concat "\n" (List.tl (ExtString.String.nsplit line "\x01")));
			flush stdout
		| '\x02' ->
			has_error := true;
		| _ ->
			prerr_endline line;
	in
	let buf = Buffer.create 0 in
	let process() =
		let lines = ExtString.String.nsplit (Buffer.contents buf) "\n" in
		(* the last line ends with \n *)
		let lines = (match List.rev lines with "" :: l -> List.rev l | _ -> lines) in
		List.iter print lines;
	in
	let tmp = String.create 1024 in
	let rec loop() =
		let b = Unix.recv sock tmp 0 1024 [] in
		Buffer.add_substring buf tmp 0 b;
		if b > 0 then begin
			if String.get tmp (b - 1) = '\n' then begin
				process();
				Buffer.reset buf;
			end;
			loop();
		end
	in
	loop();
	process();
	if !has_error then exit 1

and init ctx =
	let usage = Printf.sprintf
		"Haxe Compiler %s - (C)2005-2015 Haxe Foundation\n Usage : haxe%s -main <class> [-swf|-js|-neko|-php|-cpp|-as3] <output> [options]\n Options :"
		s_version (if Sys.os_type = "Win32" then ".exe" else "")
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
	Common.define_value com Define.HaxeVer (float_repres (float_of_int version /. 1000.));
	Common.define_value com Define.HxcppApiLevel "321";
	Common.raw_define com "haxe3";
	Common.define_value com Define.Dce "std";
	com.warning <- (fun msg p -> message ctx ("Warning : " ^ msg) p);
	com.error <- error ctx;
	if !global_cache <> None then com.run_command <- run_command ctx;
	Parser.display_error := (fun e p -> com.error (Parser.error_msg e) p);
	Parser.use_doc := !Common.display_default <> DMNone || (!global_cache <> None);
	(try
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
		com.class_path <- "" :: List.map normalize_path (loop parts)
	with
		Not_found ->
			if Sys.os_type = "Unix" then
				com.class_path <- ["/usr/lib/haxe/std/";"/usr/share/haxe/std/";"/usr/local/lib/haxe/std/";"/usr/lib/haxe/extraLibs/";"/usr/local/lib/haxe/extraLibs/";""]
			else
				let base_path = normalize_path (get_real_path (try executable_path() with _ -> "./")) in
				com.class_path <- [base_path ^ "std/";base_path ^ "extraLibs/";""]);
	com.std_path <- List.filter (fun p -> ExtString.String.ends_with p "std/" || ExtString.String.ends_with p "std\\") com.class_path;
	let set_platform pf file =
		if com.platform <> Cross then failwith "Multiple targets";
		Common.init_platform com pf;
		com.file <- file;
		if (pf = Flash) && file_extension file = "swc" then Common.define com Define.Swc;
	in
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
			com.class_path <- normalize_path path :: com.class_path
		),"<path> : add a directory to find source files");
		("-js",Arg.String (set_platform Js),"<file> : compile code to JavaScript file");
		("-swf",Arg.String (set_platform Flash),"<file> : compile code to Flash SWF file");
		("-as3",Arg.String (fun dir ->
			set_platform Flash dir;
			Common.define com Define.As3;
			Common.define com Define.NoInline;
		),"<directory> : generate AS3 code into target directory");
		("-neko",Arg.String (set_platform Neko),"<file> : compile code to Neko Binary");
		("-php",Arg.String (fun dir ->
			classes := (["php"],"Boot") :: !classes;
			set_platform Php dir;
		),"<directory> : generate PHP code into target directory");
		("-cpp",Arg.String (fun dir ->
			set_platform Cpp dir;
		),"<directory> : generate C++ code into target directory");
		("-cs",Arg.String (fun dir ->
			cp_libs := "hxcs" :: !cp_libs;
			set_platform Cs dir;
		),"<directory> : generate C# code into target directory");
		("-java",Arg.String (fun dir ->
			cp_libs := "hxjava" :: !cp_libs;
			set_platform Java dir;
		),"<directory> : generate Java code into target directory");
		("-python",Arg.String (fun dir ->
			set_platform Python dir;
		),"<file> : generate Python code as target file");
		("-xml",Arg.String (fun file ->
			Parser.use_doc := true;
			xml_out := Some file
		),"<file> : generate XML types description");
		("-main",Arg.String (fun cl ->
			if com.main_class <> None then raise (Arg.Bad "Multiple -main");
			let cpath = make_path cl in
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
			set_platform Neko neko_file;
			if com.main_class = None then begin
				let cpath = make_path file in
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
				complete_fields com (Hashtbl.fold (fun k _ acc -> (k,"",None,"") :: acc) Lexer.keywords [])
			| "memory" ->
				did_something := true;
				(try display_memory ctx with e -> prerr_endline (Printexc.get_backtrace ()));
			| _ ->
				let file, pos = try ExtString.String.split file_pos "@" with _ -> failwith ("Invalid format : " ^ file_pos) in
				let file = unquote file in
				let pos, smode = try ExtString.String.split pos "@" with _ -> pos,"" in
				let activate_special_display_mode () =
					Common.define com Define.NoCOpt;
					Parser.use_parser_resume := false
				in
				let mode = match smode with
					| "position" ->
						activate_special_display_mode();
						DMPosition
					| "usage" ->
						activate_special_display_mode();
						DMUsage
					| "type" ->
						activate_special_display_mode();
						DMType
					| "toplevel" ->
						activate_special_display_mode();
						DMToplevel
					| "" ->
						Parser.use_parser_resume := true;
						DMDefault
					| _ ->
						let smode,arg = try ExtString.String.split smode "@" with _ -> pos,"" in
						match smode with
							| "resolve" ->
								activate_special_display_mode();
								DMResolve arg
							| _ ->
								Parser.use_parser_resume := true;
								DMDefault
				in
				let pos = try int_of_string pos with _ -> failwith ("Invalid format : "  ^ pos) in
				com.display <- mode;
				Common.display_default := mode;
				Common.define_value com Define.Display (if smode <> "" then smode else "1");
				Parser.use_doc := true;
				Parser.resume_display := {
					Ast.pfile = Common.unique_full_path file;
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
			set_platform Neko "";
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
			let host, port = (try ExtString.String.split hp ":" with _ -> "127.0.0.1", hp) in
			wait_loop com host (try int_of_string port with _ -> raise (Arg.Bad "Invalid port"))
		),"<[host:]port> : wait on the given port for commands to run)");
		("--connect",Arg.String (fun _ ->
			assert false
		),"<[host:]port> : connect on the given port and run commands there)");
		("--cwd", Arg.String (fun dir ->
			assert false
		),"<dir> : set current working directory");
		("-version",Arg.Unit (fun() ->
			message ctx s_version Ast.null_pos;
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
			let m = ref 0 in
			let rec loop i =
				let d = Obj.magic i in
				if d <> Meta.Last then begin
					let t, (doc,flags) = MetaInfo.to_string d in
					if not (List.mem MetaInfo.Internal flags) then begin
						let params = ref [] and used = ref [] and pfs = ref [] in
						List.iter (function
							| MetaInfo.HasParam s -> params := s :: !params
							| MetaInfo.Platform f -> pfs := f :: !pfs
							| MetaInfo.Platforms fl -> pfs := fl @ !pfs
							| MetaInfo.UsedOn u -> used := u :: !used
							| MetaInfo.UsedOnEither ul -> used := ul @ !used
							| MetaInfo.Internal -> assert false
						) flags;
						let params = (match List.rev !params with
							| [] -> ""
							| l -> "(" ^ String.concat "," l ^ ")"
						) in
						let pfs = (match List.rev !pfs with
							| [] -> ""
							| [p] -> " (" ^ platform_name p ^ " only)"
							| pl -> " (for " ^ String.concat "," (List.map platform_name pl) ^ ")"
						) in
						let str = "@" ^ t in
						if String.length str > !m then m := String.length str;
						(str,params ^ doc ^ pfs) :: loop (i + 1)
					end else
						loop (i + 1)
				end else
					[]
			in
			let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
			let all = List.map (fun (n,doc) -> Printf.sprintf " %-*s: %s" !m n (limit_string doc (!m + 3))) all in
			List.iter (fun msg -> ctx.com.print (msg ^ "\n")) all;
			did_something := true
		),": print help for all compiler metadatas");
	] in
	let args_callback cl = classes := make_path cl :: !classes in
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
				let msg = Typecore.string_error_raise s sl (Printf.sprintf "Invalid command: %s" s) in
				raise (Arg.Bad msg)
			with Not_found ->
				raise exc);
		arg_delays := []
	in
	process_ref := process;
	process ctx.com.args;
	process_libs();
	if com.display <> DMNone then begin
		com.warning <- message ctx;
		com.error <- error ctx;
		com.main_class <- None;
		let real = get_real_path (!Parser.resume_display).Ast.pfile in
		classes := lookup_classes com real;
		if !classes = [] then begin
			if not (Sys.file_exists real) then failwith "Display file does not exist";
			(match List.rev (ExtString.String.nsplit real path_sep) with
			| file :: _ when file.[0] >= 'a' && file.[1] <= 'z' -> failwith ("Display file '" ^ file ^ "' should not start with a lowercase letter")
			| _ -> ());
			failwith "Display file was not found in class path";
		end;
		Common.log com ("Display file : " ^ real);
		Common.log com ("Classes found : ["  ^ (String.concat "," (List.map Ast.s_type_path !classes)) ^ "]");
	end;
	let add_std dir =
		com.class_path <- List.filter (fun s -> not (List.mem s com.std_path)) com.class_path @ List.map (fun p -> p ^ dir ^ "/_std/") com.std_path @ com.std_path
	in
	let ext = (match com.platform with
		| Cross ->
			(* no platform selected *)
			set_platform Cross "";
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
			add_std "js";
			"js"
		| Php ->
			add_std "php";
			"php"
		| Cpp ->
			add_std "cpp";
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
	) in
	(* if we are at the last compilation step, allow all packages accesses - in case of macros or opening another project file *)
	begin match com.display with
		| DMNone | DMToplevel ->
			()
		| _ ->
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
		begin match com.display with
			| DMNone | DMUsage | DMPosition | DMType | DMResolve _ ->
				()
			| _ ->
				if ctx.has_next then raise Abort;
				failwith "No completion point was found";
		end;
		let t = Common.timer "filters" in
		let main, types, modules = Typer.generate tctx in
		com.main <- main;
		com.types <- types;
		com.modules <- modules;
		Filters.run com tctx main;
		if ctx.has_error then raise Abort;
		(* check file extension. In case of wrong commandline, we don't want
			to accidentaly delete a source file. *)
		if not !no_output && file_extension com.file = ext then delete_file com.file;
		(match !xml_out with
		| None -> ()
		| Some "hx" ->
			Genxml.generate_hx com
		| Some file ->
			Common.log com ("Generating xml : " ^ file);
			Common.mkdir_from_path file;
			Genxml.generate com file);
		if com.platform = Flash || com.platform = Cpp then List.iter (Codegen.fix_overrides com) com.types;
		if Common.defined com Define.Dump then Codegen.dump_types com;
		if Common.defined com Define.DumpDependencies then Codegen.dump_dependencies com;
		t();
		if not !no_output then begin match com.platform with
			| Neko when !interp -> ()
			| Cpp when Common.defined com Define.Cppia -> ()
			| Cpp | Cs | Java | Php -> Common.mkdir_from_path (com.file ^ "/.")
			| _ -> Common.mkdir_from_path com.file
		end;
		if not !no_output then begin
			if !interp then begin
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
					Genswf.generate !swf_header,"swf"
				| Neko ->
					Genneko.generate,"neko"
				| Js ->
					Genjs.generate,"js"
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
				| Cross ->
					assert false
				in
				Common.log com ("Generating " ^ name ^ ": " ^ com.file);
				let t = Common.timer ("generate " ^ name) in
				generate com;
				t()
			end
		end
	end;
	Sys.catch_break false;
	List.iter (fun f -> f()) (List.rev com.final_filters);
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
	| Codegen.Generic_Exception(m,p) ->
		error ctx m p
	| Arg.Bad msg ->
		error ctx ("Error: " ^ msg) Ast.null_pos
	| Failure msg when not (is_debug_run()) ->
		error ctx ("Error: " ^ msg) Ast.null_pos
	| Arg.Help msg ->
		message ctx msg Ast.null_pos
	| Typer.DisplayFields fields ->
		let ctx = print_context() in
		let fields = List.map (fun (name,t,kind,doc) -> name, s_type ctx t, kind, (match doc with None -> "" | Some d -> d)) fields in
		let fields = if !measure_times then begin
			close_times();
			let tot = ref 0. in
			Hashtbl.iter (fun _ t -> tot := !tot +. t.total) Common.htimers;
			let fields = ("@TOTAL", Printf.sprintf "%.3fs" (get_time() -. !start_time), None, "") :: fields in
			if !tot > 0. then
				Hashtbl.fold (fun _ t acc ->
					("@TIME " ^ t.name, Printf.sprintf "%.3fs (%.0f%%)" t.total (t.total *. 100. /. !tot), None, "") :: acc
				) Common.htimers fields
			else fields
		end else
			fields
		in
		complete_fields com fields
	| Typecore.DisplayTypes tl ->
		let ctx = print_context() in
		let b = Buffer.create 0 in
		List.iter (fun t ->
			Buffer.add_string b "<type>\n";
			Buffer.add_string b (htmlescape (s_type ctx t));
			Buffer.add_string b "\n</type>\n";
		) tl;
		raise (Completion (Buffer.contents b))
	| Typecore.DisplayPosition pl ->
		let b = Buffer.create 0 in
		let error_printer file line = sprintf "%s:%d:" (Common.unique_full_path file) line in
		Buffer.add_string b "<list>\n";
		List.iter (fun p ->
			let epos = Lexer.get_error_pos error_printer p in
			Buffer.add_string b "<pos>";
			Buffer.add_string b epos;
			Buffer.add_string b "</pos>\n";
		) pl;
		Buffer.add_string b "</list>";
		raise (Completion (Buffer.contents b))
	| Typer.DisplayToplevel il ->
		let b = Buffer.create 0 in
		Buffer.add_string b "<il>\n";
		let ctx = print_context() in
		let s_type t = htmlescape (s_type ctx t) in
		List.iter (fun id -> match id with
			| Typer.ITLocal v -> Buffer.add_string b (Printf.sprintf "<i k=\"local\" t=\"%s\">%s</i>\n" (s_type v.v_type) v.v_name);
			| Typer.ITMember(c,cf) -> Buffer.add_string b (Printf.sprintf "<i k=\"member\" t=\"%s\">%s</i>\n" (s_type cf.cf_type) cf.cf_name);
			| Typer.ITStatic(c,cf) -> Buffer.add_string b (Printf.sprintf "<i k=\"static\" t=\"%s\">%s</i>\n" (s_type cf.cf_type) cf.cf_name);
			| Typer.ITEnum(en,ef) -> Buffer.add_string b (Printf.sprintf "<i k=\"enum\" t=\"%s\">%s</i>\n" (s_type ef.ef_type) ef.ef_name);
			| Typer.ITGlobal(mt,s,t) -> Buffer.add_string b (Printf.sprintf "<i k=\"global\" p=\"%s\" t=\"%s\">%s</i>\n" (s_type_path (t_infos mt).mt_path) (s_type t) s);
			| Typer.ITType(mt) -> Buffer.add_string b (Printf.sprintf "<i k=\"type\" p=\"%s\">%s</i>\n" (s_type_path (t_infos mt).mt_path) (snd (t_infos mt).mt_path));
			| Typer.ITPackage s -> Buffer.add_string b (Printf.sprintf "<i k=\"package\">%s</i>\n" s)
		) il;
		Buffer.add_string b "</il>";
		raise (Completion (Buffer.contents b))
	| Parser.TypePath (p,c,is_import) ->
		(match c with
		| None ->
			let packs, classes = read_type_path com p in
			if packs = [] && classes = [] then
				error ctx ("No classes found in " ^ String.concat "." p) Ast.null_pos
			else
				complete_fields com (
					let convert k f = (f,"",Some k,"") in
					(List.map (convert Typer.FKPackage) packs) @ (List.map (convert Typer.FKType) classes)
				)
		| Some (c,cur_package) ->
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
				let types = if c <> s_module then [] else List.map (fun t -> snd (t_path t),"",Some Typer.FKType,"") public_types in
				let ctx = print_context() in
				let make_field_doc cf =
					cf.cf_name,
					s_type ctx cf.cf_type,
					Some (match cf.cf_kind with Method _ -> Typer.FKMethod | Var _ -> Typer.FKVar),
					(match cf.cf_doc with Some s -> s | None -> "")
				in
				let types = match !statics with
					| None -> types
					| Some cfl -> types @ (List.map make_field_doc (List.filter (fun cf -> cf.cf_public) cfl))
				in
				complete_fields com types
			with Completion c ->
				raise (Completion c)
			| _ ->
				error ctx ("Could not load module " ^ (Ast.s_type_path (p,c))) Ast.null_pos)
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
);
other();
if !measure_times then report_times prerr_endline
