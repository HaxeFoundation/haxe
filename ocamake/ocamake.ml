(* ************************************************************************ *)
(*                                                                          *)
(* OCAMAKE - OCaml Automatic compilation                                    *)
(*      (c)2002 Nicolas Cannasse                                            *)
(*      (c)2002 Motion-Twin                                                 *)
(*                                                                          *)
(* Last version : http://tech.motion-twin.com                               *)
(*                                                                          *)
(* ************************************************************************ *)
open Unix
open Printf
open Arg

type compile_mode =
	| CM_DEFAULT
	| CM_BYTE
	| CM_OPT

type file_ext =
	| ML | MLI | MLL | MLY
	| CMO | CMX | CMA | CMXA
	| DLL | SO | EXE | LIB
	| CMI | O | OBJ | A

type file = {
	name : string;
	ext : file_ext;
	target : string;
	deps : string list;
}

(* ************************************************************************ *)
(* GLOBALS *)

let verbose = ref false (* print command calls in verbose mode *)
let project_name = ref None (* for VC++ DSP *)
let error_process = ref false (* VC++ error message processing *)
let chars_process = ref false (* replace chars range in errors by file data *)

(* ************************************************************************ *)
(* USEFUL FUNCTIONS *)

let if_some f opt def =
	match opt with
	| None -> def
	| Some v -> f v

let print str = print_endline str; flush Pervasives.stdout

let (???) file =
	failwith ("Don't know what to do with file " ^ file)

let str_suffix = function
	| ML -> "ml" | MLI -> "mli" | MLL -> "mll" | MLY -> "mly" | CMO -> "cmo"
	| CMX -> "cmx" | CMA -> "cma" | CMXA -> "cmxa" | DLL -> "dll" | SO -> "so"
	| EXE -> "exe" | CMI -> "cmi" | O -> "o" | A -> "a" | OBJ -> "obj"
	| LIB -> "lib"

let unescape file =
	let l = String.length file in
	if l >= 2 && file.[0] = '"' && file.[l-1] = '"' then String.sub file 1 (l-2) else file

let extension file =
	let rsplit_char str ch =
		let p = String.rindex str ch in
		let len = String.length str in
		(String.sub str 0 p, String.sub str (p + 1) (len - p - 1))	
	in
	let file = unescape file in
	let s = try snd(rsplit_char file '.') with Not_found -> "" in
	String.uppercase s

let (+!) file suff =
	let base = Filename.chop_extension file in
	base ^ "." ^ str_suffix suff

let filter_all_in func ic =
	let rec treat acc =
	try
		match func (input_line ic) with
		| None -> treat acc
		| Some data -> treat (data :: acc)
	with
		End_of_file -> close_in ic; acc
	in
	List.rev (treat [])

let rec remove_duplicates = function
	| [] -> []
	| item :: q when List.exists ((=) item) q -> remove_duplicates q
	| item :: q -> item :: remove_duplicates q

let file_time fname =
	try (Unix.stat fname).st_mtime with Unix_error _ -> 0.

let flatten = String.concat " "

let escape str =
	try
		ignore(String.index str ' ');
		"\"" ^ str ^ "\"";
	with Not_found -> str

let delete_file file =
	try Sys.remove file with Sys_error _ -> ()

let check_existence (ext,name) =
	match ext with
	| ML | MLI ->
		if not (Sys.file_exists name) then
			failwith ("No such file : "^(escape name))
	| _ -> ()
		(* Others files can be found in Ocaml stdlib or
		   user -I paths *)

exception Found_pos of int

let print_errors output msg =
	let split str sep =
		let find_sub str sub =
			let len = String.length sub in
			try
				for i = 0 to String.length str - len do
					if String.sub str i len = sub then raise (Found_pos i);
				done;
				raise Not_found
			with Found_pos i -> i 
		in
		let p = find_sub str sep in
		let len = String.length sep in
		let slen = String.length str in
		(String.sub str 0 p, String.sub str (p + len) (slen - p - len))
	in
	let process_chars file chars line =
		let cmin, cmax = split chars "-" in
		let cmin, cmax = int_of_string cmin, int_of_string cmax in
		if cmax > cmin then begin
			let f = open_in file in
			for i = 1 to line-1 do ignore(input_line f) done;
			seek_in f ((pos_in f)+cmin);
			let s = String.create (cmax - cmin) in
			ignore(input f s 0 (cmax - cmin));
			prerr_endline (try
					(String.sub s 0 (String.index s '\n'))^"..."
				with
					Not_found -> s);
		end
	in
	let printer =
		(match !error_process , !chars_process with
		| true , _ -> (function line ->
			try
				let data, chars = split line ", characters " in
				let data, lnumber = split data "\", line " in
				let _, file = split data "File \"" in
				prerr_string (file ^ "(" ^ lnumber ^ ") : ");
				let chars, _ = split chars ":" in
				if !chars_process then
					(try process_chars file chars (int_of_string lnumber) with _ -> raise Not_found)
 			with
				Not_found ->
					prerr_endline line)
		| false , true -> (function line ->
			try
				let edata, chars = split line ", characters " in
				let data, lnumber = split edata "\", line " in
				let _, file = split data "File \"" in
				let chars, _ = split chars ":" in
				prerr_string (edata^" : ");
				if !chars_process then
					process_chars file chars (int_of_string lnumber);
 			with
				Not_found ->
					prerr_endline line)

		| false , false ->
		      prerr_endline)
	in
	List.iter printer output;
	failwith msg

let exec ?(stdout=false) ?(outfirst=false) cmd errmsg =
	if !verbose then print cmd;
	let pout, pin, perr = open_process_full cmd (Unix.environment()) in
	let read = filter_all_in (fun s -> Some s) in
	let data, edata = 
	(* this is made to prevent the program lock when one
	   buffer is full and the process is waiting for us
	   to read it before exiting... while we're reading
	   the other output buffer ! *)
	(if outfirst then
		let d = read pout in
		let ed = read perr in
		d,ed
	else	
		let ed = read perr in
		let d = read pout in
		d,ed) in
	match close_process_full (pout, pin, perr) with
	| WEXITED 0 -> data,edata
	| WEXITED exitcode -> print_errors (if stdout then edata @ data else edata) errmsg
	| _ -> failwith "Build aborted by signal"

(* ************************************************************************ *)
(* DEPENDENCIES *)

let line_regexp = Str.regexp "^\\([0-9A-Za-z:_\\./\\\\-]+\\.cm[oi]\\):\\( .*\\)$"
let dep_regexp = Str.regexp " \\([0-9A-Za-z:_\\./\\\\-]+\\.cm[oi]\\)"

let build_graph opt paramlist files =
	let srcfiles = List.filter (fun (e,_) ->
		match e with
		| ML | MLI -> true
		| _ -> false) files in
	let get_name (_,f) = escape f in
	let file_names = flatten (List.map get_name srcfiles) in
	let params = flatten paramlist in
	let command = sprintf "ocamldep %s %s" params file_names in	
	let output,_ = exec command "Failed to make dependencies" ~outfirst:true in
	let data = String.concat "\n" output in	
	let data = Str.global_replace (Str.regexp "\\\\\r\n") "" data in (* win *)
	let data = Str.global_replace (Str.regexp "\\\\\n") "" data in (* unix *)		
	let rec get_deps data p =
		try
			let newp = Str.search_forward dep_regexp data p in
			let file = Str.matched_group 1 data in
			if opt && extension file = "CMO" then 
				(file +! CMX)::(get_deps data (newp+1))
			else
				file::(get_deps data (newp+1))
		with
			Not_found -> []
	in
	let rec get_lines p =		
		try
			let newp = Str.search_forward line_regexp data p in	
			let file = Str.matched_group 1 data in			
			let lines = get_deps (Str.matched_group 2 data) 0 in			
			(Filename.basename file,lines)::(get_lines (newp+1))
		with
			Not_found -> []
	in
	let lines = get_lines 0 in
	let init_infos (ext,fname) =
		let deptarget = Filename.basename (match ext with
			| ML ->  fname +! CMO
			| MLI -> fname +! CMI
			| _ -> fname) in
		let target = (match ext with
			| ML -> fname +! (if opt then CMX else CMO)
			| MLI -> fname +! CMI
			| _ -> fname) in
		{
			name = fname;
			ext = ext;
			target = target;
			deps =
				(try
					snd (List.find (fun (n,_) -> n = deptarget) lines)
				with
					Not_found -> []);
		}
	in	
	let deps = List.map init_infos files in
	match !verbose with
	| false -> deps
	| true ->
		let print_dep d =
			let dl = String.concat " " (List.map Filename.basename d.deps) in
			printf "%s: %s\n" (Filename.basename d.target) dl;
		in
		List.iter print_dep deps;
		deps

let rec graph_topological_sort all g priority acc =
	let has_dep where dep =	
		List.exists (fun f -> Filename.basename f.target =
							Filename.basename dep) where
	in
	let modified a b = (file_time a) < (file_time b) in
	let is_free file = not(List.exists (has_dep g) file.deps) in
	let rec has_priority = function
		| [] -> raise Not_found
		| x :: l ->
			try
				List.find (fun f -> x = (Filename.basename f.name)) g
			with
				Not_found -> has_priority l
	in
	let to_build file =
		all || (* rebuild all *)
		List.exists (has_dep acc) file.deps || (* a dep is rebuild *)
		List.exists (modified file.target) file.deps || (* dep modified *)
		(file_time file.target) < (file_time file.name) (* is modified *)
	in
	match g with
	| [] -> acc
	| _ ->
		let free,g = List.partition is_free g in
		match free with 
		| [] ->
			(try
				let free = has_priority priority in
				let g = List.filter ((<>) free) g in
				if to_build free then
					graph_topological_sort all g priority (acc@[free])
				else
					graph_topological_sort all g priority acc;
			with Not_found ->
				List.iter (fun f -> prerr_endline f.name) g;
				failwith "Cycle detected in file dependencies !")
		| _ ->
			let to_build = List.filter to_build free in
			graph_topological_sort all g priority (acc@to_build)

(* ************************************************************************ *)
(* COMPILATION *)

let compile ?(precomp=false) opt paramlist f =
	try
		let command = (match f.ext with
		| ML | MLI ->
			let params = flatten paramlist in
			let compiler = (if opt then "ocamlopt" else "ocamlc") in
			sprintf "%s -c %s %s" compiler params (escape f.name)
		| MLL when precomp -> "ocamllex " ^ (escape f.name)
		| MLY when precomp -> "ocamlyacc " ^ (escape f.name)
		| _ -> raise Exit) in
		print (Filename.basename (unescape f.name));
		let stdout,stderr = exec command "Build failed" in
		try
			print_errors (stderr@stdout) "";
		with
			Failure _ -> ()
	with
		Exit -> ()

let pre_compile all (ext,name) =
	match ext with
	| MLL | MLY ->
		let time = file_time name in
		if time = 0. then failwith ("No such file : "^(escape name));
		if all || (file_time (name +! ML)) < time then
			compile ~precomp:true false [] {
				name = name;
				ext = ext;
				deps = [];
				target = "";
			}
	| _ -> () (* other files type does not need pre-compilation *)

let clean_targets opt acc (ext,name) =	
	match ext with
	| MLY ->
		(name +! ML) :: (name +! MLI) :: acc
	| MLL ->
		(name +! ML) :: acc
	| ML when opt ->
		(name +! (if Sys.os_type = "Win32" then OBJ else O)) :: (name +! CMX) :: (name +! CMI) :: acc
	| ML ->
		(name +! CMO) :: (name +! CMI) :: acc
	| MLI ->
		(name +! CMI) :: acc
	| _ ->
		acc

(*
	In order to link, we need to order the CMO files.
	We currently have a ML/MLI dependency graph (in fact, tree) generated
	by ocamldep.

	To build the CMO list, we are reducing the dep-tree into one graph merging
	corresponding ML & MLI nodes. ML-ML edges are keeped, ML-MLI edges
	become ML-ML edges only if they do not create a cycle in the reduced
	graph.

	Then we sort the graph using topological ordering.
*)
let graph_reduce opt g =
	let ext = (if opt then CMX else CMO) in
	let rec path_exists g a b =
		if a = b then true else
		try
			let f = List.find (fun f -> f.target = a) g in
			List.exists (fun d -> path_exists g d b) f.deps
		with
			Not_found -> false
	in
	let rec deps_reduce f g = function		
		| [] -> []
		| dep::deps ->
			match extension dep with
			| "CMI" when not(path_exists g (dep +! ext) f.target) ->				
				(dep +! ext)::(deps_reduce f g deps)
			| "CMO" | "CMX" ->
				dep::(deps_reduce f g deps)
			| _ -> deps_reduce f g deps
	in
	let rec do_reduce g acc =
		match g with
		| [] -> acc
		| f::g' ->			
			let f = { f with deps = deps_reduce f (g@acc) f.deps } in
			do_reduce g' (f::acc)
	in
	do_reduce g []	

let is_lib f = match f.ext with
	| CMA | CMXA | CMO | CMX | DLL | SO | LIB | A | O | OBJ -> true
	| _ -> false

let link opt paramlist files priority output =
	print "Linking...";
	let sources = List.filter (fun f -> f.ext = ML) files in
	let libs = List.filter is_lib files in
	let sources = graph_topological_sort true (graph_reduce opt sources) priority [] in
	let lparams = flatten (List.map (fun f -> escape f.name) libs) in
	let sparams = flatten (List.map (fun f -> escape f.target) sources) in
	let params = flatten paramlist in
	let cc = (if opt then "ocamlopt" else "ocamlc") in
	let cmd = sprintf "%s %s %s %s -o %s" cc params lparams sparams output in
	ignore(exec ~stdout:true cmd "Linking failed")

(* ************************************************************************ *)
(* FILE PROCESSING *)

let dsp_get_files dsp_file =
	let get_file line =
		if String.length line > 7 && String.sub line 0 7 = "SOURCE=" then
			Some (unescape (String.sub line 7 (String.length line-7)))
		else
			None
	in
	filter_all_in get_file (open_in dsp_file)

let vcproj_get_files vcp_file =
	let get_file line =
		let len = String.length line in
		let p = ref 0 in
		while !p < len && (line.[!p] = ' ' || line.[!p] = '\t') do
			incr p;
		done;
		let line = String.sub line !p (len - !p) in		
		if String.length line > 13 && String.sub line 0 13 = "RelativePath=" then begin
			let str = String.sub line 13 (String.length line - 14) in
			Some (unescape str)
		end else
			None
	in
	filter_all_in get_file (open_in vcp_file)

let rec list_files errors file =
	match extension file with
	| "ML" -> [(ML,file)]
	| "MLI" -> [(MLI,file)]
	| "VCPROJ" ->
		project_name := Some (Filename.basename file);
		error_process := true;
		chars_process := true;
		List.concat (List.map (list_files false) (vcproj_get_files file))
	| "DSP" ->
		project_name := Some (Filename.basename file);
		error_process := true;
		chars_process := true;
		List.concat (List.map (list_files false) (dsp_get_files file))
	| "CMA" -> [(CMA,file)]
	| "CMXA" -> [(CMXA,file)]
	| "CMX" -> [(CMX,file)]	
	| "CMO" -> [(CMO,file)]
	| "DLL" -> [(DLL,file)]
	| "LIB" -> [(LIB,file)]
	| "A" -> [(A,file)]
	| "O" -> [(O,file)]
	| "OBJ" -> [(OBJ,file)]
	| "SO" -> [(SO,file)]
	| "MLY" -> [(MLY,file);(ML,file +! ML);(MLI,file +! MLI)]
	| "MLL" -> [(MLL,file);(ML,file +! ML)]	
	| _ -> if errors then ??? file else []

let rec get_compile_mode cm = function
	| [] -> cm
	| (ext,name)::files ->
		let error() = failwith "Mixed bytecode and native compilation files." in
		match ext with
		| ML | MLI | MLL | MLY | DLL | SO ->
			get_compile_mode cm files
		| CMA | CMO ->
			if cm = CM_OPT then error() else get_compile_mode CM_BYTE files
		| CMXA | CMX | A | O | OBJ | LIB ->
			if cm = CM_BYTE then error() else get_compile_mode CM_OPT files
		| EXE | CMI ->
			assert false

let rec get_output_file islib cm =
	match !project_name,islib,cm with
	| None, _ , _ -> None
	| Some name,false,_ -> Some (name +! EXE)
	| Some name,true,CM_OPT -> Some (name +! CMXA)
	| Some name,true,_ -> Some (name +! CMA)

(* ************************************************************************ *)
(* MAIN *)

;;
try

let usage =
	"OCAMAKE v1.4 - Copyright (C)2002-2005 Nicolas Cannasse"
	^"\r\nLast version : http://tech.motion-twin.com" in
let compile_mode = ref CM_DEFAULT in
let compile_cma = ref false in
let do_clean = ref false in
let gen_make = ref false in
let rebuild_all = ref false in
let output_file = ref None in
let preprocessor = ref None in
let argfiles = ref [] in
let paths = ref [] in
let cflags = ref [] in
let lflags = ref [] in
let remf = ref [] in
let priority = ref [] in
let arg_spec = [
  ("-all", Unit (fun () -> rebuild_all := true), ": rebuild all files");
  ("-o", String (fun f -> output_file := Some f), "<file> : set output");
  ("-a", Unit (fun () -> compile_cma := true), ": build a library");
  ("-opt", Unit (fun () -> compile_mode := CM_OPT), ": native compilation");
  ("-clean", Unit (fun () -> do_clean := true), ": delete intermediate files");
  ("-I", String (fun p -> paths := p::!paths), "<path> : additional path");
  ("-v", Unit (fun () -> verbose := true), ": turn on verbose mode");
  ("-n", String (fun f -> remf := f::!remf),"<file>: don't compile this file");
  ("-mak", Unit (fun () -> gen_make := true), ": generate Makefile");
  ("-lp", String (fun f -> lflags := f::!lflags), "<p> : linker parameter");
  ("-cp", String (fun f -> cflags := f::!cflags), "<p> : compiler parameter");
  ("-pp", String (fun c -> preprocessor := Some c), "<cmd> : preprocessor");
  ("-epp", Unit (fun() -> error_process := true), ": use MSVC error messages format");
  ("-cpp", Unit (fun() -> chars_process := true), ": convert characters range in errors to file expression");
  ("-g", Unit (fun () -> lflags := "-g"::!lflags; cflags := "-g"::!cflags), ": compile/link in debug mode");
  ("-P", String (fun f -> priority := f::!priority), ": give linking priority to a file when linking ordering failed");
] in
Arg.parse arg_spec (fun arg -> argfiles := arg :: !argfiles) usage;
let files = List.concat (List.map (list_files true) (List.rev !argfiles)) in
let files = List.filter (fun (_,f) ->
	let name = Filename.basename f in
	not(List.exists (fun f -> Filename.basename f = name) !remf)) files in
let compile_mode = get_compile_mode !compile_mode files in
let output_file , compile_mode = (match !output_file with
	| None -> get_output_file !compile_cma compile_mode , compile_mode
	| Some file ->
		match extension file , compile_mode with
		| "CMA" , CM_OPT
		| "CMXA", CM_BYTE -> failwith "Mixed bytecode and native compilation files."
		| "CMA" , _ ->
			compile_cma := true;
			Some file , CM_BYTE
		| "CMXA" , _ ->
			compile_cma := true;
			Some file , CM_OPT
		| _ , _ ->
			Some file , compile_mode)
in
let opt = (compile_mode = CM_OPT) in
if !compile_cma then lflags := "-a"::!lflags;
match files with
  | [] -> Arg.usage arg_spec usage
  | _ ->
	let files = remove_duplicates files in
	let get_path (_,f) = "-I " ^ escape (Filename.dirname f) in
	let paths = List.map (fun p -> "-I " ^ (escape p)) !paths in
	let paths = remove_duplicates (paths@(List.map get_path files)) in
	let p4param = if_some (fun cmd -> "-pp " ^ (escape cmd)) !preprocessor "" in
	match !do_clean,!gen_make with
	| true,true ->
		failwith "Cannot have -mak & -clean at the same time"
	| false,false ->
		if_some delete_file output_file ();
		List.iter (pre_compile !rebuild_all) files;
		List.iter check_existence files;
		let g = build_graph opt (p4param::paths) files in
		let files = graph_topological_sort !rebuild_all g [] [] in
		List.iter (compile opt (!cflags @ p4param::paths)) files;
		if_some (link opt (!lflags @ paths) g (List.rev !priority)) output_file ();
		print "Done";
	| true,false ->
		print "Cleaning...";
		if_some delete_file output_file ();
		let to_clean = List.fold_left (clean_targets opt) [] files in
		List.iter delete_file to_clean;
		if opt && !compile_cma then
			if_some (fun f -> delete_file (f +! (if Sys.os_type = "Win32" then LIB else A))) output_file ();
	| false,true ->
		List.iter (pre_compile !rebuild_all) files;
		let g = build_graph opt (p4param::paths) files in
		let out = open_out "Makefile" in
		let fprint s = output_string out (s^"\n") in
		let genmak f =
			let ext = if opt then CMX else CMO in
			match f.ext with
			| MLL ->
				fprint ((f.name +! ext)^": "^(f.name +! ML)^"\n")
			| MLY ->
				fprint ((f.name +! ext)^": "^(f.name +! ML)^"\n");
				fprint ((f.name +! CMI)^": "^(f.name +! ML)^" "^(f.name +! MLI)^"\n")
			| _ when f.deps <> [] ->
				fprint (f.target^": "^(flatten f.deps)^"\n")
			| _ ->
				()
		in
		let compiles = graph_topological_sort true g [] [] in
		let libs = List.filter is_lib compiles in
		let cmos = List.filter (fun f -> f.ext = ML) compiles in
		fprint "# Makefile generated by OCamake ";
		fprint "# http://tech.motion-twin.com";
		fprint ".SUFFIXES : .ml .mli .cmo .cmi .cmx .mll .mly";
		fprint "";
		fprint ("CFLAGS="^(flatten (!cflags @ p4param::paths)));
		fprint ("LIBS="^(flatten (List.map (fun f -> f.name) libs)));
		let targets = flatten (List.map (fun f -> f.target) cmos) in
		(match output_file with
		| None ->
			fprint "";
			fprint ("all: "^targets^"\n");
		| Some out ->
			fprint ("LFLAGS= -o "^out^" "^(flatten (!lflags @ paths)));
			fprint "";
			fprint ("all: "^out^"\n");
			fprint (out^": "^targets);
			(* I need to reuse the list of targets since $^ is for Make and $** for NMake *)
			fprint ("\t"^(if opt then "ocamlopt" else "ocamlc")^" $(LFLAGS) $(LIBS) "^targets^"\n"));
		List.iter genmak g;
		fprint "";
		fprint "clean:";
		let cleanfiles = flatten (List.fold_left (clean_targets opt) [] files) in
		if_some (fun o ->
				fprint ("\trm -f "^o);
				if opt && !compile_cma then fprint ("\trm -f "^(o +! LIB)^" "^(o +! A));
			) output_file ();
		fprint ("\trm -f "^cleanfiles);
		fprint "";
		fprint "wclean:";
		if_some (fun o ->
				fprint ("\t-@del "^o^" 2>NUL");
				if opt && !compile_cma then fprint ("\t-@del "^(o +! LIB)^" "^(o +! A)^" 2>NUL");
		) output_file ();
		fprint ("\t-@del "^cleanfiles^" 2>NUL");
		fprint "";
		fprint "# SUFFIXES";
		fprint ".ml.cmo:\n\tocamlc $(CFLAGS) -c $<\n";
		fprint ".ml.cmx:\n\tocamlopt $(CFLAGS) -c $<\n";
		fprint ".mli.cmi:\n\tocamlc $(CFLAGS) $<\n";
		fprint ".mll.ml:\n\tocamllex $<\n";
		fprint ".mly.ml:\n\tocamlyacc $<\n";
		close_out out
with
	Failure msg ->
		Pervasives.flush Pervasives.stdout;
		prerr_endline msg;
		Pervasives.flush Pervasives.stderr;
		exit 1;

(* ************************************************************************ *)
