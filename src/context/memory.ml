open Globals
open Common
open Type

type memory_request =
	| MCache
	| MContext of string
	| MModule of string * path

let clear_descendants md =
	List.iter (function
		| TClassDecl c ->
			c.cl_descendants <- []
		| _ ->
			()
	) md.m_types

let update_module_type_deps deps md =
	let deps = ref (Obj.repr md :: deps) in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			deps := Obj.repr c :: !deps;
			c.cl_descendants <- []; (* prevent false positive *)
			List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_statics;
			List.iter (fun f -> deps := Obj.repr f :: !deps) c.cl_ordered_fields;
		| TEnumDecl e ->
			deps := Obj.repr e :: !deps;
			List.iter (fun n -> deps := Obj.repr (PMap.find n e.e_constrs) :: !deps) e.e_names;
		| TTypeDecl t -> deps := Obj.repr t :: !deps;
		| TAbstractDecl a -> deps := Obj.repr a :: !deps;
	) md.m_types;
	!deps

let rec scan_module_deps cs m h =
	if Hashtbl.mem h m.m_id then
		()
	else begin
		Hashtbl.add h m.m_id m;
		PMap.iter (fun _ mdep ->
			let m = (cs#get_context mdep.md_sign)#find_module mdep.md_path in
			scan_module_deps cs m h
		) m.m_extra.m_deps
	end

let module_sign key md =
	if md.m_extra.m_sign = key then "" else "(" ^ (try Digest.to_hex md.m_extra.m_sign with _ -> "???" ^ md.m_extra.m_sign) ^ ")"

let collect_leaks m deps out =
	let leaks = ref [] in
	let leak s =
		leaks := s :: !leaks
	in
	if (Objsize.objsize m deps [Obj.repr Common.memory_marker]).Objsize.reached then leak "common";
	if (Objsize.objsize m deps [Obj.repr Typecore.memory_marker]).Objsize.reached then leak "typecore";
	PMap.iter (fun _ md ->
		if (Objsize.objsize m deps [Obj.repr md]).Objsize.reached then leak (s_type_path md.m_path ^ module_sign m.m_extra.m_sign md);
	) out;
	!leaks

let get_out out =
	Obj.repr Common.memory_marker :: Obj.repr Typecore.memory_marker :: PMap.fold (fun m acc -> Obj.repr m :: acc) out []

let get_module_memory cs all_modules m =
	let mdeps = Hashtbl.create 0 in
	scan_module_deps cs m mdeps;
	let deps = ref [Obj.repr null_module] in
	let out = ref all_modules in
	let deps = Hashtbl.fold (fun _ md deps ->
		out := PMap.remove md.m_id !out;
		if m == md then
			deps
		else
			update_module_type_deps deps md;
	) mdeps !deps in
	clear_descendants m;
	let out = !out in
	let chk = get_out out in
	let inf = Objsize.objsize m deps chk in
	let leaks = if inf.reached then collect_leaks m deps out else [] in
	(Objsize.size_with_headers inf,(inf.reached,deps,out,leaks))

let fmt_size sz =
	if sz < 1024 then
		string_of_int sz ^ " B"
	else if sz < 1024*1024 then
		string_of_int (sz asr 10) ^ " KB"
	else
		Printf.sprintf "%.1f MB" ((float_of_int sz) /. (1024.*.1024.))

let fmt_word f =
	fmt_size (int_of_float f * (Sys.word_size / 8))

let size v =
	fmt_size (mem_size v)
