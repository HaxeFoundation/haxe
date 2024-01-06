open Globals
open Ast
open Type

(** retrieve string from @:native metadata or raise Not_found *)
let get_native_name meta =
	let rec get_native meta = match meta with
		| [] -> raise Not_found
		| (Meta.Native,[v],p as meta) :: _ ->
			meta
		| _ :: meta ->
			get_native meta
	in
	let (_,e,mp) = get_native meta in
	match e with
	| [Ast.EConst (Ast.String(name,_)),p] ->
		name,p
	| [] ->
		raise Not_found
	| _ ->
		Error.raise_typing_error "String expected" mp

(* Rewrites class or enum paths if @:native metadata is set *)
let apply_native_paths t =
	let get_real_name meta name =
		let name',p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (name,SDoubleQuotes)), p], p), name'
	in
	let get_real_path meta path =
		let name,p = get_native_name meta in
		(Meta.RealPath,[Ast.EConst (Ast.String (s_type_path path,SDoubleQuotes)), p], p), parse_path name
	in
	try
		(match t with
		| TClassDecl c ->
			let did_change = ref false in
			let field cf = try
				let meta,name = get_real_name cf.cf_meta cf.cf_name in
				cf.cf_name <- name;
				cf.cf_meta <- meta :: cf.cf_meta;
				List.iter (fun cf -> cf.cf_name <- name) cf.cf_overloads;
				did_change := true
			with Not_found ->
				()
			in
			let fields cfs old_map =
				did_change := false;
				List.iter field cfs;
				if !did_change then
					List.fold_left (fun map f -> PMap.add f.cf_name f map) PMap.empty cfs
				else
					old_map
			in
			c.cl_fields <- fields c.cl_ordered_fields c.cl_fields;
			c.cl_statics <- fields c.cl_ordered_statics c.cl_statics;
			let meta,path = get_real_path c.cl_meta c.cl_path in
			c.cl_meta <- meta :: c.cl_meta;
			c.cl_path <- path;
		| TEnumDecl e ->
			let did_change = ref false in
			let field _ ef = try
				let meta,name = get_real_name ef.ef_meta ef.ef_name in
				ef.ef_name <- name;
				ef.ef_meta <- meta :: ef.ef_meta;
				did_change := true;
			with Not_found ->
				()
			in
			PMap.iter field e.e_constrs;
			if !did_change then begin
				let names = ref [] in
				e.e_constrs <- PMap.fold
					(fun ef map ->
						names := ef.ef_name :: !names;
						PMap.add ef.ef_name ef map
					)
					e.e_constrs PMap.empty;
				e.e_names <- !names;
			end;
			let meta,path = get_real_path e.e_meta e.e_path in
			e.e_meta <- meta :: e.e_meta;
			e.e_path <- path;
		| _ ->
			())
	with Not_found ->
		()
