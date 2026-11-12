type t =
	| User
	| Std
	| Lib of string option
	| Path of string
	| Pack of string list

exception Invalid of string
exception Unknown_kind

let expected = "Expected user, std, lib, lib:name, path:path or pack:dot.path"

let is_module_name s = s <> "" && match s.[0] with 'A'..'Z' -> true | _ -> false

let is_ident c = match c with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false

let parse_dot_path v =
	let parts = ExtString.String.nsplit v "." in
	let rec check parts = match parts with
		| [] -> ()
		| part :: parts ->
			if part = "" || not (String.for_all is_ident part) then raise (Invalid ("Invalid dot path: " ^ v));
			if is_module_name part && parts <> [] then raise (Invalid "Sub-types can't be selected, use the module or package path");
			check parts
	in
	check parts;
	parts

let make kind value =
	let required () = match value with
		| Some "" -> raise (Invalid (Printf.sprintf "Expected a value after %s:" kind))
		| Some v -> v
		| None -> raise (Invalid (Printf.sprintf "Expected %s:value" kind))
	in
	match kind,value with
	| "user",None -> User
	| "std",None -> Std
	| "lib",None -> Lib None
	| "lib",Some _ -> Lib (Some (required ()))
	| "path",_ -> Path (required ())
	| "pack",_ -> Pack (parse_dot_path (required ()))
	| _ -> raise Unknown_kind

let parse s = match String.index_opt s ':' with
	| Some i -> make (String.sub s 0 i) (Some (String.sub s (i + 1) (String.length s - i - 1)))
	| None -> make s None

let rec matches_package pack file_pack = match pack,file_pack with
	| [],_ -> true
	| [m],Some [m'] when is_module_name m -> m = m'
	| p :: pack,Some (p' :: file_pack) when p = p' -> matches_package pack (Some file_pack)
	| _ -> false

let matcher (class_paths : ClassPaths.class_paths) file =
	let scope = lazy (class_paths#scope_of file) in
	let comparable = lazy (Path.comparable_path file) in
	let pack = lazy (class_paths#package_of file) in
	fun selector -> match selector,Stdlib.Lazy.force scope with
		| User,Some ClassPath.User -> true
		| Std,Some (ClassPath.Std | ClassPath.StdTarget) -> true
		| Lib None,Some (ClassPath.Lib _) -> true
		| Lib (Some name),Some (ClassPath.Lib (Some name')) -> ClassPath.same_lib_name name name'
		| Path path,_ ->
			let path = Path.comparable_path path in
			let file = Stdlib.Lazy.force comparable in
			file = path || ExtString.String.starts_with file (Path.add_trailing_slash path)
		| Pack pack',_ -> matches_package pack' (Stdlib.Lazy.force pack)
		| _ -> false
