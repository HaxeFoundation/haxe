open Globals
include DefineList

type define = {
	mutable values : (string,string) PMap.t;
	mutable defines_signature : string option;
}

type user_define = {
	doc : string;
	flags : define_parameter list;
	source : string option;
}

let register_user_define user_defines s data =
	Hashtbl.replace user_defines s data

type define_origin =
	| Compiler
	| UserDefined of string option

type define_infos = {
	d_doc : string;
	d_params : string list;
	d_platforms : Globals.platform list;
	d_origin : define_origin;
	d_links : string list;
	d_deprecated : string option;
}

let infos ?user_defines d =
	let extract_infos (t, (doc, flags), origin) =
		let params = ref [] and pfs = ref [] and links = ref [] and deprecated = ref None in
		List.iter (function
			| HasParam s -> params := s :: !params
			| Platforms fl -> pfs := fl @ !pfs
			| Link url -> links := url :: !links
			| Deprecated s -> deprecated := Some s
		) flags;
		(t, {
			d_doc = doc;
			d_params = !params;
			d_platforms = !pfs;
			d_origin = origin;
			d_links = !links;
			d_deprecated = !deprecated;
		})
	in

	extract_infos (match (user_defines,d) with
	| (Some(user_defines), Custom(s)) when (Hashtbl.mem user_defines s) ->
		let infos = Hashtbl.find user_defines s in
		(s, (infos.doc, infos.flags), (UserDefined infos.source))
	| (_, Custom(s)) ->
		(s, ("", []), Compiler)
	| _ ->
		let def,infos = DefineList.infos d in
		(def, infos, Compiler))

let get_define_key d =
	match (infos d) with (s,_) -> s

let get_documentation user_defines d =
	let t, data = infos ~user_defines:user_defines d in
	let params = (match List.rev data.d_params with
		| [] -> ""
		| l -> "<" ^ String.concat ">, <" l ^ "> "
	) in
	let origin = match data.d_origin with
		| UserDefined Some s -> " (from " ^ s ^ ")"
		| Compiler | UserDefined None -> ""
	in
	let pfs = platform_list_help (List.rev data.d_platforms) in
	(String.concat "-" (ExtString.String.nsplit t "_")), params ^ data.d_doc ^ pfs ^ origin

let get_documentation_list user_defines =
	let m = ref 0 in
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then begin
			let (str,desc) = get_documentation user_defines d in
			if String.length str > !m then m := String.length str;
			(str,desc) :: loop (i + 1)
		end else
			[]
	in
	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
	all,!m

let get_user_documentation_list user_defines =
	let m = ref 0 in
	let user_defines_list = (Hashtbl.fold (fun d _ acc ->
		let (str,desc) = get_documentation user_defines (Custom d) in
		if String.length str > !m then m := String.length str;
		(str,desc) :: acc
	) user_defines []) in

	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) user_defines_list in
	all,!m

let get_define_list user_defines =
	let rec loop i acc =
		let d = Obj.magic i in
		if d <> Last then (infos ~user_defines d) :: loop (i + 1) acc
		else acc
	in

	let all = loop 0 (Hashtbl.fold (fun str _ acc -> (infos ~user_defines (Custom str)) :: acc) user_defines []) in
	List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) all

let raw_defined ctx k =
	PMap.mem k ctx.values

let defined ctx k =
	raw_defined ctx (get_define_key k)

let raw_defined_value ctx k =
	PMap.find k ctx.values

let defined_value ctx v =
	raw_defined_value ctx (get_define_key v)

let defined_value_safe ?default ctx v =
	try defined_value ctx v
	with Not_found -> match default with Some s -> s | None -> ""

let raw_define_value ctx k v =
	ctx.values <- PMap.add k v ctx.values;
	ctx.defines_signature <- None

let define_value ctx k v =
	raw_define_value ctx (get_define_key k) v

let raw_define ctx k =
	raw_define_value ctx k "1"

let raw_undefine ctx k =
	ctx.values <- PMap.remove k ctx.values;
	ctx.defines_signature <- None

let define ctx k =
	raw_define_value ctx (get_define_key k) "1"

let get_signature def =
	match def.defines_signature with
	| Some s -> s
	| None ->
		let defines = PMap.foldi (fun k v acc ->
			(* don't make much difference between these special compilation flags *)
			match String.concat "_" (ExtString.String.nsplit k "-") with
			(* If we add something here that might be used in conditional compilation it should be added to
			   Parser.parse_macro_ident as well (issue #5682).
			   Note that we should removed flags like use_rtti_doc here.
			*)
			| "display" | "use_rtti_doc" | "macro_times" | "display_details" | "no_copt" | "display_stdin" | "hxb.stats"
			| "message.reporting" | "message.log_file" | "message.log_format" | "message.no_color"
			| "dump" | "dump_dependencies" | "dump_ignore_var_ids" -> acc
			| _ -> (k ^ "=" ^ v) :: acc
		) def.values [] in
		let str = String.concat "@" (List.sort compare defines) in
		let s = Digest.string str in
		def.defines_signature <- Some s;
		s

let deprecation_lut =
	let h = Hashtbl.create 0 in
	List.iter (fun (name,reason) ->
		Hashtbl.add h name reason
	) deprecated_defines;
	h
