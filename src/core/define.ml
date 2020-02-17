open Globals
include DefineList

type define = {
	mutable values : (string,string) PMap.t;
	mutable defines_signature : string option;
}

let get_documentation_list() =
	let m = ref 0 in
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then begin
			let t, (doc,flags) = infos d in
			let params = ref [] and pfs = ref [] in
			List.iter (function
				| HasParam s -> params := s :: !params
				| Platforms fl -> pfs := fl @ !pfs
				| Link _ -> ()
			) flags;
			let params = (match List.rev !params with
				| [] -> ""
				| l -> "<" ^ String.concat ">, <" l ^ "> "
			) in
			let pfs = platform_list_help (List.rev !pfs) in
			if String.length t > !m then m := String.length t;
			((String.concat "-" (ExtString.String.nsplit t "_")),params ^ doc ^ pfs) :: (loop (i + 1))
		end else
			[]
	in
	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
	all,!m

let raw_defined ctx v =
	PMap.mem v ctx.values

let defined ctx v =
	raw_defined ctx (fst (infos v))

let raw_defined_value ctx k =
	PMap.find k ctx.values

let defined_value ctx v =
	raw_defined_value ctx (fst (infos v))

let defined_value_safe ?default ctx v =
	try defined_value ctx v
	with Not_found -> match default with Some s -> s | None -> ""

let raw_define_value ctx k v =
	ctx.values <- PMap.add k v ctx.values;
	let k = String.concat "_" (ExtString.String.nsplit k "-") in
	ctx.values <- PMap.add k v ctx.values;
	ctx.defines_signature <- None

let raw_define ctx v =
	let k,v = try ExtString.String.split v "=" with _ -> v,"1" in
	raw_define_value ctx k v

let define_value ctx k v =
	raw_define ctx (fst (infos k) ^ "=" ^ v)

let define ctx v =
	raw_define ctx (fst (infos v))

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
			| "display" | "use_rtti_doc" | "macro_times" | "display_details" | "no_copt" | "display_stdin"
			| "dump" | "dump_dependencies" | "dump_ignore_var_ids" -> acc
			| _ -> (k ^ "=" ^ v) :: acc
		) def.values [] in
		let str = String.concat "@" (List.sort compare defines) in
		let s = Digest.string str in
		def.defines_signature <- Some s;
		s

let is_haxe3_compat def = raw_defined def "hx3compat"
