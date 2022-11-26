open Globals
include MetaList

let has m ml = List.exists (fun (m2,_,_) -> m = m2) ml
let has_one_of ml1 ml2 = List.exists (fun (m2,_,_) -> List.mem m2 ml1) ml2
let get m ml = List.find (fun (m2,_,_) -> m = m2) ml

let rec remove m = function
	| [] -> []
	| (m2,_,_) :: l when m = m2 -> l
	| x :: l -> x :: remove m l

type user_meta = {
	doc : string;
	flags : meta_parameter list;
	source : string option;
}

type meta_origin =
	| Compiler
	| UserDefined of string option

let register_user_meta user_metas s data =
	Hashtbl.replace user_metas s data

let get_info ?user_metas m = match (user_metas,m) with
	| (Some(user_metas), Custom(s)) when (Hashtbl.mem user_metas s) ->
		let infos = Hashtbl.find user_metas s in
		(s, (infos.doc, infos.flags), (UserDefined infos.source))
	| _ ->
		let meta,infos = MetaList.get_info m in
		(meta, infos, Compiler)

let to_string m = match (get_info m) with (s,_,_) -> s

let hmeta =
	let h = Hashtbl.create 0 in
	let rec loop i =
		let m = Obj.magic i in
		if m <> Last then begin
			Hashtbl.add h (to_string m) m;
			loop (i + 1);
		end;
	in
	loop 0;
	h

let parse s = try Hashtbl.find hmeta (":" ^ s) with Not_found -> Custom (":" ^ s)

let from_string s =
	if s = "" then Custom "" else match s.[0] with
	| ':' -> (try Hashtbl.find hmeta s with Not_found -> Custom s)
	| '$' -> Dollar (String.sub s 1 (String.length s - 1))
	| _ -> Custom s

let get_documentation user_metas d =
	let t, (doc,flags), origin = get_info ~user_metas:user_metas d in
	if not (List.mem UsedInternally flags) then begin
		let params = ref [] and used = ref [] and pfs = ref [] in
		List.iter (function
			| HasParam s -> params := s :: !params
			| Platforms fl -> pfs := fl @ !pfs
			| UsedOn ul -> used := ul @ !used
			| UsedInternally -> die "" __LOC__
			| Link _ -> ()
		) flags;
		let params = (match List.rev !params with
			| [] -> ""
			| l -> "(<" ^ String.concat ">, <" l ^ ">) "
		) in
		let pfs = platform_list_help (List.rev !pfs) in
		let origin = match origin with
			| UserDefined Some s -> " (from " ^ s ^ ")"
			| Compiler | UserDefined None -> ""
		in
		let str = "@" ^ t in
		Some (str,params ^ doc ^ pfs ^ origin)
	end else
		None

let get_documentation_list user_metas =
	let m = ref 0 in
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then begin match get_documentation user_metas d with
			| None -> loop (i + 1)
			| Some (str,desc) ->
				if String.length str > !m then m := String.length str;
					(str,desc) :: loop (i + 1)
		end else
			[]
	in
	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
	all,!m

let get_all user_metas =
	let rec loop i acc =
		let d = Obj.magic i in
		if d <> Last then d :: loop (i + 1) acc
		else acc
	in

	let all = loop 0 (Hashtbl.fold (fun str _ acc -> (Custom str) :: acc) user_metas []) in
	List.sort (fun m1 m2 -> String.compare (to_string m1) (to_string m2)) all

let get_user_documentation_list user_metas =
	let m = ref 0 in
	let user_meta_list = (Hashtbl.fold (fun meta _ acc ->
		begin match get_documentation user_metas (Custom meta) with
			| None -> acc
			| Some (str, desc) ->
				if String.length str > !m then m := String.length str;
				(str,desc) :: acc
		end
	) user_metas []) in

	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) user_meta_list in
	all,!m

let copy_from_to m src dst =
	try (get m src) :: dst
	with Not_found -> dst
