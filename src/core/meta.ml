open Globals
include MetaList

let has m ml = List.exists (fun (m2,_,_) -> m = m2) ml
let has_one_of ml1 ml2 = List.exists (fun (m2,_,_) -> List.mem m2 ml1) ml2
let get m ml = List.find (fun (m2,_,_) -> m = m2) ml

let rec remove m = function
	| [] -> []
	| (m2,_,_) :: l when m = m2 -> l
	| x :: l -> x :: remove m l

let to_string m = fst (get_info m)

let hmeta =
	let h = Hashtbl.create 0 in
	let rec loop i =
		let m = Obj.magic i in
		if m <> Last then begin
			Hashtbl.add h (fst (get_info m)) m;
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

let get_documentation d =
	let t, (doc,flags) = get_info d in
	if not (List.mem UsedInternally flags) then begin
		let params = ref [] and used = ref [] and pfs = ref [] in
		List.iter (function
			| HasParam s -> params := s :: !params
			| Platforms fl -> pfs := fl @ !pfs
			| UsedOn ul -> used := ul @ !used
			| UsedInternally -> assert false
			| Link _ -> ()
		) flags;
		let params = (match List.rev !params with
			| [] -> ""
			| l -> "(<" ^ String.concat ">, <" l ^ ">) "
		) in
		let pfs = platform_list_help (List.rev !pfs) in
		let str = "@" ^ t in
		Some (str,params ^ doc ^ pfs)
	end else
		None

let get_documentation_list () =
	let m = ref 0 in
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then begin match get_documentation d with
			| None -> loop (i + 1)
			| Some (str,desc) ->
				if String.length str > !m then m := String.length str;
					(str,desc) :: loop (i + 1)
		end else
			[]
	in
	let all = List.sort (fun (s1,_) (s2,_) -> String.compare s1 s2) (loop 0) in
	all,!m

let get_all () =
	let rec loop i =
		let d = Obj.magic i in
		if d <> Last then d :: loop (i + 1)
		else []
	in
	loop 0

let copy_from_to m src dst =
	try (get m src) :: dst
	with Not_found -> dst