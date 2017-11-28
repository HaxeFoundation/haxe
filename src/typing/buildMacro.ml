open Ast
open Common
open Type
open Typecore
open Error


type candidate =
	| Candidate of pos * path * string * recover_mode

and recover_mode =
	| RMBoth
	| RMMethod
	| RMModule
	| RMNo


let candidate_pos  = function
	| Candidate(p,_,_,_) -> p

let apply_macro ctx mode path el p =
	let rev_path = List.rev path in
	let starts_with_uppercase s =
		if String.length s > 0 then
			let first_char = String.get s 0 in
			(Char.lowercase first_char) != first_char
		else
			false
	in
	let check_candidates candidates try_absolute_path  =
		let sort_candidates c =
			List.sort (fun c1 c2 -> begin
				let p1 = candidate_pos c1 in
				let p2 = candidate_pos c2 in
				compare p2 p1
			end) c
		in
		let run (cpath, meth) rmode =
			try
				ctx.g.do_macro ctx mode cpath meth el p
			with e -> match rmode, e with
				| (RMBoth | RMModule), Error (Module_not_found _, p) -> None
				| (RMBoth | RMMethod), Error (Method_not_found _, p) -> None
				| _ -> raise e
		in
		let candidates = sort_candidates candidates in
		(* iterate all candidates *)
		let res = List.fold_left (fun acc c -> begin
			match c, acc with
			| _, (_, RMNo) | _, (Some _, _) ->
				acc (* we already have a result *)
			| Candidate (_, path, meth, recover_mode), (None, _) ->
				(run (path, meth) recover_mode, recover_mode)
		end) (None, RMBoth) candidates in

		match (fst res), try_absolute_path with
			| None, true ->
				(* just try as an absolute path *)
				let cpath, meth = match rev_path with
				| meth :: name :: pack ->
					(List.rev pack,name), meth
				| _ ->
					error "Invalid macro path" p
				in
				ctx.g.do_macro ctx mode cpath meth el p
			| None, false ->
				error "Invalid macro path" p
			| res, _ -> res
	in
	let candidates, try_absolute = (match rev_path with
		| meth :: name :: [] when (starts_with_uppercase name) -> begin
			(* maybe it's an imported class (also consider wildcard packages) *)
			let get_path pack name meth =
				let pack = List.map fst pack in
				((List.rev pack,name), meth)
			in
			let candidates = List.fold_left (fun a (l, mode) ->
				match (List.rev l), mode with
				| (name1,pos) :: pack, (IAsName s) when s = name ->
					let (p, m) = (get_path pack name1 meth) in
					let c = Candidate(pos, p, m, RMNo) in
					c::a
				| (name1,pos) :: pack, (INormal) when name1 = name ->
					let (p,m) = (get_path pack name1 meth) in
					let c = Candidate(pos, p, m, RMNo) in
					c::a
				| _ -> a
			) [] ctx.m.module_imports in
			(* allow error recovering for wildcard candidates *)
			let mk_wildcard_candidates name meth =
				List.fold_left (fun a (pack, pos) ->
					let c = Candidate(pos, (pack, name), meth, RMModule) in
					c::a
				) [] ctx.m.wildcard_packages
			in
			let candidates = candidates @ (mk_wildcard_candidates name meth) in
			candidates, true
		end
		| meth :: [] -> begin
			(* maybe it's just a function *)
			let get_path pack name meth =
				let pack = List.map fst pack in
				((List.rev pack,name), meth)
			in
			let candidates = List.fold_left (fun a (l, mode) ->
				match (List.rev l), mode with
				| (meth1,pos) :: (name,_) :: pack, (IAsName s) when s = meth ->
					let (p,m) = get_path pack name meth1 in
					let c = Candidate(pos, p, m, RMNo) in
					c :: a
				| (meth1,pos) :: (name,_) :: pack, INormal when meth1 = meth ->
				let (p,m) = (get_path pack name meth1) in
					let c = Candidate(pos, p, m, RMNo) in
					c :: a
				| (name,pos) :: pack, IAll when (starts_with_uppercase name) ->
					let pack = List.map fst pack in
					let c = Candidate(pos, (List.rev pack,name), meth, RMBoth) in
					c :: a (* allow error recovering for IAll candiates *)
				| _ -> a
			) [] ctx.m.module_imports in
			candidates, false
		end
		| meth :: sub :: name :: [] when (starts_with_uppercase sub) && (starts_with_uppercase name) -> begin
			(* maybe it's a subtype of an imported module *)
			let get_path pack name sub meth =
				let pack = name :: (List.map fst pack) in
				((List.rev pack,sub), meth)
			in
			let candidates = List.fold_left (fun a (l, mode) ->
				match (List.rev l), mode with
				| (name1,pos) :: pack, (IAsName s) when s = name ->
					let p,m = get_path pack name1 sub meth in
					let c = Candidate(pos, p, m, RMNo) in
					c :: a
				| (name1,pos) :: pack, (INormal | IAll) when name1 = name ->
					let p, m = get_path pack name1 sub meth in
					let c = Candidate(pos, p, m, RMNo) in
					c::a
				| _ -> a
			) [] ctx.m.module_imports in
			candidates, false
		end
		| _ ->
			(* we don't have import candidates, just try as an absolute path *)
			[], true

	) in
	check_candidates candidates try_absolute
