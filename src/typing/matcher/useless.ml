open Globals
open Ast
open Pattern
open Constructor
open Case

(*
	Implements checks for useless patterns based on http://moscova.inria.fr/~maranget/papers/warn/index.html.
*)

type useless =
	| False
	| Pos of pos
	| True

(* U part *)

let specialize is_tuple con pM =
	let rec loop acc pM = match pM with
		| patterns :: pM ->
			begin match patterns with
				| (PatConstructor(con',patterns1),_) :: patterns2 when not is_tuple && Constructor.equal con con' ->
					loop ((patterns1 @ patterns2) :: acc) pM
				| (PatTuple patterns1,_) :: patterns2 when is_tuple ->
					loop ((patterns1 @ patterns2) :: acc) pM
				| (PatAny,p) :: patterns2 ->
					let patterns1 = ExtList.List.make (arity con) (PatAny,p) in
					loop ((patterns1 @ patterns2) :: acc) pM
				| (PatBind(_,pat1),_) :: patterns2 ->
					loop acc ((pat1 :: patterns2) :: pM)
				| _ ->
					loop acc pM
			end
		| [] ->
			List.rev acc
	in
	loop [] pM

let default pM =
	let rec loop acc pM = match pM with
		| patterns :: pM ->
			begin match patterns with
				| ((PatBind(_,(PatAny,_)) | PatAny),_) :: patterns ->
					loop (patterns :: acc) pM
				| _ ->
					loop acc pM
			end
		| [] ->
			List.rev acc
	in
	loop [] pM

let rec u pM q =
	match q,pM with
	| [],[] -> true
	| [],_ -> false
	| (q1 :: ql),_ ->
		let rec loop pat = match fst pat with
			| PatConstructor(con,patterns) ->
				let s = specialize false con pM in
				u s (patterns @ ql)
			| PatTuple patterns ->
				let s = specialize true (ConConst TNull,pos pat) pM in
				u s (patterns @ ql)
			| PatAny ->
				let d = default pM in
				u d ql
			| PatOr(pat1,pat2) ->
				u pM (pat1 :: ql) || u pM (pat2 :: ql)
			| PatBind(_,pat1) ->
				loop pat1
			| PatExtractor _ ->
				true (* ? *)
		in
		loop q1

(* U' part *)

let transfer_column source target =
	let source,target = List.fold_left2 (fun (source,target) patterns1 patterns2 -> match patterns1 with
		| pat :: patterns -> patterns :: source,(pat :: patterns2) :: target
		| [] -> source,target
	) ([],[]) source target in
	List.rev source,List.rev target

let copy p = List.map (fun _ -> []) p

let specialize' is_tuple con pM qM rM =
	let arity = arity con in
	let rec loop pAcc qAcc rAcc pM qM rM = match pM,qM,rM with
		| p1 :: pM,q1 :: qM,r1 :: rM ->
			let rec loop2 p1 = match p1 with
				| (PatConstructor(con',patterns1),_) :: patterns2 when not is_tuple && Constructor.equal con con' ->
					loop ((patterns1 @ patterns2) :: pAcc) (q1 :: qAcc) (r1 :: rAcc) pM qM rM
				| (PatTuple patterns1,_) :: patterns2 when is_tuple ->
					loop ((patterns1 @ patterns2) :: pAcc) (q1 :: qAcc) (r1 :: rAcc) pM qM rM
				| (PatAny,p) :: patterns2 ->
					let patterns1 = ExtList.List.make arity (PatAny,p) in
					loop ((patterns1 @ patterns2) :: pAcc) (q1 :: qAcc) (r1 :: rAcc) pM qM rM
				| (PatOr(pat1,pat2),_) :: patterns2 ->
					loop pAcc qAcc rAcc (((pat1 :: patterns2) :: (pat2 :: patterns2) :: pM)) (q1 :: q1 :: qM) (r1 :: r1 :: rM)
				| (PatBind(_,pat1),_) :: patterns2 ->
					loop2 (pat1 :: patterns2)
				| _ ->
					loop pAcc qAcc rAcc pM qM rM
			in
			loop2 p1
		| [],_,_ ->
			List.rev pAcc,List.rev qAcc,List.rev rAcc
		| _ ->
			die "" __LOC__
	in
	loop [] [] [] pM qM rM

let combine et1 et2 = match fst et1,fst et2 with
	| True,True -> True
	| False,False -> False
	| True,False -> Pos (pos et2)
	| False,True -> Pos (pos et1)
	| True,Pos _ -> fst et2
	| Pos _,True -> fst et1
	| False,Pos _ -> Pos (pos et1)
	| Pos _,_ -> fst et1

let rec u' pM qM rM p q r =
	match p with
	| [] ->
		begin match r with
			| [] -> if u qM q then True else False
			| _ ->
				snd (List.fold_left (fun (i,et) pat -> match fst pat with
					| PatOr(pat1,pat2) ->
						 let process_row i l q =
							 let rec loop acc k l = match l with
								 | x :: l when i = k -> x,(List.rev acc) @ l @ q
								 | x :: l -> loop (x :: acc) (k + 1) l
								 | [] -> die "" __LOC__
							 in
							 loop [] 0 l
						 in
						let col,mat = List.fold_left2 (fun (col,mat) r q ->
							 let x,l = process_row i r q in
							 ([x] :: col,l :: mat)
						 ) ([],[]) rM qM in
						 let col,mat = List.rev col,List.rev mat in
						let _,r = process_row i r q in
						let et1 = u' col mat (copy mat) [pat1] r [] in
						let qM = (mat @ [r]) in
						let et2 = u' (col @ [[pat1]]) qM (copy qM) [pat2] r [] in
						let et3 = combine (et1,pos pat1) (et2,pos pat2) in
						let p = punion (pos pat1) (pos pat2) in
						let et = combine (et,p) (et3,p) in
						(i + 1,et)
					| _ -> die "" __LOC__
				) (0,True) r)
		end
	| (pat :: pl) ->
		let rec loop pat = match fst pat with
			| PatConstructor(con,patterns) ->
				let pM,qM,rM = specialize' false con pM qM rM in
				u' pM qM rM (patterns @ pl) q r
			| PatTuple patterns ->
				let pM,qM,rM = specialize' true (ConConst TNull,pos pat) pM qM rM in
				u' pM qM rM (patterns @ pl) q r
			| PatAny ->
				let pM,qM = transfer_column pM qM in
				u' pM qM rM pl (pat :: q) r
			| PatOr _ ->
				let pM,rM = transfer_column pM rM in
				u' pM qM rM pl q (pat :: r)
			| PatBind(_,pat1) ->
				loop pat1
			| PatExtractor _ ->
				True
		in
		loop pat

(* Sane part *)

let check_case ctx p (case,bindings,patterns) =
	let p = List.map (fun (_,_,patterns) -> patterns) p in
	match u' p (copy p) (copy p) patterns [] [] with
		| False -> Typecore.warning ctx WUnusedPattern "This case is unused" case.case_pos
		| Pos p -> Typecore.warning ctx WUnusedPattern "This pattern is unused" p
		| True -> ()

let check ctx cases =
	ignore(List.fold_left (fun acc (case,bindings,patterns) ->
		check_case ctx acc (case,bindings,patterns);
		if case.case_guard = None then acc @ [case,bindings,patterns] else acc
	) [] cases)
