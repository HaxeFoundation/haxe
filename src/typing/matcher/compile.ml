open Globals
open Ast
open Type
open MatcherGlobals
open Error
open Subject
open Typecore
open DecisionTree
open Case
open Constructor
open Pattern
open Bind

exception Extractor

module ConTable = Hashtbl.Make(Constructor)

module DtTable = Hashtbl.Make(DecisionTree)

type matcher_context = {
	ctx : typer;
	dt_table : dt DtTable.t;
	match_pos : pos;
	match_debug : bool;
	mutable dt_count : int;
}

let hashcons mctx dt p =
	try
		DtTable.find mctx.dt_table dt
	with Not_found ->
		let dti = {dt_t = dt; dt_i = mctx.dt_count; dt_pos = p; dt_goto_target = false; dt_texpr = None } in
		DtTable.add mctx.dt_table dt dti;
		mctx.dt_count <- mctx.dt_count + 1;
		dti

let leaf mctx case = hashcons mctx (Leaf case) case.case_pos
let fail mctx p = hashcons mctx Fail p
let switch mctx subject cases default = hashcons mctx (Switch(subject,cases,default)) subject.epos
let bind mctx bindings dt = hashcons mctx (Bind(bindings,dt)) dt.dt_pos
let guard mctx e dt1 dt2 = hashcons mctx (Guard(e,dt1,dt2)) (punion dt1.dt_pos dt2.dt_pos)
let guard_null mctx e dt1 dt2 = hashcons mctx (GuardNull(e,dt1,dt2)) (punion dt1.dt_pos dt2.dt_pos)

let get_sub_subjects mctx e con arg_positions =
	match fst con with
	| ConEnum(en,ef) ->
		let tl = Monomorph.spawn_constrained_monos (fun t -> t) en.e_params in
		let t_en = TEnum(en,tl) in
		let e = if not (type_iseq t_en e.etype) then mk (TCast(e,None)) t_en e.epos else e in
		begin match follow ef.ef_type with
			| TFun(args,_) ->
				let rec combine args positions =
					match (args, positions) with
						| (a :: args, p :: positions) -> (a, p) :: combine args positions
						| (a :: args, []) -> (a, e.epos) :: combine args positions
						| _ -> []
				in
				let arg_and_pos = combine args arg_positions in
				ExtList.List.mapi
					(fun i ((n,_,t), p) ->
						let params = apply_params en.e_params tl (monomorphs ef.ef_params t) in
						(n,mk (TEnumParameter({ e with epos = p },ef,i)) params p)
					)
					arg_and_pos
			| _ ->
				[]
		end
	| ConFields sl ->
		List.map (fun s -> s,ExprToPattern.type_field_access mctx.ctx e s) sl
	| ConArray 0 -> []
	| ConArray i ->
		ExtList.List.init i (fun i ->
			let ei = Texpr.Builder.make_int mctx.ctx.com.basic i e.epos in
			Printf.sprintf "a%i" i,Calls.acc_get mctx.ctx (Calls.array_access mctx.ctx e ei MGet e.epos)
		)
	| ConConst _ | ConTypeExpr _ | ConStatic _ ->
		[]

let specialize subject con cases =
	let arity = arity con in
	let rec specialize (case,bindings,patterns) = match patterns with
		| (PatConstructor(con',patterns1),_) :: patterns2 when Constructor.equal con con' ->
			Some (case,bindings,patterns1 @ patterns2)
		| (PatAny,_) as pat :: patterns2 ->
			Some (case,bindings,ExtList.List.make arity pat @ patterns2)
		| (PatBind(v,pat1),p) :: patterns ->
			specialize (case,(make_bind v p subject) :: bindings,pat1 :: patterns)
		| _ ->
			None
	in
	ExtList.List.filter_map specialize cases

let default subject cases =
	let rec default (case,bindings,patterns) = match patterns with
		| (PatAny,_) :: patterns ->
			Some (case,bindings,patterns)
		| (PatBind(v,pat1),p) :: patterns ->
			default (case,((make_bind v p subject) :: bindings),pat1 :: patterns)
		| _ ->
			None
	in
	ExtList.List.filter_map default cases

let rec is_wildcard_pattern pat = match fst pat with
	| PatAny -> true
	| PatBind(_,pat1) -> is_wildcard_pattern pat1
	| _ -> false

let expand cases =
	let rec expand f (case,bindings,patterns) = match patterns with
		| (PatOr(pat1,pat2),_) :: patterns ->
			(expand f (case,bindings,pat1 :: patterns)) @ (expand f (case,bindings,pat2 :: patterns))
		| (PatBind(v,pat1),p) :: patterns ->
			expand (fun pat2 -> f (PatBind(v,pat2),p)) (case,bindings,pat1 :: patterns)
		| (PatTuple patterns1,_) :: patterns2 ->
			expand f (case,bindings,patterns1 @ patterns2)
		| pat :: patterns ->
			[(case,bindings,f pat :: patterns)]
		| [] ->
			[(case,bindings,patterns)]
	in
	List.flatten (List.map (expand (fun pat -> pat)) cases)

let s_subjects subjects =
	String.concat " " (List.map (fun subject -> subject#to_string) subjects)

let s_case (case,bindings,patterns) =
	let bind_init e =
		Printf.sprintf " = %s" (s_expr_pretty e)
	in
	let s_bindings = String.concat ", " (List.map (fun bind -> Printf.sprintf "%s<%i>%s" bind.b_var.v_name bind.b_var.v_id (bind_init bind.b_expr)) bindings) in
	let s_patterns = String.concat " " (List.map Pattern.to_string patterns) in
	let s_expr = match case.case_expr with None -> "" | Some e -> Type.s_expr_pretty false "\t\t" false s_type e in
	let s_guard = match case.case_guard with None -> "" | Some e -> Type.s_expr_pretty false "\t\t" false s_type e in
	Printf.sprintf "\n\t\tbindings: %s\n\t\tpatterns: %s\n\t\tguard: %s\n\t\texpr: %s" s_bindings s_patterns s_guard s_expr

let s_cases cases =
	String.concat "\n" (List.map s_case cases)

let select_column subjects cases =
	let rec loop i patterns = match patterns with
		| ((PatAny | PatExtractor _),_) :: patterns -> loop (i + 1) patterns
		| (PatBind(_,pat1),_) :: patterns -> loop i (pat1 :: patterns)
		| [] -> 0
		| _ -> i
	in
	let _,_,patterns = List.hd cases in
	let i = loop 0 patterns in
	let subjects,cases = if i = 0 then
		subjects,cases
	else begin
		let rec sort i cur acc l = match l with
			| x :: l ->
				if i = cur then x :: acc @ l
				else sort i (cur + 1) (x :: acc) l
			| [] ->
				acc
		in
		let subjects = sort i 0 [] subjects in
		let cases = List.map (fun (case,bindings,patterns) ->
			let patterns = sort i 0 [] patterns in
			case,bindings,patterns
		) cases in
		subjects,cases
	end in
	subjects,cases

let rec compile mctx subjects cases = match cases with
	| [] ->
		fail mctx (match subjects with subject :: _ -> subject#get_pos | _ -> mctx.match_pos);
	| (_,_,patterns) as case :: cases when List.for_all is_wildcard_pattern patterns ->
		compile_leaf mctx subjects case cases
	| _ ->
		let cases = expand cases in
		let subjects,cases = select_column subjects cases in
		let cases = expand cases in (* TODO: is this really necessary? *)
		try
			compile_switch mctx subjects cases
		with Extractor ->
			compile_extractors mctx subjects cases

and compile_leaf mctx subjects (case,bindings,patterns) cases =
	if mctx.match_debug then print_endline (Printf.sprintf "compile_leaf:\n\tsubjects: %s\n\tcase: %s\n\tcases: %s" (s_subjects subjects) (s_case (case,bindings,patterns)) (s_cases cases));
	let dt = leaf mctx case in
	let dt = match case.case_guard with
		| None ->
			dt
		| Some e ->
			let dt2 = compile mctx subjects cases in
			guard mctx e dt dt2
	in
	let rec loop patterns subjects bindings = match patterns,subjects with
		| [PatAny,_],_ ->
			bindings
		| (PatBind(v,pat1),p) :: patterns,subject :: subjects ->
			loop (pat1 :: patterns) (subject :: subjects) ((make_bind v p subject#get_assigned_expr) :: bindings)
		| _ :: patterns,_ :: subjects ->
			loop patterns subjects bindings
		| [],[] ->
			bindings
		| [],subject :: _ ->
			raise_typing_error "Invalid match: Not enough patterns" subject#get_pos
		| (_,p) :: _,[] ->
			raise_typing_error "Invalid match: Too many patterns" p
	in
	let bindings = loop patterns subjects bindings in
	if bindings = [] then dt else bind mctx (List.rev bindings) dt

and compile_switch mctx subjects cases =
	let subject,subjects = match subjects with
		| [] -> raise Internal_match_failure
		| subject :: subjects -> subject,subjects
	in
	let switch_subject,reset_subject = subject#get_scoped_expr in
	let subject = subject#get_expr in
	let get_column_sigma cases =
		let sigma = ConTable.create 0 in
		let unguarded = ConTable.create 0 in
		let null = ref [] in
		List.iter (fun (case,bindings,patterns) ->
			let rec loop bindings pat = match fst pat with
				| PatConstructor((ConConst TNull,_),_) ->
					null := (case,bindings,List.tl patterns) :: !null;
				| PatConstructor(con,patterns) ->
					if case.case_guard = None then ConTable.replace unguarded con true;
					let arg_positions = snd (List.split patterns) in
					ConTable.replace sigma con arg_positions;
				| PatBind(_,(PatAny,_)) ->
					()
				| PatBind(v,pat1) ->
					loop ((make_bind v (pos pat) subject) :: bindings) pat1
				| PatAny -> ()
				| PatExtractor _ -> raise Extractor
				| _ -> raise_typing_error ("Unexpected pattern: " ^ (Pattern.to_string pat)) case.case_pos;
			in
			loop bindings (List.hd patterns)
		) cases;
		let sigma = ConTable.fold (fun con arg_positions acc -> (con,ConTable.mem unguarded con,arg_positions) :: acc) sigma [] in
		let sigma = List.sort (fun ((_,p1),_,_)  ((_,p2),_,_) -> p1.pmin - p2.pmin) sigma in
		sigma,List.rev !null
	in
	let sigma,null = get_column_sigma cases in
	if mctx.match_debug then print_endline (Printf.sprintf "compile_switch:\n\tsubject: %s\n\ttsubjects: %s\n\tcases: %s" (s_expr_pretty subject) (s_subjects subjects) (s_cases cases));
	let switch_cases = List.map (fun (con,unguarded,arg_positions) ->
		let sub_subjects = get_sub_subjects mctx subject con arg_positions in
		let rec loop bindings locals sub_subjects = match sub_subjects with
			| (name,e) :: sub_subjects ->
				let v = add_local mctx.ctx VGenerated (Printf.sprintf "%s%s" gen_local_prefix name) e.etype e.epos in
				let ev = mk (TLocal v) v.v_type v.v_pos in
				let bind = make_bind_gen v v.v_pos e BindUnused in
				let subject = new subject ev (Some bind) in
				loop (bind :: bindings) (subject :: locals) sub_subjects
			| [] ->
				List.rev bindings,List.rev locals
		in
		let bindings,sub_subjects = loop [] [] sub_subjects in
		let subjects = sub_subjects @ subjects in
		let spec = specialize subject con cases in
		let dt = compile mctx subjects spec in
		let dt = bind mctx bindings dt in
		{
			sc_con = con;
			sc_unguarded = unguarded;
			sc_dt = dt;
		}
	) sigma in
	(* This is very awkward: We need to know the first occurrence of the subject expression in the
	   decision tree before recursing. Ideally, this would be handled automatically during the recursion,
	   but for this to work we would have to replace some texpr occurrences with subject. *)
	let (subject_null,subject_switch,subject_default),reset_subject =
		let subjects = match null with
			| [] ->
				if is_explicit_null subject.etype then
					switch_subject,subject,subject
				else begin match switch_cases with
				| [] ->
					subject,subject,switch_subject
				| _ ->
					subject,switch_subject,subject
				end
			| _ ->
				switch_subject,subject,subject
		in
		(subjects,reset_subject)
	in
	let default = default subject_default cases in
	let switch_default = compile mctx subjects default in
	let dt = if switch_cases = [] then switch_default else switch mctx subject_switch switch_cases switch_default in
	let null_guard dt_null =
		guard_null mctx subject_null dt_null dt
	in
	let dt = match null with
		| [] ->
			if is_explicit_null subject.etype then null_guard switch_default else dt
		| cases ->
			let dt_null = compile mctx subjects (cases @ default) in
			null_guard dt_null
	in
	reset_subject();
	dt

and compile_extractors mctx subjects cases =
	let subject,subjects = match subjects with
		| [] -> raise Internal_match_failure
		| subject :: subjects -> subject,subjects
	in
	let subject = subject#get_expr in
	if mctx.match_debug then print_endline (Printf.sprintf "compile_extractor:\n\tsubject: %s\n\ttsubjects: %s\n\tcases: %s" (s_expr_pretty subject) (s_subjects subjects) (s_cases cases));
	(* Find all extractors of the current column and associate them with bindings, if exist *)
	let num_extractors,cases = List.fold_left (fun (i,extractors) ((_,_,patterns) as case) ->
		let rec loop bindings pat = match pat with
			| (PatExtractor ex,_) ->
				i + 1,(case,Some (ex,i + 1,bindings)) :: extractors
			| (PatBind(v,pat1),p) ->
				loop ((make_bind v p subject) :: bindings) pat1
			| _ ->
				i,(case,None) :: extractors
		in
		loop [] (List.hd patterns)
	) (0,[]) cases in
	let pat_any = (PatAny,null_pos) in
	let lookup_expr lut e =
		let rec loop el = match el with
			| [] ->
				None
			| (e',ev) :: el ->
				if Texpr.equal e e' then Some ev else loop el
		in
		loop lut
	in
	let rec loop acc_cases acc_subjects acc_bind expr_lut cases = match cases with
		| ((_,_,[]),_) :: _ ->
			die "" __LOC__
		 | ((case,bindings,(pattern1 :: patterns)),ex) :: cases ->
			begin match ex with
			| None ->
				(* If there's no extractor, generate `[pattern1, _, ..., _ ]` *)
				let patterns = make_offset_list 0 num_extractors pattern1 pat_any @ patterns in
				loop ((case,bindings,patterns) :: acc_cases) acc_subjects acc_bind expr_lut cases
			| Some (ex,i,bindings1) ->
				(* Replace the _ local with our subject *)
				let rec replace e = match e.eexpr with
					| TLocal v' when v' == ex.ex_var -> subject
					| _ -> Type.map_expr replace e
				in
				let e1 = replace ex.ex_expr in
				let bindings = bindings1 @ bindings in
				(* For the patterns, generate `[_, ..., extractorPatternI, ..., _] *)
				let make_patterns i = make_offset_list i (num_extractors - i) ex.ex_pattern pat_any @ patterns in
				(* See if we already had an equal extractor expression. In that case we can reuse its _hx_tmp *)
				begin match lookup_expr expr_lut e1 with
				| None ->
					(* Generate a local and add it to the subjects so that they become `[subject1, ..., localI, ...]` *)
					let v = alloc_var VExtractorVariable "_hx_tmp" e1.etype e1.epos in
					let ev = mk (TLocal v) v.v_type e1.epos in
					let bind = make_bind_gen v v.v_pos e1 BindDeferred in
					let patterns = make_patterns i in
					let subject = new subject ev (Some bind) in
					loop ((case,bindings,patterns) :: acc_cases) (subject :: acc_subjects) (bind :: acc_bind) ((e1,(subject,i)) :: expr_lut) cases
				| Some(subject,i) ->
					let patterns = make_patterns i in
					loop ((case,bindings,patterns) :: acc_cases) (subject :: acc_subjects) acc_bind expr_lut cases
				end
			end
		| [] ->
			List.rev acc_cases,List.rev acc_subjects,List.rev acc_bind
	in
	let cases,ex_subjects,ex_binds = loop [] [] [] [] (List.rev cases) in
	(* At the end of all this we have something like this:
		var _hx_tmp1 = extractorLhs1(subject);
		var _hx_tmpN = extractorLhsN(subject);
		switch [subject, _hx_tmp1, _hx_tmpN] {
			case [normalSubjectMatch, _, _]:
			case [_, extractorRhs1, _]:
			case [_, _, extractorRhsN]:
		}
	*)
	let subject = new subject subject None in
	let dt = compile mctx ((subject :: ex_subjects) @ subjects) cases in
	bind mctx ex_binds dt

let compile ctx match_debug subjects cases p =
	let mctx = {
		ctx = ctx;
		match_debug = match_debug;
		dt_table = DtTable.create 7;
		match_pos = p;
		dt_count = 0;
	} in
	let rec loop (subjects,vars) el = match el with
		| [] ->
			List.rev subjects,List.rev vars
		| e :: el ->
			let subjects,vars = match e.eexpr with
			| TConst _ | TLocal _ ->
				(new subject e None:: subjects,vars)
			| _ ->
				let v = gen_local ctx e.etype e.epos in
				let ev = mk (TLocal v) e.etype e.epos in
				let bind = make_bind v e.epos e in
				(new subject ev (Some bind) :: subjects,bind :: vars)
			in
			loop (subjects,vars) el
	in
	let subjects,vars = loop ([],[]) subjects in
	begin match cases,subjects with
	| [],(subject :: _) ->
		let dt_fail = fail mctx subject#get_pos in
		switch mctx subject#get_expr [] dt_fail
	| _ ->
		let dt = compile mctx subjects cases in
		Useless.check mctx.ctx cases;
		match vars with
			| [] -> dt
			| _ -> bind mctx vars dt
	end
