open Ast
open Common
open Type
open Typecore

type con_def =
	| CEnum of tenum * tenum_field
	| CConst of tconstant
	| CAnon of int * (string * tclass_field) list
	| CType of module_type
	| CArray of int

type con = con_def * pos

type subterm_def =
	| SVar of tvar
	| SSub of subterm * int

and subterm = subterm_def * pos

type pattern_def =
	| PatAny
	| PatVar of subterm
	| PatCon of con * pattern list
	| PatOr of pattern * pattern
	| PatBind of tvar * pattern

and pattern = {
	pdef : pattern_def;
	ptype : t;
	ppos : Ast.pos;
}

type outcome = {
	mutable o_bindings : (tvar * subterm) list;
	o_expr : texpr;
	o_guard : texpr option;
	mutable o_paths : int;
	o_pos : pos;
	o_id : int;
}

(* TODO: should this be a pattern array instead for easier column access? *)
type pattern_row = pattern list * outcome

type pattern_matrix = pattern_row list

(* TODO: turn this into a dag with maximal sharing *)
type decision_tree =
	| Bind of outcome * decision_tree option
	| Switch of subterm * t * (con * decision_tree) list

type matcher = {
	ctx : typer;
	mutable outcomes : (pattern list,outcome) PMap.t;
	mutable value_only : bool;
	mutable num_outcomes : int;
}

type pattern_ctx = {
	mutable pc_locals : (string, tvar) PMap.t;
	mutable pc_sub_vars : (string, tvar) PMap.t option;
}

(* An unmatched pattern with its position *)
exception Not_exhaustive of pattern * int

let unify ctx a b p =
	try unify_raise ctx a b p with Error (Unify l,p) -> error (error_msg (Unify l)) p

(* An anonymous any pattern *)
let any = {
	pdef = PatAny;
	ppos = Ast.null_pos;
	ptype = t_dynamic
}

(* Returns the arity of a given constructor *)
let arity (con : con) = match fst con with
	| CEnum (_,{ef_type = TFun(args,_)}) -> List.length args
	| CEnum _ -> 0
	| CConst _ -> 0
	| CAnon (i,fl) -> i
	| CType mt -> 0
	| CArray i -> i

(* Creates a new outcome *)
let mk_outcome ctx e guard pat =
	let out = {
		o_bindings = [];
		o_expr = e;
		o_guard = guard;
		o_paths = 0;
		o_pos = (match pat with
			| [pat] -> pat.ppos
			| pat :: pl -> List.fold_left (fun p pat -> punion p pat.ppos) pat.ppos pl
			| [] -> assert false);
		o_id = ctx.num_outcomes;
	} in
	ctx.num_outcomes <- ctx.num_outcomes + 1;
	ctx.outcomes <- PMap.add pat out ctx.outcomes;
	out

(* Clones an outcome. This is used when or patterns are found to preserve bindings *)
let clone_outcome ctx out pat =
	try
		PMap.find [pat] ctx.outcomes
	with Not_found ->
		let out = {out with o_pos = pat.ppos} in
		ctx.outcomes <- PMap.add [pat] out ctx.outcomes;
		out

(* Binds a subterm to an outcome variable *)
let bind_subterm out v st =
	if not (List.mem_assq v out.o_bindings) then out.o_bindings <- (v,st) :: out.o_bindings

(* Printing *)

let s_const = function
	| TInt i -> Int32.to_string i
	| TFloat s -> s ^ "f"
	| TString s -> Printf.sprintf "\"%s\"" (Ast.s_escape s)
	| TBool b -> if b then "true" else "false"
	| TNull -> "null"
	| TThis -> "this"
	| TSuper -> "super"

let s_con = function
	| CEnum(_,ef) -> ef.ef_name
	| CConst TNull -> "_"
	| CConst c -> s_const c
	| CAnon (i,fl) -> (String.concat "," (List.map (fun (s,_) -> s) fl)) ^ ":"
	| CType mt -> s_type_path (t_path mt)
	| CArray i -> "[" ^(string_of_int i) ^ "]"

let rec s_subterm = function
	| SVar v,_ -> v.v_name
	| SSub (st,i),_ -> s_subterm st ^ "." ^ (string_of_int i)

let rec s_pattern pat = match pat.pdef with
	| PatVar v -> s_subterm v
	| PatCon ((c,_),[]) -> s_con c
	| PatCon ((c,_),pl) -> s_con c ^ "(" ^ (String.concat "," (List.map s_pattern pl)) ^ ")"
	| PatOr (pat1,pat2) -> s_pattern pat1 ^ " | " ^ s_pattern pat2
	| PatAny -> "_"
	| PatBind(v,pat) -> v.v_name ^ "=" ^ s_pattern pat

let rec s_pattern_vec pl =
	String.concat " " (List.map s_pattern pl)

let s_outcome out = (match out.o_bindings with
	| [] -> ""
	| _ -> "var " ^ String.concat ", " (List.map (fun (v,st) -> v.v_name ^ ":" ^ (s_type (print_context()) v.v_type) ^ " = " ^ (s_subterm st)) out.o_bindings))
		(* ^ "id: " ^ (string_of_int out.o_id) *)
	(* ^ (s_expr (s_type (print_context())) out.o_expr) *)

let rec s_pattern_matrix pmat =
	String.concat "\n" (List.map (fun (pl,out) -> (s_pattern_vec pl) ^ "->" ^ (s_outcome out)) pmat)

let rec s_decision_tree tabs tree = tabs ^ match tree with
	| Bind (out,None)->
		s_outcome out;
	| Bind (out,Some dt) ->
		"if (" ^ (s_expr (s_type (print_context())) (match out.o_guard with Some e -> e | None -> assert false)) ^ ") " ^ (s_outcome out) ^ " else " ^ s_decision_tree tabs dt
	| Switch (st, t, cl) ->
		"switch(" ^ (s_subterm st) ^ ":" ^ (s_type (print_context()) t) ^ ") { \n" ^ tabs
		^ (String.concat ("\n" ^ tabs) (List.map (fun ((c,_),dt) ->
			"case " ^ (s_con c) ^ ":\n" ^ (s_decision_tree (tabs ^ "\t") dt)
		) cl))
		^ "\n" ^ (if String.length tabs = 0 then "" else (String.sub tabs 0 (String.length tabs - 1))) ^ "}"

(* Decides if two constructors are equal *)
let con_eq c1 c2 = match fst c1,fst c2 with
	| CConst c1,CConst c2 ->
		c1 = c2
	| CEnum(e1,ef1),CEnum(e2,ef2) ->
		e1 == e2 && ef1.ef_name = ef2.ef_name
	| CAnon (i1,fl1),CAnon (i2,fl2) ->
		(try
			List.iter (fun (s,_) -> if not (List.mem_assoc s fl1) then raise Not_found) fl2;
			true
		with Not_found ->
			false)
	| CType mt1,CType mt2 ->
		t_path mt1 = t_path mt2
	| CArray a1, CArray a2 ->
		a1 == a2
	| _ ->
		false

(* Swaps column 0 and i in a given vector *)
(* TODO: optimize this *)
let swap_columns i (row : 'a list) : 'a list =
	match row with
	| rh :: rt ->
		let hd = ref rh in
		let rec loop count acc col = match col with
			| [] -> acc
			| ch :: cl when i = count ->
				let acc = acc @ [!hd] @ cl in
				hd := ch;
				acc
			| ch :: cl ->
				loop (count + 1) (ch :: acc) cl
		in
		let tl = loop 1 [] rt in
		(!hd :: tl)
	| _ ->
		[]

(* Convenience function to make a constructor pattern *)
let mk_con_pat c args t p = {
	pdef = PatCon((c,p),args);
	ptype = t;
	ppos = p;
}

(* Convenience function to make an any pattern *)
let mk_any t p = {
	pdef = PatAny;
	ptype = t;
	ppos = p;
}

let unify_enum_field en pl ef t =
	let t2 = match follow ef.ef_type with
		| TFun(_,r) -> r
		| t2 -> t2
	in
	let monos = List.map (fun _ -> mk_mono()) ef.ef_params in
	Type.unify (apply_params en.e_types pl (apply_params ef.ef_params monos t2)) t

(* Transform an expression to a pattern *)
(* TODO: sanity check this *)
let to_pattern ctx e t =
	let perror p = error "Unrecognized pattern" p in
	let verror n p = error ("Variable " ^ n ^ " must appear exactly once in each sub-pattern") p in
	let rec loop tctx e t = match e with
		| EParenthesis(e),_ ->
			loop tctx e t
		| ECall(ec,el),p ->
			let ec = type_expr_with_type ctx ec (Some t) false in
			(match follow ec.etype with
			| TAnon a -> (match !(a.a_status) with
				| Statics c when has_meta ":extractor" c.cl_meta ->
					let cf = try PMap.find "unapply" c.cl_statics with Not_found -> error "Missing extractor method unapply" c.cl_pos in
					let tcf = apply_params cf.cf_params (List.map (fun _ -> mk_mono()) cf.cf_params) (follow cf.cf_type) in
					(match tcf,el with
					| TFun([(_,_,ta)],r),[e] ->
						unify ctx t ta p;
						error ("Extractors are not supported yet") p;
					| TFun (_),[e] ->
						error "Method unapply must accept exactly 1 argument." cf.cf_pos;
					| TFun _,_ ->
						error "Invalid number of arguments to extractor, must be exactly 1" p
					| _ ->
						error "Invalid type for method unapply" cf.cf_pos)			
				| _ -> perror p)				
			| TEnum(en,pl)
			| TFun(_,TEnum(en,pl)) ->
				let ef = match ec.eexpr with
					| TEnumField(_,s)
					| TClosure ({ eexpr = TTypeExpr (TEnumDecl _) },s) -> PMap.find s en.e_constrs
					| _ -> error ("Expected constructor for enum " ^ (s_type_path en.e_path)) p
				in
				(try unify_enum_field en pl ef t with Unify_error l -> error (error_msg (Unify l)) p);
				let tl = match ef.ef_type with
					| TFun(args,_) -> List.map (fun (_,_,t) -> t) args
					| _ -> error "Arguments expected" p
				in
				let rec loop2 acc el tl = match el,tl with
					| (EConst(Ident "_"),_) as e :: [], t :: tl ->
						let pat = loop tctx e t_dynamic in
						(ExtList.List.make ((List.length tl) + 1) pat) @ acc
					| e :: el, t :: tl ->
						let pat = loop tctx e (apply_params en.e_types pl (apply_params ef.ef_params (List.map (fun _ -> mk_mono()) ef.ef_params) t)) in
						loop2 (pat :: acc) el tl
					| e :: _, [] ->
						error "Too many arguments" (pos e);
					| [],_ :: _ ->
						error "Not enough arguments" p;
					| [],[] ->
						acc
				in
				mk_con_pat (CEnum(en,ef)) (List.rev (loop2 [] el tl)) t p
			| _ -> perror p)
		| (EConst(Ident "null"),p) ->
			error "null-patterns are not allowed" p
		| (EConst((Ident ("false" | "true") | Int _ | String _ | Float _) as c),p) ->
			let e = Codegen.type_constant ctx.com c p in
			unify ctx e.etype t p;
			let c = match e.eexpr with TConst c -> c | _ -> assert false in
			mk_con_pat (CConst c) [] t p
		| (EConst(Ident "_"),p) ->
			{
				pdef = PatAny;
				ptype = t;
				ppos = p;
			}
		| (EField _,p) ->
			let e = type_expr_with_type ctx e (Some t) false in
			(match e.eexpr with
			| TConst c -> mk_con_pat (CConst c) [] t p
			| TTypeExpr mt -> mk_con_pat (CType mt) [] t p
			| _ -> error "Constant expression expected" p)
		| ((EConst(Ident s),p) as ec) -> (try
				(* HACK so type_ident via type_field does not cause display errors *)
				ctx.untyped <- true;
				let ec = try type_expr_with_type ctx ec (Some t) true with _ -> raise Not_found in
				ctx.untyped <- false;
				(* we might have found the wrong thing entirely *)
				(try unify_raise ctx t ec.etype ec.epos with Error (Unify _,_) -> raise Not_found);
				(match ec.eexpr with
					| TEnumField(en,s)
					| TField ({ eexpr = TTypeExpr (TEnumDecl en) },s) ->
						let ef = PMap.find s en.e_constrs in
						unify_enum_field en (List.map (fun _ -> mk_mono()) en.e_types) ef t;
						mk_con_pat (CEnum(en,ef)) [] t p
					| TTypeExpr mt ->
						mk_con_pat (CType mt) [] t p
					| _ ->
						raise Not_found);
			with Not_found ->
				let v = match tctx.pc_sub_vars with
					| Some vmap -> (try PMap.find s vmap with Not_found -> verror s p)
					| None -> alloc_var s t
				in
				unify ctx t v.v_type p;
				if PMap.mem s tctx.pc_locals then verror s p;
				tctx.pc_locals <- PMap.add s v tctx.pc_locals;
				{
					pdef = PatVar(SVar v,p);
					ptype = t;
					ppos = p;
				})
		| ((EObjectDecl fl),p) ->
			(match follow t with
			| TAnon {a_fields = fields}
			| TInst({cl_fields = fields},_) ->
				List.iter (fun (n,(_,p)) -> if not (PMap.mem n fields) then error (unify_error_msg (print_context()) (has_extra_field t n)) p) fl;
				let fl,pl,i = PMap.foldi (fun n cf (sl,pl,i) ->
					let pat = try loop tctx (List.assoc n fl) cf.cf_type with Not_found -> (mk_any cf.cf_type p) in
					(n,cf) :: sl,pat :: pl,i + 1
				) fields ([],[],0) in
				mk_con_pat (CAnon (i,fl)) pl t p;
			| t ->
				error ("Invalid pattern, expected something matching " ^ (s_type (print_context()) t)) p)
		| (ECast(e1,Some t2),p) ->
			let t2 = Typeload.load_complex_type ctx p t2 in
			unify ctx t t2 p;
			loop tctx e1 t2
		| (ECast(e1,None),p) ->
			loop tctx e1 t_dynamic
		| (EArrayDecl [],p) ->
			mk_con_pat (CArray 0) [] t p
		| (EArrayDecl el,p) ->
			(match t with
			| TInst({cl_path=[],"Array"},[t2]) ->
				let pl = List.map (fun e -> loop tctx e t2) el in
				mk_con_pat (CArray (List.length el)) pl t p
			| _ ->
				error ((s_type (print_context()) t) ^ " should be Array") p)
		| (EBinop(OpOr,(EBinop(OpOr,e1,e2),p2),e3),p1) ->
			loop tctx (EBinop(OpOr,e1,(EBinop(OpOr,e2,e3),p2)),p1) t
		| (EBinop(OpAssign,(EConst(Ident s),_),e1),p) ->
			let v = match tctx.pc_sub_vars with
				| Some vmap -> (try PMap.find s vmap with Not_found -> verror s p)
				| None -> alloc_var s t
			in
			unify ctx t v.v_type p;
			if PMap.mem s tctx.pc_locals then verror s p;
			tctx.pc_locals <- PMap.add s v tctx.pc_locals;
			let pat1 = loop tctx e1 t in
			{
				pdef = PatBind(v,pat1);
				ptype = t;
				ppos = p;
			};
		| (EBinop(OpOr,e1,e2),p) ->
			let old = tctx.pc_locals in
			let pat1 = loop tctx e1 t in
			let tctx2 = {
				pc_sub_vars = Some tctx.pc_locals;
				pc_locals = old;
			} in
			let pat2 = loop tctx2 e2 t in
			PMap.iter (fun s _ -> if not (PMap.mem s tctx2.pc_locals) then verror s p) tctx.pc_locals;
			unify ctx pat1.ptype pat2.ptype pat1.ppos;
			{
				pdef = PatOr(pat1,pat2);
				ptype = pat2.ptype;
				ppos = punion pat1.ppos pat2.ppos;
			}
		| (_,p) ->
			ctx.com.warning "Unrecognized pattern, falling back to normal switch" p;
			raise Exit
	in
	let tctx = {
		pc_locals = PMap.empty;
		pc_sub_vars = None;
	} in
	let e = loop tctx e t in
	PMap.iter (fun n v -> ctx.locals <- PMap.add n v ctx.locals) tctx.pc_locals;
	e

(* Turns a list of expressions into OpOr binops *)
let rec collapse_case el = match el with
	| e :: [] ->
		e
	| e :: el ->
		let e2 = collapse_case el in
		EBinop(OpOr,e,e2),punion (pos e) (pos e2)
	| [] ->
		assert false

(* Turns a list of patterns into Or patterns *)
let rec collapse_pattern pl = match pl with
	| pat :: [] ->
		pat
	| pat :: pl ->
		let pat2 = collapse_pattern pl in
		{
			pdef = PatOr(pat,pat2);
			ppos = punion pat.ppos pat2.ppos;
			ptype = pat.ptype
		}
	| [] ->
		assert false	

(* Calculates the specialization matrix of pmat for constructor c *)
let spec mctx (c : con) (pmat : pattern_matrix) : pattern_matrix =
	let a = arity c in
	let rec loop acc pl out = match pl with
		| ({pdef=PatCon(c2,cpl)}) :: pl when con_eq c c2 ->
			(cpl @ pl,out) :: acc
		| ({pdef=PatCon(_,_)}) :: pl ->
			acc
		| ({pdef=PatAny} as pat) :: pl ->
			((ExtList.List.make a pat) @ pl,out) :: acc
		| ({pdef=PatVar v} as pat) :: pl ->
			((ExtList.List.init a (fun i -> {pat with pdef = PatVar(SSub(v,i),pat.ppos)})) @ pl,out) :: acc
		| ({pdef=PatOr(pat1,pat2)}) :: pl ->
			let out2 = clone_outcome mctx out pat2 in
			let acc1 = loop acc (pat1 :: pl) out in
			loop acc1 (pat2 :: pl) out2
		| ({pdef=PatBind(_,pat)}) :: pl ->
			loop acc (pat :: pl) out
		| [] ->
			assert false
	in
	List.rev (List.fold_left (fun acc (pl,out) -> loop acc pl out) [] pmat)

(* Calculates the default matrix of pmat *)
let default mctx (pmat : pattern_matrix) : pattern_matrix =
	let rec loop acc pl out = match pl with
		| ({pdef=PatCon _}) :: pl ->
			acc
		| ({pdef=PatVar _ | PatAny}) :: pl ->
			(pl,out) :: acc
		| ({pdef=PatOr(pat1,pat2)}) :: pl ->
			let out2 = clone_outcome mctx out pat2 in
			let acc1 = loop acc (pat1 :: pl) out in
			loop acc1 (pat2 :: pl) out2;
		| ({pdef=PatBind(_,pat)}) :: pl ->
			loop acc (pat :: pl) out
		| [] ->
			assert false
	in
	List.rev (List.fold_left (fun acc (pl,out) -> loop acc pl out) [] pmat)

(* Picks a good column *)
(* TODO: check if we can use better heuristics *)
let pick_column (pmat : pattern_matrix) =
	let rec loop i row = match row with
		| ({pdef = PatVar _ | PatAny}) :: rl ->
			loop (i + 1) rl
		| [] ->
			0
		| _ ->
			i
	in
	loop 0 (fst (List.hd pmat))

(* Determines the sigma of a column, i.e. the list of found constructors *)
let rec column_sigma mctx (st : subterm) (pmat : pattern_matrix) : ((con * bool) list * t) =
	let t = mk_mono () in
	let guarded = Hashtbl.create 0 in
	let rec loop acc pmat =
		let rec loop2 acc row =
			match row with
			| (({pdef=PatCon(c,_)} as pat) :: _),out ->
				unify mctx.ctx pat.ptype t pat.ppos;
				let g = out.o_guard <> None in
				begin try
					let g2 = Hashtbl.find guarded (fst c) in
					if g2 && not g then Hashtbl.replace guarded (fst c) false
				with Not_found ->
					Hashtbl.add guarded (fst c) g;
				end;
				if List.exists (fun c2 -> con_eq c2 c) acc then acc else c :: acc
			| ({pdef=PatOr(pat1,pat2)} :: _),out ->
				let acc1 = loop acc [[pat1],out] in
				loop acc1 [[pat2],out]
			| ({pdef=PatVar(SVar v,_)} :: _),out ->
				bind_subterm out v st;
				acc
			| (({pdef=PatBind(v,pat)}) :: pl,out) ->
				bind_subterm out v st;
				loop2 acc ((pat :: pl),out)
			| _ ->
				acc
		in
		List.fold_left (fun acc row -> loop2 acc row) acc pmat
	in
	let sigma = loop [] pmat in
	List.map (fun c -> c,Hashtbl.find guarded (fst c)) sigma,t

(* Binds remaining subterms to free variables *)
let bind_remaining (out : outcome) (stl : subterm list) (row : pattern list) =
	let rec loop st pat = match st,pat with
		| st :: stl,{pdef = PatAny} :: pl ->
			loop stl pl
		| st :: stl,{pdef = PatVar(SVar v,_)} :: pl ->
			bind_subterm out v st;
			loop stl pl
		| st :: stl,pat :: pl ->
			loop ([st] @ stl) pl
		| st :: stl,[] ->
			()
		| [],_ ->
			()
	in
	loop stl row

(* Returns an exhaustive list of all constructors for a given type *)
(* TODO: cache this? *)
let all_ctors t =
	let h = ref PMap.empty in
	let inf = match follow t with
	| TAbstract({a_path = [],"Bool"},_) ->
		h := PMap.add (CConst(TBool true)) Ast.null_pos !h;
		h := PMap.add (CConst(TBool false)) Ast.null_pos !h;
		false	
	| TInst({cl_path=[],"String"},_)
	| TInst({cl_path=[],"Array"},_)
	| TAbstract _ ->
		true
	| TEnum(en,pl) ->
		PMap.iter (fun _ ef ->
			try unify_enum_field en pl ef t;
				h := PMap.add (CEnum(en,ef)) ef.ef_pos !h
			with Unify_error _ ->
				()
		) en.e_constrs;
		false
	| TAnon {a_fields = fields}
	| TInst({cl_fields = fields},_) ->
		false
	| _ ->
		true
	in
	h,inf

(* Generates the decision tree for a given pattern matrix *)
let rec compile mctx (stl : subterm list) (n : int) (pmat : pattern_matrix) = match pmat with
	| [] ->
		assert false
	| (row,out) :: rl when List.for_all (fun pat -> match pat.pdef with PatVar _ -> true | _ -> false) row ->
		(* The first row has only variables or wildcards (or nothing at all). *)
		bind_remaining out stl row;
		out.o_paths <- out.o_paths + 1;
		if out.o_guard = None || match rl with [] -> true | _ -> false then
			(* Not guarded, yield outcome *)
			Bind(out,None)
		else
			(* Guarded, yield outcome and continue *)
			Bind(out,Some (compile mctx stl 0 rl))
	| (row,out) :: _ ->
		let i = pick_column pmat in
		if i > 0 then begin
			(* Some column is better than the first, swap them and loop *)
			let pat_swap = List.map (fun (row,out) -> (swap_columns i row),out) pmat in
			let stl_swap = swap_columns i stl in
			compile mctx stl_swap i pat_swap
		end else begin
			(* Get column sigma and derive cases *)
			let st_head,st_tail = match stl with st :: stl -> st,stl | _ -> assert false in
			let sigma,t = column_sigma mctx st_head pmat in
			let c_all,inf = all_ctors t in
			let cases = List.rev_map (fun (c,g) ->
				let a = arity c in
				if not g then c_all := PMap.remove (fst c) !c_all;
				let pmat_spec = spec mctx c pmat in
				let stl_sub = ExtList.List.init a (fun i -> SSub(st_head,i),pos c) in
				try
					let dt = compile mctx (stl_sub @ st_tail) 0 pmat_spec in
					c,dt
				with Not_exhaustive (pat,i) ->
					let a2 = a - i - 1 in
					let args = (ExtList.List.make i any) @ [pat] @ (if a2 > 0 then (ExtList.List.make (a - i - 1) any) else []) in
					let pattern = mk_con_pat (fst c) args t_dynamic (pos c) in
					raise (Not_exhaustive(pattern,i))				
			) sigma in
			if not inf && PMap.is_empty !c_all then Switch (st_head,t,cases) else begin
				let pmat_def = default mctx pmat in
				match pmat_def,cases with
				| [],_ when inf && mctx.value_only ->
					(* toplevel infinite: assume value switch and don't report non-exhaustiveness to retain old behavior *)
					Switch (st_head,t,cases)
				| [],_ ->
					(* non-exhaustive *)
					let cl = PMap.foldi (fun c p acc -> (c,p) :: acc) !c_all [] in
					(match cl with
					| [] ->
						raise (Not_exhaustive(any,0))
					| _ ->
						let pl = List.map (fun c -> (mk_con_pat (fst c) (ExtList.List.make (arity c) any) t_dynamic (pos c))) cl in
						raise (Not_exhaustive (collapse_pattern pl,n)))
				| _,[] ->
					(* there is only the default case, so we don't have to switch on it *)
					compile mctx st_tail 0 pmat_def
				| _ ->
					(* normal switch case *)
					let dt = compile mctx st_tail 0 pmat_def in
					Switch (st_head,t,cases @ [(CConst TNull, pos st_head),dt])
			end
		end

(* Conversion to current typed AST *)

let subterm_to_varname st =
	String.concat "_s" (ExtString.String.nsplit (s_subterm st) ".")

let mk_const ctx p = function
	| TString s -> mk (TConst (TString s)) ctx.com.basic.tstring p
	| TInt i -> mk (TConst (TInt i)) ctx.com.basic.tint p
	| TFloat f -> mk (TConst (TFloat f)) ctx.com.basic.tfloat p
	| TBool b -> mk (TConst (TBool b)) ctx.com.basic.tbool p
	| TNull -> mk (TConst TNull) (ctx.com.basic.tnull (mk_mono())) p
	| _ -> error "Unsupported constant" p

let switch_infos ctx st =
	let v = PMap.find (subterm_to_varname st) ctx.locals in
	let p = pos st in
	let e_v = mk (TLocal v) v.v_type p in
	v,e_v,p

(* Translates constants to a TSwitch *)
let rec to_value_switch ctx need_val st t cases =
	let v,e_var,p = switch_infos ctx st in
	let def = ref None in
	let cases = ExtList.List.filter_map (fun ((c,p),dt) ->
		match c with
		| CConst TNull ->
			def := Some (to_typed_ast ctx need_val dt);
			None
		| CConst c ->
			Some ([mk_const ctx p c],to_typed_ast ctx need_val dt)
		| CType mt ->
			Some ([Typer.type_module_type ctx mt None p],to_typed_ast ctx need_val dt)
		| c ->
			error ("Unexpected "  ^ (s_con c)) p
	) cases in
	let el = (List.map (fun (_,e) -> e) cases) @ match !def with None -> [] | Some e -> [e] in
	let t = if not need_val then (mk_mono()) else unify_min ctx (List.rev el) in
	mk (TSwitch(e_var,cases,!def)) t p

(* Translates enum constructors to a TMatch *)
and to_enum_switch ctx need_val st en pl cases =
	let v,e_var,p = switch_infos ctx st in
	let def = ref None in
	let cases = ExtList.List.filter_map (fun ((c,p),dt) ->
		match c with
		| CEnum(en,ef) ->
			let save = save_locals ctx in
			let vl = match follow ef.ef_type with
			| TFun(args,_) ->
				let vl = ExtList.List.mapi (fun i (_,_,t) ->
					let n = subterm_to_varname (SSub(st,i),p) in
					let v = add_local ctx n t in
					Some v
				) args in
				Some vl
			| _ -> None in
			let e = to_typed_ast ctx need_val dt in
			save ();
			Some ([ef.ef_index],vl,e)
		| CConst TNull ->
			def := Some (to_typed_ast ctx need_val dt);
			None			
		| c ->
			error ("Unexpected "  ^ (s_con c)) p
	) cases in
	let el = (List.map (fun (_,_,e) -> e) cases) @ match !def with None -> [] | Some e -> [e] in
	let t = if not need_val then (mk_mono()) else unify_min ctx (List.rev el) in
	mk (TMatch(e_var,(en,pl),cases,!def)) t p

(* Binds fields to subterm vars, then generates inner tree *)
(* TODO: this wrapping could be removed if subterms supported field names *)
and to_anon_switch ctx need_val st fields cases =
	let v,e_var,p = switch_infos ctx st in
	match cases with
		| ((CAnon (_,an),p),dt) :: _ ->
			let save = save_locals ctx in
			let vl = ExtList.List.mapi (fun i (s,cf) ->
				let n = subterm_to_varname (SSub(st,i),p) in
				let cf = PMap.find s fields in
				let v2 = add_local ctx n cf.cf_type in
				v2,Some (mk (TField(e_var,s)) v2.v_type p)
			) an in
			let edt = to_typed_ast ctx need_val dt in
			let e = mk (TBlock [
				mk (TVars vl) t_dynamic p;
				edt;
			]) edt.etype p in
			save();
			e
		| _ ->
			assert false

(* Switches over the length of the input array *)
and to_array_switch ctx need_val st t cases =
	let v,e_var,p = switch_infos ctx st in
	let def = ref None in
	let cases = ExtList.List.filter_map (fun ((c,p),dt) -> match c with
		| CArray i ->
			let save = save_locals ctx in
			let vl = ExtList.List.init i (fun i ->
				let n = subterm_to_varname (SSub(st,i),p) in
				let v = add_local ctx n t in
				v, Some (mk (TArray(e_var,mk_const ctx p (TInt (Int32.of_int i)))) v.v_type p)
			) in
			let e = to_typed_ast ctx need_val dt in
			let e = mk (TBlock [
				mk (TVars vl) t_dynamic p;
				e;
			]) e.etype e.epos in
			save();
			Some ([mk_const ctx p (TInt (Int32.of_int i))],e)
		| CConst TNull ->
			def := Some (to_typed_ast ctx need_val dt);
			None
		| c ->
			error ("Unexpected "  ^ (s_con c)) p			
	) cases in
	let el = (List.map (fun (_,e) -> e) cases) @ match !def with None -> [] | Some e -> [e] in
	let t = if not need_val then (mk_mono()) else unify_min ctx (List.rev el) in	
	let e_eval = mk (TField(e_var,"length")) ctx.com.basic.tint p in
	mk (TSwitch(e_eval,cases,!def)) t p

and to_typed_ast ctx need_val (dt : decision_tree) : texpr =
	match dt with
	| Bind (out,dt) ->
		let p = out.o_expr.epos in
		let vl = List.map (fun (v,st) ->
			let vt = PMap.find (subterm_to_varname st) ctx.locals in
			v, Some (mk (TLocal (vt)) vt.v_type p)
		) out.o_bindings in	
		let e = match out.o_guard,dt with
			| Some econd,Some dt ->
				let eif = out.o_expr in
				let eelse = to_typed_ast ctx need_val dt in
				mk (TIf(econd,eif,Some eelse)) eif.etype (punion econd.epos eelse.epos)
			| None,None
			| Some _,None ->
				out.o_expr;
			| None, Some _ ->
				assert false
		in
		mk (TBlock [
			mk (TVars vl) t_dynamic p;
			e;
		]) e.etype p
	| Switch(st,t,cases) ->
		match follow t with
		| TEnum(en,pl) ->
			to_enum_switch ctx need_val st en pl cases
		| TInst({cl_path=[],"Array"},[t]) ->
			to_array_switch ctx need_val st t cases;
		| (TInst({cl_path=[],"String"},_) as t)
		| (TAbstract _ as t) ->
			to_value_switch ctx need_val st t cases			
		| TAnon {a_fields = fields}
		| TInst({cl_fields = fields},_) ->
			to_anon_switch ctx need_val st fields cases
		| t ->
			to_value_switch ctx need_val st t cases

(* Main match function *)
let match_expr ctx e cases def need_val with_type p =
	if ctx.untyped then raise Exit;
	let cases = match cases,def with
		| [],None -> error "Empty switch" p
		| cases,Some def -> cases @ [[(EConst(Ident "_")),pos def],def]
		| _ -> cases
	in
	let evals = match fst e with
		| EArrayDecl el ->
			List.map (fun e -> type_expr ctx e true) el
		| _ ->
			[type_expr_with_type ctx e with_type need_val]
	in
	let mctx = {
		ctx = ctx;
		outcomes = PMap.empty;
		num_outcomes = 0;
		value_only = match evals with
			| [e] -> (match follow e.etype with
				| TEnum(en,_) when PMap.is_empty en.e_constrs ->
					raise Exit
				| TDynamic _
				| TMono _ ->
					true
				| TAbstract({a_path=[],"Bool"},_) ->
					false
				| TInst({cl_path=[],"String"},_)
				| TAbstract _ ->
					true
				| _ ->
					false)
			| _ ->
				false
	} in
	let v_evals = List.map (fun e -> gen_local ctx e.etype) evals in
	(* 1. turn case expressions to patterns *)
	let patterns = List.map (fun (el,e) ->
		let epat = collapse_case el in
		let epat,guard = match fst epat with
			| EIn(e1,e2) -> e1, Some e2
			| _ -> epat,None
		in
		let save = save_locals ctx in
		let pat = match fst epat,evals with
			| EArrayDecl el,[eval] when (match follow eval.etype with TInst({cl_path=[],"Array"},[_]) -> true | _ -> false) ->
				[to_pattern ctx epat eval.etype]
			| EArrayDecl el,evals ->
				(try List.map2 (fun e eval -> to_pattern ctx e eval.etype) el evals
				with Invalid_argument _ -> error ("Invalid number of arguments: expected " ^ (string_of_int (List.length evals)) ^ ", found " ^ (string_of_int (List.length el))) (pos epat))
			| EConst(Ident "_"),evals -> List.map (fun eval -> mk_any eval.etype (pos epat)) evals
			| _,_ :: _ :: [] -> error "This kind of binding is not allowed because we do not have tuples" (pos epat);
			| _,_ -> [to_pattern ctx epat (List.hd evals).etype]
		in		
		let e = type_expr ctx e need_val in
		let guard = match guard with
			| None -> None
			| Some e ->
				let e = type_expr ctx e need_val in
				unify ctx e.etype ctx.com.basic.tbool e.epos;
				Some e
		in
		save();
		let out = mk_outcome mctx e guard pat in
		(pat,out)
	) cases in
	if Common.defined ctx.com Common.Define.MatchDebug then print_endline (s_pattern_matrix patterns);
	(* 2. compile patterns to decision tree *)
 	let dt = try
 		compile mctx (List.map2 (fun e v -> SVar v,e.epos) evals v_evals) 0 patterns
 	with Not_exhaustive (pat,_) ->
 		error ("This match is not exhaustive, these patterns are not matched: " ^ (s_pattern pat)) p
 	in
 	if Common.defined ctx.com Common.Define.MatchDebug then print_endline (s_decision_tree "" dt);
 	if not mctx.value_only then PMap.iter (fun pat out -> if out.o_paths = 0 then ctx.com.warning "This pattern is unused" out.o_pos) mctx.outcomes;
	(* 3. transform decision tree to current AST *)
	(* TODO: we could instead add a new tAST node holding the decision tree and optimize in the generators *)
	let t = if not need_val then
		mk_mono()
	else
		try Typer.unify_min_raise ctx (List.map (fun (_,out) -> out.o_expr) patterns) with Error (Unify l,p) -> error (error_msg (Unify l)) p
	in
	let edt = to_typed_ast ctx need_val dt in
	mk (TBlock [
		mk (TVars(List.map2 (fun e v -> v,Some e) evals v_evals)) t_dynamic p;
		edt;
	]) t p
;;
match_expr_ref := match_expr