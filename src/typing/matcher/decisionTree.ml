open Globals
open Type
open Case
open Bind
open MatcherGlobals

type type_finiteness =
	| Infinite          (* type has inifite constructors (e.g. Int, String) *)
	| CompileTimeFinite (* type is considered finite only at compile-time but has inifite possible run-time values (enum abstracts) *)
	| RunTimeFinite     (* type is truly finite (Bool, enums) *)

type t =
	| Leaf of Case.t
	| Switch of texpr * switch_case list * dt
	| Bind of bind list * dt
	| Guard of texpr * dt * dt
	| GuardNull of texpr * dt * dt
	| Fail

and dt = {
	dt_t : t;
	dt_i : int;
	dt_pos : pos;
	mutable dt_goto_target : bool;
	mutable dt_texpr : texpr option;
}

and switch_case = {
	sc_con : Constructor.t;
	sc_unguarded : bool;
	sc_dt : dt;
}

let make_bind_gen v p e s = {
	b_var = v;
	b_pos = p;
	b_expr = e;
	b_status = s;
}

let make_bind v p e = make_bind_gen v p e BindUsed

let tab_string = "    "

let to_string dt =
	let buf = Buffer.create 0 in
	let indices = Stack.create () in
	let push_index i = Stack.push i indices in
	let add_line tabs s =
		if Buffer.length buf > 0 then Buffer.add_char buf '\n';
		if not (Stack.is_empty indices) then begin
			Buffer.add_string buf (Printf.sprintf "%2i" (Stack.pop indices));
			Buffer.add_substring buf tabs 0 (String.length tabs - 2);
		end else
			Buffer.add_string buf tabs;
		Buffer.add_string buf s
	in
	let add s =
		Buffer.add_string buf s
	in
	let s_expr tabs e =
		Type.s_expr_pretty true tabs false s_type e
	in
	let print_expr_noblock tabs e = match e.eexpr with
		| TBlock el ->
			List.iter (fun e ->
				add_line tabs (s_expr tabs e) ;
			) el
		| _ ->
			add_line tabs (s_expr tabs e)
	in
	let print_case_expr tabs case = match case.case_expr with
		| None ->
			()
		| Some e ->
			print_expr_noblock tabs e
	in
	let rec loop tabs dt =
		push_index dt.dt_i;
		match dt.dt_t with
		| Leaf case ->
			print_case_expr tabs case
		| Switch(e,cases,dt) ->
			add_line tabs (Printf.sprintf "switch (%s)" (s_expr tabs e));
			List.iter (fun sc ->
				add_line (tabs ^ tab_string) "case ";
				add (Constructor.to_string sc.sc_con);
				add (if sc.sc_unguarded then "(unguarded)" else "guarded");
				add ":";
				loop (tabs ^ tab_string ^ tab_string) sc.sc_dt;
			) cases;
			add_line (tabs ^ tab_string) "default";
			loop (tabs ^ tab_string ^ tab_string) dt;
		| Bind(bl,dt) ->
			List.iter (fun bind ->
				add_line tabs "var ";
				add bind.b_var.v_name;
				add "<";
				add (string_of_int bind.b_var.v_id);
				add ">";
				add " = ";
				add (s_expr tabs bind.b_expr);
				begin match bind.b_status with
					| BindUnused ->
						add " // unused"
					| BindDeferred ->
						add " // deferred"
					| BindUsed ->
						()
				end
			) bl;
			loop tabs dt
		| Guard(e,dt1,dt2) ->
			print_guard tabs e dt1 dt2 false
		| GuardNull(e,dt1,dt2) ->
			print_guard tabs e dt1 dt2 true
		| Fail ->
			add_line tabs "<fail>";
	and print_guard tabs e dt1 dt2 is_null_guard =
		add_line tabs "if (";
		add (s_expr tabs e);
		if is_null_guard then add " == null";
		add ")";
		loop (tabs ^ tab_string) dt1;
		add_line tabs "else";
		loop (tabs ^ tab_string) dt2;
	in
	loop tab_string dt;
	Buffer.contents buf

let equal_dt dt1 dt2 = dt1.dt_i = dt2.dt_i

let equal_bind_expr e1 e2 =
	Texpr.equal e1 e2

let equal dt1 dt2 = match dt1,dt2 with
	| Leaf case1,Leaf case2 ->
		case1 == case2
	| Switch(subject1,cases1,dt1),Switch(subject2,cases2,dt2) ->
		Texpr.equal subject1 subject2 &&
		Ast.safe_for_all2 (fun sc1 sc2 -> Constructor.equal sc1.sc_con sc2.sc_con && sc1.sc_unguarded = sc2.sc_unguarded && equal_dt sc1.sc_dt sc2.sc_dt) cases1 cases2 &&
		equal_dt dt1 dt2
	| Bind(l1,dt1),Bind(l2,dt2) ->
		Ast.safe_for_all2 (fun bind1 bind2 -> bind1.b_var == bind2.b_var && equal_bind_expr bind1.b_expr bind2.b_expr) l1 l2 &&
		equal_dt dt1 dt2
	| Fail,Fail ->
		true
	| (Guard(e1,dt11,dt12),Guard(e2,dt21,dt22)) | (GuardNull(e1,dt11,dt12),GuardNull(e2,dt21,dt22)) ->
		e1 == e2 && equal_dt dt11 dt21 && equal_dt dt12 dt22
	| _ ->
		false

let hash = Hashtbl.hash