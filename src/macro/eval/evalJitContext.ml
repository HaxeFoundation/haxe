open Type
open EvalContext

type varacc =
	| Local of int
	| Env of int

(*
	JitContext keeps track of allocated local variables and closures. Scopes can be pushed
	and popped and influence the "slots" of variables. It also manages the maximum number
	of locals allocated at the same time, which the run-time can use to allocate a fixed
	array.
*)

type t = {
	ctx : context;
	(* The scope stack. *)
	mutable scopes : scope list;
	(* The captured variables declared in this context. Maps variable IDs to capture slots. *)
	mutable captures : (int,int * bool) Hashtbl.t;
	(* The current number of locals. *)
	mutable num_locals : int;
	(* The maximum number of locals. *)
	mutable max_num_locals : int;
	(* The number of closures in this context.*)
	mutable num_closures : int;
	(* Whether or not this function has a return that's not at the end of control flow. *)
	mutable has_nonfinal_return : bool;
	(* The name of capture variables. Maps local slots to variable names. Only filled in debug mode. *)
	mutable capture_infos : (int,var_info) Hashtbl.t;
	(* Variables which are accessed but not declared in this scope. *)
	mutable captures_outside_scope : tvar list;
}

let var_info_of_var var =
	{
		vi_name = var.v_name;
		vi_pos = var.v_pos;
		vi_generated = (match var.v_kind with VUser _ -> false | _ -> true)
	}

(* Creates a new context *)
let create ctx = {
	ctx = ctx;
	scopes = [];
	captures = Hashtbl.create 0;
	num_locals = 0;
	max_num_locals = 0;
	num_closures = 0;
	has_nonfinal_return = false;
	capture_infos = Hashtbl.create 0;
	captures_outside_scope = []
}

(* Returns the number of locals in [scope]. *)
let num_locals scope =
	Hashtbl.length scope.locals

(* Pushes a new scope onto context [jit]. *)
let push_scope jit pos =
	let scope = {
		local_offset = jit.num_locals;
		locals = Hashtbl.create 0;
		local_infos = Hashtbl.create 0;
		local_ids = Hashtbl.create 0;
		pos = pos;
	} in
	jit.scopes <- scope :: jit.scopes

(* Pops the top scope from context [jit] .*)
let pop_scope jit = match jit.scopes with
	| scope :: tl ->
		jit.scopes <- tl;
		jit.num_locals <- jit.num_locals - (num_locals scope);
	| [] ->
		Globals.die "" __LOC__

(* Increases number of locals and updates maximum number of locals if necessary *)
let increase_num_locals jit =
	jit.num_locals <- jit.num_locals + 1;
	if jit.num_locals > jit.max_num_locals then jit.max_num_locals <- jit.num_locals

(* Adds capture variable [var] to context [jit]. *)
let add_capture jit var declared =
	let i = Hashtbl.length jit.captures in
	Hashtbl.add jit.captures var.v_id (i,declared);
	if jit.ctx.debug.support_debugger then begin
		Hashtbl.replace jit.capture_infos i (var_info_of_var var)
	end;
	i

(* Adds variable [var] to the current top scope of context [jit]. *)
let add_local jit var = match jit.scopes with
	| [] -> Globals.die "" __LOC__
	| scope :: _ ->
		let i = Hashtbl.length scope.locals in
		Hashtbl.add scope.locals var.v_id i;
		increase_num_locals jit;
		let slot = scope.local_offset + i in
		if jit.ctx.debug.support_debugger then begin
			if Typecore.is_gen_local var then var.v_name <- "_g" ^ String.sub var.v_name 1 (String.length var.v_name - 1);
			Hashtbl.replace scope.local_ids var.v_name var.v_id;
			Hashtbl.replace scope.local_infos i (var_info_of_var var)
		end;
		slot

(*
	Declares variable [var] in context [jit]. If the variable is captured, it is added to
	the capture hash table. Otherwise it is added to the local hash table of the top scope.

	Returns either [Env slot] if the variable is captured or [Local slot] otherwise.
*)
let declare_local jit var =
	if var.v_capture then Env (add_capture jit var true)
	else Local (add_local jit var)

(*
	Declares function argument [var] in context [jit].

	All function arguments are added as local variables. If the variable is captured, it
	is also added as a capture variable. This is handled by [EvalEmitter.handle_capture_arguments].
*)
let declare_arg jit var =
	let varacc = add_local jit var in
	if var.v_capture then add_capture jit var true,Some varacc else varacc,None

(* Declares a variable for `this` in context [jit]. *)
let declare_local_this jit = match jit.scopes with
	| [] -> Globals.die "" __LOC__
	| scope :: _ ->
		let i = Hashtbl.length scope.locals in
		Hashtbl.add scope.locals 0 i;
		increase_num_locals jit;
		if jit.ctx.debug.support_debugger then begin
			Hashtbl.replace scope.local_ids "this" 0;
			Hashtbl.replace scope.local_infos 0 { vi_name = "this"; vi_pos = Globals.null_pos; vi_generated = false }
		end;
		Local i

(* Gets the slot of variable id [vid] in context [jit]. *)
let get_slot_raise jit vid =
	let rec loop scopes = match scopes with
		| [] -> raise Not_found
		| scope :: scopes ->
			try
				scope.local_offset + Hashtbl.find scope.locals vid
			with Not_found ->
				loop scopes
	in
	loop jit.scopes

let get_slot jit vid p =
	try get_slot_raise jit vid
	with Not_found -> EvalMisc.throw_string "Unbound variable" p

(* Gets the slot of captured variable id [vid] in context [jit]. *)
let get_capture_slot jit var =
	try fst (Hashtbl.find jit.captures var.v_id)
	with Not_found ->
		jit.captures_outside_scope <- var :: jit.captures_outside_scope;
		add_capture jit var false