open Type
open EvalContext
open EvalEmitter

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
	mutable captures : (int,int) Hashtbl.t;
	(* The current number of locals. *)
	mutable local_count : int;
	(* The maximum number of locals. *)
	mutable max_local_count : int;
	(* The number of closures in this context.*)
	mutable num_closures : int;
	(* Whether or not this function has a return that's not at the end of control flow. *)
	mutable has_nonfinal_return : bool;
	(* The name of capture variables. Maps local slots to variable names. Only filled in debug mode. *)
	mutable capture_infos : (int,var_info) Hashtbl.t;
}

(* Creates a new context *)
let create ctx = {
	ctx = ctx;
	scopes = [];
	captures = Hashtbl.create 0;
	local_count = 0;
	max_local_count = 0;
	num_closures = 0;
	has_nonfinal_return = false;
	capture_infos = Hashtbl.create 0;
}

(* Returns the number of locals in [scope]. *)
let num_locals scope =
	Hashtbl.length scope.locals

(* Pushes a new scope onto context [jit]. *)
let push_scope jit pos =
	let scope = {
		local_offset = jit.local_count;
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
		jit.local_count <- jit.local_count - (num_locals scope);
	| [] ->
		assert false

(* Increases number of locals and updates maximum number of locals if necessary *)
let increase_local_count jit =
	jit.local_count <- jit.local_count + 1;
	if jit.local_count > jit.max_local_count then jit.max_local_count <- jit.local_count

(*
	Declares variable [var] in context [jit]. If the variable is captured, it is added to
	the capture hash table. Otherwise it is added to the local hash table of the top scope.

	Returns either [Env slot] if the variable is captured or [Local slot] otherwise.
*)
let declare_local jit var =
	if var.v_capture then begin
		let i = Hashtbl.length jit.captures in
		Hashtbl.add jit.captures var.v_id i;
		if jit.ctx.debug.support_debugger then begin
			Hashtbl.replace jit.capture_infos i var.v_name
		end;
		Env i
	end else match jit.scopes with
	| [] -> assert false
	| scope :: _ ->
		let i = Hashtbl.length scope.locals in
		Hashtbl.add scope.locals var.v_id i;
		increase_local_count jit;
		let slot = scope.local_offset + i in
		if jit.ctx.debug.support_debugger then begin
			Hashtbl.replace scope.local_ids var.v_name var.v_id;
			Hashtbl.replace scope.local_infos i var.v_name
		end;
		Local slot

(* Declares a variable for `this` in context [jit]. *)
let declare_local_this jit = match jit.scopes with
	| [] -> assert false
	| scope :: _ ->
		let i = Hashtbl.length scope.locals in
		Hashtbl.add scope.locals 0 i;
		increase_local_count jit;
		if jit.ctx.debug.support_debugger then begin
			Hashtbl.replace scope.local_ids "this" 0;
			Hashtbl.replace scope.local_infos 0 "this"
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
	with Not_found -> throw_string "Unbound variable" p

(* Gets the slot of captured variable id [vid] in context [jit]. *)
let get_capture_slot jit vid =
	Hashtbl.find jit.captures vid