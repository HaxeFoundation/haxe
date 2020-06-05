open Globals
open Type
open Typecore
open Common
open Ast

type rename_init = {
	mutable ri_scope : var_scope;
	mutable ri_hoisting : bool;
	mutable ri_no_shadowing : bool;
	mutable ri_switch_cases_no_blocks : bool;
	mutable ri_reserved : bool StringMap.t;
	mutable ri_reserve_current_top_level_symbol : bool;
}

(**
	For initialization.
	Make the `name` a reserved word.
	No local variable will be allowed to have such name.
*)
let reserve_init ri name =
	ri.ri_reserved <- StringMap.add name true ri.ri_reserved

(**
	Make all module-level names reserved.
	No local variable will have a name matching a module-level declaration.
*)
let reserve_all_types ri com path_to_name =
	List.iter (fun mt ->
		let tinfos = t_infos mt in
		let native_name = try fst (TypeloadCheck.get_native_name tinfos.mt_meta) with Not_found -> path_to_name tinfos.mt_path in
		match mt with
		| TClassDecl c when native_name = "" ->
			List.iter (fun cf ->
				let native_name = try fst (TypeloadCheck.get_native_name cf.cf_meta) with Not_found -> cf.cf_name in
				reserve_init ri native_name
			) c.cl_ordered_statics
		| TClassDecl { cl_kind = KModuleFields m; cl_ordered_statics = fl } ->
			let prefix = Path.flat_path m.m_path ^ "_" in
			List.iter (fun cf ->
				let name = try fst (TypeloadCheck.get_native_name cf.cf_meta) with Not_found -> prefix ^ cf.cf_name in
				reserve_init ri name
			) fl
		| _ ->
			reserve_init ri native_name
	) com.types

(**
	Initialize the context for local variables renaming
*)
let init com =
	let ri = {
		ri_scope = com.config.pf_scoping.vs_scope;
		ri_reserved = StringMap.empty;
		ri_hoisting = false;
		ri_no_shadowing = false;
		ri_switch_cases_no_blocks = false;
		ri_reserve_current_top_level_symbol = false;
	} in
	reserve_init ri "this";
	List.iter (fun flag ->
		match flag with
		| VarHoisting ->
			ri.ri_hoisting <- true;
		| NoShadowing ->
			ri.ri_no_shadowing <- true;
		| SwitchCasesNoBlocks ->
			ri.ri_switch_cases_no_blocks <- true;
		| ReserveNames names ->
			List.iter (reserve_init ri) names
		| ReserveAllTopLevelSymbols ->
			reserve_all_types ri com (fun (pack,name) -> if pack = [] then name else List.hd pack)
		| ReserveAllTypesFlat ->
			reserve_all_types ri com Path.flat_path
		| ReserveCurrentTopLevelSymbol -> ri.ri_reserve_current_top_level_symbol <- true
	) com.config.pf_scoping.vs_flags;
	ri

type scope = {
	(** Parent scope *)
	parent : scope option;
	(** Child scopes *)
	mutable children : scope list;
	(**
		Pairs of "variable declared => the list of variables it overlaps with".
		That list contains variables, which were declared _before_ the current one, but
		used _after_ the current one was declared.
		Example:
		```
		var a = 123;
		var b = 324;
		trace(a);
		```
		in this example `b` overlaps with `a`.
	*)
	mutable own_vars : (tvar * (tvar IntMap.t ref)) list;
	(** Variables declared outside of this scope, but used inside of it *)
	mutable foreign_vars : tvar IntMap.t;
	(** List of variables used in current loop *)
	loop_vars : tvar IntMap.t ref;
	(** Current loops depth *)
	mutable loop_count : int;
}

type rename_context = {
	rc_hoisting : bool;
	rc_no_shadowing : bool;
	rc_switch_cases_no_blocks : bool;
	rc_scope : var_scope;
	mutable rc_reserved : bool StringMap.t;
}

(**
	Make `name` a reserved word.
	No local variable will be allowed to have such name.
*)
let reserve_ctx rc name =
	rc.rc_reserved <- StringMap.add name true rc.rc_reserved

(**
	Make `name` a reserved word.
	No local variable will be allowed to have such name.
*)
let reserve reserved name =
	reserved := StringMap.add name true !reserved

let create_scope parent =
	let scope = {
		parent = parent;
		children = [];
		own_vars = [];
		foreign_vars = IntMap.empty;
		loop_vars = ref IntMap.empty;
		loop_count = 0;
	} in
	Option.may (fun p -> p.children <- scope :: p.children) parent;
	scope

(**
	Invoked for each `TVar v` texpr_expr
*)
let declare_var rc scope v =
	let overlaps =
		if not rc.rc_hoisting || IntMap.is_empty scope.foreign_vars then
			if scope.loop_count = 0 then IntMap.empty
			else !(scope.loop_vars)
		else
			if scope.loop_count = 0 then
				scope.foreign_vars
			else begin
				let overlaps = ref !(scope.loop_vars) in
				IntMap.iter (fun i o ->
					if not (IntMap.mem i !overlaps) then
						overlaps := IntMap.add i o !overlaps
				) scope.foreign_vars;
				!overlaps
			end
	in
	scope.own_vars <- (v, ref overlaps) :: scope.own_vars;
	if scope.loop_count > 0 then
		scope.loop_vars := IntMap.add v.v_id v !(scope.loop_vars)

(**
	Invoked for each `TLocal v` texr_expr
*)
let rec use_var rc scope v =
	let rec loop declarations =
		match declarations with
		| [] ->
			if (rc.rc_no_shadowing || rc.rc_hoisting) && not (IntMap.mem v.v_id scope.foreign_vars) then
				scope.foreign_vars <- IntMap.add v.v_id v scope.foreign_vars;
			(match scope.parent with
			| Some parent -> use_var rc parent v
			| None -> raise (Failure "Failed to locate variable declaration")
			)
		| (d, _) :: _ when d == v -> ()
		| (d, overlaps) :: rest ->
			if not (IntMap.mem v.v_id !overlaps) then
				overlaps := IntMap.add v.v_id v !overlaps;
			loop rest
	in
	loop scope.own_vars;
	if scope.loop_count > 0 && not (IntMap.mem v.v_id !(scope.loop_vars)) then
		scope.loop_vars := IntMap.add v.v_id v !(scope.loop_vars)

let collect_loop scope fn =
	scope.loop_count <- scope.loop_count + 1;
	fn();
	scope.loop_count <- scope.loop_count - 1;
	if scope.loop_count < 0 then
		raise (Failure "Unexpected loop count");
	if scope.loop_count = 0 then
		scope.loop_vars := IntMap.empty

(**
	Collect all the variables declared and used in `e` expression.
*)
let rec collect_vars ?(in_block=false) rc scope e =
	let collect_vars =
		match e.eexpr with
		| TBlock _ | TFunction _ -> collect_vars ~in_block:true rc
		| _ -> collect_vars ~in_block:false rc
	in
	match e.eexpr with
	| TVar (v, e_opt) when rc.rc_hoisting || (match e_opt with Some { eexpr = TFunction _ } -> true | _ -> false) ->
		declare_var rc scope v;
		Option.may (collect_vars scope) e_opt
	| TVar (v, e_opt) ->
		Option.may (collect_vars scope) e_opt;
		declare_var rc scope v
	| TLocal v ->
		use_var rc scope v
	| TFunction fn ->
		let scope = create_scope (Some scope) in
		List.iter (fun (v,_) -> declare_var rc scope v) fn.tf_args;
		List.iter (fun (v,_) -> use_var rc scope v) fn.tf_args;
		(match fn.tf_expr.eexpr with
		| TBlock exprs -> List.iter (collect_vars scope) exprs
		| _ -> collect_vars scope fn.tf_expr
		)
	| TTry (try_expr, catches) ->
		collect_vars scope try_expr;
		List.iter (fun (v, catch_expr) ->
			let v_expr = mk (TVar (v,None)) t_dynamic v.v_pos in
			let e =
				match catch_expr.eexpr with
				| TBlock exprs -> { catch_expr with eexpr = TBlock (v_expr :: exprs) }
				| _ -> { catch_expr with eexpr = TBlock [v_expr; catch_expr] }
			in
			collect_vars scope e
		) catches
	| TSwitch (target, cases, default_opt) when rc.rc_switch_cases_no_blocks ->
		collect_vars scope target;
		List.iter (fun (el,e) ->
			List.iter (collect_vars scope) el;
			collect_ignore_block ~in_block:true rc scope e
		) cases;
		Option.may (collect_ignore_block ~in_block:true rc scope) default_opt
	| TBlock exprs when rc.rc_scope = BlockScope && not in_block ->
		let scope = create_scope (Some scope) in
		List.iter (collect_vars scope) exprs
	| TWhile (condition, body, flag) ->
		collect_loop scope (fun() ->
			if flag = NormalWhile then
				collect_vars scope condition;
			collect_vars scope body;
			if flag = DoWhile then
				collect_vars scope condition;
		)
	(*
		This only happens for `cross` target, because for real targets all loops are converted to `while` at this point
		Idk if this works correctly.
	*)
	| TFor (v, iterator, body) ->
		collect_loop scope (fun() ->
			if rc.rc_hoisting then
				declare_var rc scope v;
			collect_vars scope iterator;
			if not rc.rc_hoisting then
				declare_var rc scope v;
			collect_vars scope body
		)
	| _ ->
		iter (collect_vars scope) e

and collect_ignore_block ?(in_block=false) rc scope e =
	match e.eexpr with
	| TBlock el -> List.iter (collect_vars ~in_block rc scope) el
	| _ -> collect_vars ~in_block rc scope e

let trailing_numbers = Str.regexp "[0-9]+$"

(**
	Rename `v` if needed
*)
let maybe_rename_var rc reserved (v,overlaps) =
	(* chop escape char for all local variables generated *)
	if is_gen_local v then begin
		let name = String.sub v.v_name 1 (String.length v.v_name - 1) in
		v.v_name <- "_g" ^ (Str.replace_first trailing_numbers "" name)
	end;
	let name = ref v.v_name in
	let count = ref 0 in
	let same_name _ o = !name = o.v_name in
	while (
		StringMap.mem !name !reserved
		|| IntMap.exists same_name !overlaps
	) do
		incr count;
		name := v.v_name ^ (string_of_int !count);
	done;
	v.v_name <- !name;
	if rc.rc_no_shadowing || (has_var_flag v VCaptured && rc.rc_hoisting) then reserve reserved v.v_name

(**
	Rename variables found in `scope`
*)
let rec rename_vars rc scope =
	let reserved = ref rc.rc_reserved in
	if (rc.rc_hoisting || rc.rc_no_shadowing) && not (IntMap.is_empty scope.foreign_vars) then
		IntMap.iter (fun _ v -> reserve reserved v.v_name) scope.foreign_vars;
	List.iter (maybe_rename_var rc reserved) (List.rev scope.own_vars);
	List.iter (rename_vars rc) scope.children

(**
	Rename local variables in `e` expression if needed.
*)
let run ctx ri e =
	(try
		let rc = {
			rc_scope = ri.ri_scope;
			rc_hoisting = ri.ri_hoisting;
			rc_no_shadowing = ri.ri_no_shadowing;
			rc_switch_cases_no_blocks = ri.ri_switch_cases_no_blocks;
			rc_reserved = ri.ri_reserved;
		} in
		if ri.ri_reserve_current_top_level_symbol then begin
			match ctx.curclass.cl_path with
			| s :: _,_ | [],s -> reserve_ctx rc s
		end;
		let scope = create_scope None in
		collect_vars rc scope e;
		rename_vars rc scope;
	with Failure msg ->
		die ~p:e.epos msg __LOC__
	);
	e