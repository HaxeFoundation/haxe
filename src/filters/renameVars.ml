open Globals
open Type
open Typecore
open Common
open Ast

type rename_init = {
	mutable ri_reserve_top_level_symbol : bool;
	mutable ri_reserved : bool StringMap.t;
}

(**
	For initialization.
	Make the `name` a reserved word.
	No local variable will be allowed to have such name.
*)
let reserve_init ri name =
	ri.ri_reserved <- StringMap.add name true ri.ri_reserved

(**
	Make all class names reserved names.
	No local variable will have a name matching a class.
*)
let reserve_all_class_names ri com =
	List.iter (fun mt ->
		let tinfos = t_infos mt in
		let native_name = try fst (TypeloadCheck.get_native_name tinfos.mt_meta) with Not_found -> Path.flat_path tinfos.mt_path in
		if native_name = "" then
			match mt with
			| TClassDecl c ->
				List.iter (fun cf ->
					let native_name = try fst (TypeloadCheck.get_native_name cf.cf_meta) with Not_found -> cf.cf_name in
					reserve_init ri native_name
				) c.cl_ordered_statics;
			| _ -> ()
		else
			reserve_init ri native_name
	) com.types

(**
	Initialize the context for local variables renaming
*)
let init com =
	let ri = {
		ri_reserve_top_level_symbol = false;
		ri_reserved = StringMap.empty
	} in
	(match com.platform with
	| Php ->
		reserve_init ri "this";
		List.iter (fun flag ->
			match flag with
			| ReserveNames names -> List.iter (reserve_init ri) names
			| ReserveAllClassNames -> reserve_all_class_names ri com
			| ReserveTopLevelSymbol -> ri.ri_reserve_top_level_symbol <- true
		) com.config.pf_scoping.vs_flags
	(* Old behavior *)
	| _ ->
		ri.ri_reserved <- RenameVarsOld.collect_reserved_local_names com
	);
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
	mutable vars : (tvar * (tvar list ref)) list;
	(** List of variables used in current loop *)
	loop_vars : tvar list ref;
	(** Current loops depth *)
	mutable loop_count : int;
}

type rename_context = {
	mutable rc_reserved : bool StringMap.t;
	mutable rc_captured : tvar list;
}

(**
	Make `name` a reserved word.
	No local variable will be allowed to have such name.
*)
let reserve rc name =
	rc.rc_reserved <- StringMap.add name true rc.rc_reserved

let create_scope parent =
	let scope = {
		parent = parent;
		children = [];
		vars = [];
		loop_vars = (match parent with Some p -> p.loop_vars | None -> ref []);
		loop_count = (match parent with Some p -> p.loop_count | None -> 0);
	} in
	Option.may (fun p -> p.children <- scope :: p.children) parent;
	scope

(**
	Invoked for each `TVar v` texpr_expr
*)
let declare_var rc scope v =
	let overlaps =
		if scope.loop_count = 0 then []
		else !(scope.loop_vars)
	in
	scope.vars <- (v, ref overlaps) :: scope.vars;
	if scope.loop_count > 0 then
		scope.loop_vars := v :: !(scope.loop_vars);
	if v.v_capture then
		rc.rc_captured <- v :: rc.rc_captured

(**
	Invoked for each `TLocal v` texr_expr
*)
let rec use_var rc scope v =

	let rec loop declarations =
		match declarations with
		| [] ->
			(match scope.parent with
			| Some parent -> use_var rc parent v
			| None -> assert false
			)
		| (d, _) :: _ when d == v -> ()
		| (d, overlaps) :: rest ->
			if not (List.memq v !overlaps) then
				overlaps := v :: !overlaps;
			loop rest
	in
	loop scope.vars

let rec collect_vars rc scope e =
	match e.eexpr with
	| TVar (v, e_opt) ->
		declare_var rc scope v;
		Option.may (collect_vars rc scope) e_opt
	| TLocal v ->
		use_var rc scope v
	| TWhile (condition, body, NormalWhile) ->
		scope.loop_count <- scope.loop_count + 1;
		collect_vars rc scope condition;
		collect_vars rc scope body;
		scope.loop_count <- scope.loop_count - 1;
	(* At this point all loops are expected to be transformed into normal `while` loops *)
	| TFor _ | TWhile _ ->
		assert false
	| _ ->
		iter (collect_vars rc scope) e

(**
	Rename local variables in `e` expression if needed.
*)
let run ctx ri e =
	match ctx.com.platform with
	| Php ->
		let rc = {
			rc_reserved = ri.ri_reserved;
			rc_captured = [];
		} in
		if ri.ri_reserve_top_level_symbol then begin
			match ctx.curclass.cl_path with
			| s :: _,_ | [],s -> reserve rc s
		end;
		let scope = create_scope None in
		collect_vars rc scope e;
		e
	(* Old behavior *)
	| _ ->
		RenameVarsOld.rename_local_vars ctx ri.ri_reserved e