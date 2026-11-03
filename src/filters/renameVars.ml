open Globals
open Type
open Ast
open PlatformConfig

type rename_init = {
	mutable ri_scope : var_scope;
	mutable ri_hoisting : bool;
	mutable ri_no_shadowing : bool;
	mutable ri_no_catch_var_shadowing : bool;
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
let reserve_all_types ri types path_to_name =
	List.iter (fun mt ->
		let tinfos = t_infos mt in
		let native_name = try fst (Native.get_native_name tinfos.mt_meta) with Not_found -> path_to_name tinfos.mt_path in
		match mt with
		| TClassDecl c when native_name = "" ->
			List.iter (fun cf ->
				let native_name = try fst (Native.get_native_name cf.cf_meta) with Not_found -> cf.cf_name in
				reserve_init ri native_name
			) c.cl_ordered_statics
		| TClassDecl { cl_kind = KModuleFields m; cl_ordered_statics = fl } ->
			let prefix = Path.flat_path m.m_path ^ "_" in
			List.iter (fun cf ->
				let name = try fst (Native.get_native_name cf.cf_meta) with Not_found -> prefix ^ cf.cf_name in
				reserve_init ri name
			) fl
		| _ ->
			reserve_init ri native_name
	) types

(**
	Initialize the context for local variables renaming
*)
let init config types =
	let ri = {
		ri_scope = config.pf_scoping.vs_scope;
		ri_reserved = StringMap.empty;
		ri_hoisting = false;
		ri_no_shadowing = false;
		ri_no_catch_var_shadowing = false;
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
		| NoCatchVarShadowing ->
			ri.ri_no_catch_var_shadowing <- true;
		| SwitchCasesNoBlocks ->
			ri.ri_switch_cases_no_blocks <- true;
		| ReserveNames names ->
			List.iter (reserve_init ri) names
		| ReserveAllTopLevelSymbols ->
			reserve_all_types ri types (fun (pack,name) -> if pack = [] then name else List.hd pack)
		| ReserveAllTypesFlat ->
			reserve_all_types ri types Path.flat_path
		| ReserveCurrentTopLevelSymbol -> ri.ri_reserve_current_top_level_symbol <- true
	) config.pf_scoping.vs_flags;
	ri

module Overlaps = struct
	(* Both fields are persistent, so copies share their contents with the original. Every variable
	   declared in a loop gets a copy of the loop's variables, which would otherwise be quadratic. *)
	type t = {
		mutable ov_vars : tvar list;
		mutable ov_lut : bool IntMap.t;
	}

	let create () = {
		ov_vars = [];
		ov_lut = IntMap.empty;
	}

	let copy ov = {
		ov_vars = ov.ov_vars;
		ov_lut = ov.ov_lut;
	}

	let add v ov =
		ov.ov_vars <- v :: ov.ov_vars;
		ov.ov_lut <- IntMap.add v.v_id true ov.ov_lut

	let iter f ov =
		List.iter f ov.ov_vars

	let mem id ov =
		IntMap.mem id ov.ov_lut

	let reset ov =
		ov.ov_vars <- [];
		ov.ov_lut <- IntMap.empty

	let is_empty ov = match ov.ov_vars with
		| [] ->
			true
		| _ ->
			false

end

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
	mutable own_vars : (tvar * Overlaps.t) list;
	(** Variables declared outside of this scope, but used inside of it *)
	mutable foreign_vars : Overlaps.t;
	(** List of variables used in current loop *)
	loop_vars : Overlaps.t;
	(** Current loops depth *)
	mutable loop_count : int;
}

type rename_context = {
	rc_hoisting : bool;
	rc_no_shadowing : bool;
	rc_no_catch_var_shadowing : bool;
	rc_switch_cases_no_blocks : bool;
	rc_scope : var_scope;
	mutable rc_reserved : bool StringMap.t;
	(** Scope a variable is declared in *)
	rc_var_origins : scope IntHashtbl.t;
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
		foreign_vars = Overlaps.create();
		loop_vars = Overlaps.create();
		loop_count = 0;
	} in
	Option.may (fun p -> p.children <- scope :: p.children) parent;
	scope

(**
	Invoked for each `TVar v` texpr_expr
*)
let declare_var rc scope v =
	let overlaps =
		if not rc.rc_hoisting || Overlaps.is_empty scope.foreign_vars then
			if scope.loop_count = 0 then Overlaps.create ()
			else Overlaps.copy scope.loop_vars
		else
			if scope.loop_count = 0 then
				Overlaps.copy scope.foreign_vars
			else begin
				let overlaps = Overlaps.copy scope.loop_vars in
				Overlaps.iter (fun o ->
					Overlaps.add o overlaps
				) scope.foreign_vars;
				overlaps
			end
	in
	scope.own_vars <- (v, overlaps) :: scope.own_vars;
	IntHashtbl.add rc.rc_var_origins v.v_id scope;
	if scope.loop_count > 0 then
		Overlaps.add v scope.loop_vars

let will_be_reserved rc v =
	rc.rc_no_shadowing || (has_var_flag v VCaptured && rc.rc_hoisting)

let unbound_variable v =
	raise (Failure (Printf.sprintf "Unbound variable: %s<%i>" v.v_name v.v_id))

(**
	Invoked for each `TLocal v` texr_expr
*)
let rec determine_overlaps rc scope v =
	let rec loop declarations =
		match declarations with
		| [] ->
			if (rc.rc_no_shadowing || rc.rc_hoisting) then
				Overlaps.add v scope.foreign_vars;
			(match scope.parent with
			| Some parent -> determine_overlaps rc parent v
			| None -> unbound_variable v
			)
		| (d, _) :: _ when d == v ->
			()
		| (d, overlaps) :: rest ->
			(* If we find a declaration that already knows us, we don't have to keep
			   looping because we can be sure that we've been here before. *)
			if not (Overlaps.mem v.v_id overlaps) then begin
				Overlaps.add v overlaps;
				loop rest
			end
	in
	loop scope.own_vars

let use_var rc scope v =
	if not (will_be_reserved rc v) then
		determine_overlaps rc scope v
	else begin
		let origin = IntHashtbl.find rc.rc_var_origins v.v_id in
		let rec loop scope =
			if scope != origin then begin
				if (rc.rc_no_shadowing || rc.rc_hoisting) then
					Overlaps.add v scope.foreign_vars;
				match scope.parent with
				| Some parent ->
					loop parent
				| None ->
					unbound_variable v
			end
		in
		loop scope
	end;
	if scope.loop_count > 0 then
		Overlaps.add v scope.loop_vars

let collect_loop scope fn =
	scope.loop_count <- scope.loop_count + 1;
	fn();
	scope.loop_count <- scope.loop_count - 1;
	if scope.loop_count < 0 then
		raise (Failure "Unexpected loop count");
	if scope.loop_count = 0 then
		Overlaps.reset scope.loop_vars

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
		List.iter (fun (_,eo) -> Option.may (collect_vars scope) eo) fn.tf_args;
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
			collect_vars scope e;
			if rc.rc_no_catch_var_shadowing then use_var rc scope v;
		) catches
	| TSwitch switch when rc.rc_switch_cases_no_blocks ->
		collect_vars scope switch.switch_subject;
		List.iter (fun case ->
			List.iter (collect_vars scope) case.case_patterns;
			collect_ignore_block ~in_block:true rc scope case.case_expr
		) switch.switch_cases;
		Option.may (collect_ignore_block ~in_block:true rc scope) switch.switch_default
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
	| _ ->
		iter (collect_vars scope) e

and collect_ignore_block ?(in_block=false) rc scope e =
	match e.eexpr with
	| TBlock el -> List.iter (collect_vars ~in_block rc scope) el
	| _ -> collect_vars ~in_block rc scope e

(**
	All variables by their current name. This allows checking whether a variable overlaps with a
	variable of a given name without collecting the names of everything it overlaps with.
*)
module VarsByName = struct
	type t = (string, tvar IntMap.t) Hashtbl.t

	let find vars name =
		try Hashtbl.find vars name with Not_found -> IntMap.empty

	let add vars v =
		Hashtbl.replace vars v.v_name (IntMap.add v.v_id v (find vars v.v_name))

	let remove vars v =
		Hashtbl.replace vars v.v_name (IntMap.remove v.v_id (find vars v.v_name))

	let has_overlap vars name overlaps =
		IntMap.exists (fun id _ -> Overlaps.mem id overlaps) (find vars name)

	let rec add_scope vars scope =
		List.iter (fun (v,_) -> add vars v) scope.own_vars;
		List.iter (add_scope vars) scope.children
end

(**
	Rename `v` if needed
*)
let trailing_numbers = Str.regexp "[0-9]+$"
let maybe_rename_var rc vars reserved (v,overlaps) =
	let commit name =
		v.v_meta <- (Meta.RealPath,[EConst (String(v.v_name,SDoubleQuotes)),null_pos],null_pos) :: v.v_meta;
		(* removes v under its old name *)
		VarsByName.remove vars v;
		v.v_name <- name;
		(* adds v under its new name *)
		VarsByName.add vars v
	in
	(* chop escape char for all local variables generated *)
	if String.unsafe_get v.v_name 0 = String.unsafe_get gen_local_prefix 0 then begin
		let name = String.sub v.v_name 1 (String.length v.v_name - 1) in
		commit ("_g" ^ (Str.replace_first trailing_numbers "" name))
	end;
	let rec loop name count =
		if StringMap.mem name !reserved || VarsByName.has_overlap vars name overlaps then begin
			let count = count + 1 in
			loop (v.v_name ^ (string_of_int count)) count
		end else
			name
	in
	let name = loop v.v_name 0 in
	commit name;
	if will_be_reserved rc v then reserve reserved v.v_name

(**
	Rename variables found in `scope`
*)
let rec rename_vars rc vars scope =
	let reserved = ref rc.rc_reserved in
	if (rc.rc_hoisting || rc.rc_no_shadowing) && not (Overlaps.is_empty scope.foreign_vars) then
		Overlaps.iter (fun v -> reserve reserved v.v_name) scope.foreign_vars;
	List.iter (maybe_rename_var rc vars reserved) (List.rev scope.own_vars);
	List.iter (rename_vars rc vars) scope.children

(**
	Rename local variables in `e` expression if needed.
*)
let run cl_path ri e =
	let rc = {
		rc_scope = ri.ri_scope;
		rc_hoisting = ri.ri_hoisting;
		rc_no_shadowing = ri.ri_no_shadowing;
		rc_no_catch_var_shadowing = ri.ri_no_catch_var_shadowing;
		rc_switch_cases_no_blocks = ri.ri_switch_cases_no_blocks;
		rc_reserved = ri.ri_reserved;
		rc_var_origins = IntHashtbl.create 0;
	} in
	if ri.ri_reserve_current_top_level_symbol then begin
		match cl_path with
		| s :: _,_ | [],s -> reserve_ctx rc s
	end;
	let scope = create_scope None in
	collect_vars rc scope e;
	let vars = Hashtbl.create 0 in
	VarsByName.add_scope vars scope;
	rename_vars rc vars scope;
	e
