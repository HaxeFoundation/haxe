open Ast
open Common
open Type
open Typecore
open Error
open Globals
open FiltersCommon

let get_native_name = TypeloadCheck.get_native_name

(* -------------------------------------------------------------------------- *)
(* RENAME LOCAL VARS *)

let collect_reserved_local_names com =
	match com.platform with
	| Js ->
		let h = ref StringMap.empty in
		let add name = h := StringMap.add name true !h in
		List.iter (fun mt ->
			let tinfos = t_infos mt in
			let native_name = try fst (get_native_name tinfos.mt_meta) with Not_found -> Path.flat_path tinfos.mt_path in
			if native_name = "" then
				match mt with
				| TClassDecl c ->
					List.iter (fun cf ->
						let native_name = try fst (get_native_name cf.cf_meta) with Not_found -> cf.cf_name in
						add native_name
					) c.cl_ordered_statics;
				| _ -> ()
			else
				add native_name
		) com.types;
		!h
	| _ -> StringMap.empty

let rec rename_local_vars_aux ctx reserved e =
	let initial_reserved = reserved in
	let own_vars = Hashtbl.create 10 in
	let own_vars_ordered = ref [] in
	let foreign_vars = Hashtbl.create 5 in (* Variables which are declared outside of `e` *)
	let declare v =
		own_vars_ordered := v :: !own_vars_ordered;
		Hashtbl.add own_vars v.v_id v
	in
	let referenced v =
		if not (Hashtbl.mem own_vars v.v_id) && not (Hashtbl.mem foreign_vars v.v_name) then
			Hashtbl.add foreign_vars v.v_name ()
	in
	let reserved = ref reserved in
	let reserve name =
		reserved := StringMap.add name true !reserved
	in
	let check t =
		match (t_infos t).mt_path with
		| [], name | name :: _, _ -> reserve name
	in
	let check_type t =
		match follow t with
		| TInst (c,_) -> check (TClassDecl c)
		| TEnum (e,_) -> check (TEnumDecl e)
		| TType (t,_) -> check (TTypeDecl t)
		| TAbstract (a,_) -> check (TAbstractDecl a)
		| TMono _ | TLazy _ | TAnon _ | TDynamic _ | TFun _ -> ()
	in
	let funcs = ref [] in
	let rec collect e = match e.eexpr with
		| TLocal v ->
			referenced v
 		| TVar(v,eo) ->
			declare v;
			(match eo with None -> () | Some e -> collect e)
		| TFor(v,e1,e2) ->
			declare v;
			collect e1;
			collect e2;
		| TTry(e1,catches) ->
			collect e1;
			List.iter (fun (v,e) ->
				declare v;
				check_type v.v_type;
				collect e
			) catches
		| TFunction tf ->
			begin match ctx.com.config.pf_nested_function_scoping with
			| Hoisted ->
				funcs := tf :: !funcs;
			| Nested | Independent ->
				List.iter (fun (v,_) -> declare v) tf.tf_args;
				collect tf.tf_expr
			end
		| TTypeExpr t ->
			check t
		| TNew (c,_,_) ->
			Type.iter collect e;
			check (TClassDecl c);
		| TCast (e,Some t) ->
			collect e;
			check t;
		| TConst TSuper ->
			check_type e.etype
		| _ ->
			Type.iter collect e
	in
	(* Pass 1: Collect used identifiers and variables. *)
	collect e;
	(* Pass 2: Check and rename variables. *)
	let count_table = Hashtbl.create 0 in
	let maybe_rename v =
		(* chop escape char for all local variables generated *)
		if is_gen_local v then v.v_name <- "_g" ^ String.sub v.v_name 1 (String.length v.v_name - 1);
		let name = ref v.v_name in
		let count = ref (try Hashtbl.find count_table v.v_name with Not_found -> 0) in
		while StringMap.mem !name !reserved do
			incr count;
			name := v.v_name ^ (string_of_int !count);
		done;
		reserve !name;
		Hashtbl.replace count_table v.v_name !count;
		if not (Meta.has Meta.RealPath v.v_meta) then
			v.v_meta <- (Meta.RealPath,[EConst (String(v.v_name,SDoubleQuotes)),e.epos],e.epos) :: v.v_meta;
		v.v_name <- !name;
	in
	Hashtbl.iter (fun name _ -> reserve name) foreign_vars;
	List.iter maybe_rename (List.rev !own_vars_ordered);
	(* Pass 3: Recurse into nested functions. *)
	List.iter (fun tf ->
		reserved := initial_reserved;
		List.iter (fun (v,_) ->
			maybe_rename v;
		) tf.tf_args;
		ignore(rename_local_vars_aux ctx !reserved tf.tf_expr);
	) !funcs

let rename_local_vars ctx reserved e =
	let reserved = ref reserved in
	let reserve name =
		reserved := StringMap.add name true !reserved
	in
	reserve "this";
	if ctx.com.platform = Java then reserve "_";
	begin match ctx.curclass.cl_path with
		| s :: _,_ | [],s -> reserve s
	end;
	rename_local_vars_aux ctx !reserved e;
	e