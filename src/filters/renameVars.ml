open Globals
open Type
open Typecore
open Common

type rename_context = {
	mutable reserved : bool StringMap.t;
}

(**
	Make the `name` a reserved word.
	No local variable will be allowed to have such name.
*)
let reserve r name =
	r.reserved <- StringMap.add name true r.reserved

(**
	Make all class names reserved names.
	No local variable will have a name matching a class.
*)
let reserve_all_class_names r com =
	List.iter (fun mt ->
		let tinfos = t_infos mt in
		let native_name = try fst (TypeloadCheck.get_native_name tinfos.mt_meta) with Not_found -> Path.flat_path tinfos.mt_path in
		if native_name = "" then
			match mt with
			| TClassDecl c ->
				List.iter (fun cf ->
					let native_name = try fst (TypeloadCheck.get_native_name cf.cf_meta) with Not_found -> cf.cf_name in
					reserve r native_name
				) c.cl_ordered_statics;
			| _ -> ()
		else
			reserve r native_name
	) com.types

(**
	Initialize the context for local variables renaming
*)
let init com =
	let r = {
		reserved = StringMap.empty
	} in
	(match com.platform with
	| Php ->
		List.iter (fun flag ->
			match flag with
			| ReserveNames names -> List.iter (reserve r) names
			| ReserveAllClassNames -> reserve_all_class_names r com
		) com.config.pf_scoping.vs_flags
	(* Old behavior *)
	| _ ->
		r.reserved <- RenameVarsOld.collect_reserved_local_names com
	);
	r

(**
	Rename local variables in `e` expression if needed.
*)
let run ctx r e =
	match ctx.com.platform with
	(* Old behavior *)
	| _ ->
		RenameVarsOld.rename_local_vars ctx r.reserved e