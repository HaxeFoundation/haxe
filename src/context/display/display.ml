open Ast
open Common
open DisplayTypes
open DisplayMode
open CompletionItem
open Type
open Typecore
open Globals
open DisplayPosition
open ImportStatus

let merge_core_doc ctx mtype =
	display_position#run_outside (fun () -> Typecore.merge_core_doc ctx mtype)

let parse_module' com m p =
	display_position#run_outside (fun () -> TypeloadParse.parse_module' com m p)

let parse_module ctx m p =
	display_position#run_outside (fun () -> TypeloadParse.parse_module ctx m p)

module ReferencePosition = struct
	let reference_position = ref ("",null_pos,SKOther)
	let set (s,p,k) =
		let p =
			if p = null_pos then p
			else {p with pfile = Path.get_full_path p.pfile}
		in
		reference_position := (s,p,k)
	let get () = !reference_position
	let reset () = reference_position := ("",null_pos,SKOther)
end

let preprocess_expr com e = match com.display.dms_kind with
	| DMDefinition | DMTypeDefinition | DMUsage _ | DMImplementation | DMHover | DMDefault -> ExprPreprocessing.find_before_pos com.display.dms_kind e
	| DMSignature -> ExprPreprocessing.find_display_call e
	| _ -> e

let sort_fields l with_type tk =
	let p = match tk with
		| TKExpr p | TKField p -> Some p
		| _ -> None
	in
	let expected_name = WithType.get_expected_name with_type in
	let l = List.map (fun ci ->
		let i = get_sort_index tk ci (Option.default Globals.null_pos p) expected_name in
		ci,i
	) l in
	let sort l =
		List.map fst (List.sort (fun (_,i1) (_,i2) -> compare i1 i2) l)
	in
	(* This isn't technically accurate, but I don't think it matters. *)
	let rec dynamify_type_params t = match follow t with
		| TInst({cl_kind = KTypeParameter _},_) -> mk_mono()
		| _ -> Type.map dynamify_type_params t
	in
	let l = match with_type with
		| WithType.WithType(t,_) when (match follow t with TMono _ -> false | _ -> true) ->
			let comp item = match item.ci_type with
				| None -> 9
				| Some (t',_) ->
				(* For enum constructors, we consider the return type of the constructor function
				   so it has the same priority as argument-less constructors. *)
				let t' = match item.ci_kind,follow t' with
					| ITEnumField _,TFun(_,r) -> r
					| _ -> t'
				in
				let t' = dynamify_type_params t' in
				if type_iseq t' t then 0 (* equal types - perfect *)
				else if t' == t_dynamic then 5 (* dynamic isn't good, but better than incompatible *)
				else try Type.unify t' t; 1 (* assignable - great *)
				with Unify_error _ -> match follow t' with
					| TFun(_,tr) ->
						if type_iseq tr t then 2 (* function returns our exact type - alright *)
						else (try Type.unify tr t; 3 (* function returns compatible type - okay *)
						with Unify_error _ -> 7) (* incompatible function - useless *)
					| _ ->
						6 (* incompatible type - probably useless *)
			in
			let l = List.map (fun (item,i1) ->
				let i2 = comp item in
				item,(i2,i1)
			) l in
			sort l
		| _ ->
			sort l
	in
	l

let get_import_status ctx path =
	try
		let mt' = ctx.g.do_load_type_def ctx null_pos (mk_type_path ([],snd path)) in
		if path <> (t_infos mt').mt_path then Shadowed else Imported
	with _ ->
		Unimported
