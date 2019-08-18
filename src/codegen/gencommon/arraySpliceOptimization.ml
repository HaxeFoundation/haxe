open Common
open Type
open Gencommon

(*
	This filter finds lone array.splice(...) calls within blocks
	and replaces them with array.spliceVoid(...) calls
	that don't allocate additional array for removed items.
*)
let init com =
	let rec run e =
		match e.eexpr with
		| TBlock el ->
			let el = List.map (fun e ->
				match e.eexpr with
				| TCall ({ eexpr = TField (eobj, FInstance ({ cl_path = [],"Array" } as cl, params, { cf_name = "splice" })) } as e_splice, args) ->
					let f_spliceVoid = PMap.find "spliceVoid" cl.cl_fields in
					let e_spliceVoid = { e_splice with
						eexpr = TField (eobj, FInstance (cl, params, f_spliceVoid));
						etype = f_spliceVoid.cf_type;
					} in
					{ e with
						eexpr = TCall (e_spliceVoid, args);
						etype = com.basic.tvoid;
					}
				| _ ->
					run e
			) el in
			{ e with eexpr = TBlock el }
		| _ ->
			Type.map_expr run e
	in
	run

let name = "array_splice_synf"
let priority = solve_deps name [DAfter ExpressionUnwrap.priority]

let configure gen =
	let run = init gen.gcon in
	gen.gsyntax_filters#add name (PCustom priority) run
