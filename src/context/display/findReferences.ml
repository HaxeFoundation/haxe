open Globals
open Ast
open DisplayTypes
open Common
open Type
open Typecore
open ImportHandling

let find_possible_references tctx cs =
	let name,_,kind = Display.ReferencePosition.get () in
	ignore(SyntaxExplorer.explore_uncached_modules tctx cs [name,kind])

let find_references tctx com with_definition pos_filters =
	let t = Timer.timer ["display";"references";"collect"] in
	let symbols,relations = Statistics.collect_statistics tctx pos_filters true in
	t();
	let rec loop acc (relations:(Statistics.relation * pos) list) = match relations with
		| (Statistics.Referenced,p) :: relations when not (List.mem p acc) -> loop (p :: acc) relations
		| _ :: relations -> loop acc relations
		| [] -> acc
	in
	let t = Timer.timer ["display";"references";"filter"] in
	let usages = Hashtbl.fold (fun p sym acc ->
		let acc = if with_definition then p :: acc else acc in
		(try loop acc (Hashtbl.find relations p)
		with Not_found -> acc)
	) symbols [] in
	t();
	Display.ReferencePosition.reset();
	usages

let collect_reference_positions com =
	let name,pos,kind = Display.ReferencePosition.get () in
	match kind, com.display.dms_kind with
	| SKField (cf,Some c), DMUsage (_,find_descendants,find_base) when (find_descendants || find_base) && not (has_class_field_flag cf CfStatic) ->
		let collect() =
			let c =
				let rec loop = function
					| [] -> raise Exit
					| TClassDecl c' :: _ when c'.cl_path = c.cl_path ->
						print_endline (Printf.sprintf "%s: %b (%s, %s)" (s_type_path c.cl_path) (c == c') (Digest.to_hex c.cl_module.m_extra.m_sign) (Digest.to_hex c'.cl_module.m_extra.m_sign));
						c'
					| _ :: types -> loop types
				in
				loop com.types
			in
			let field_class_pairs =
				(* check classes hierarchy *)
				let cf,c =
					if find_base then
						let rec loop c =
							match c.cl_super with
							| None -> (PMap.find cf.cf_name c.cl_fields),c
							| Some (csup,_) ->
								try loop csup
								with Not_found -> (PMap.find cf.cf_name c.cl_fields),c
						in
						try loop c
						with Not_found -> cf,c
					else
						cf,c
				in
				(* check interfaces of the found base class *)
				let rec fold_interface acc (i,_) =
					try loop i @ acc
					with Not_found -> acc
				and loop c =
					match List.fold_left fold_interface [] c.cl_implements with
					| [] -> [(PMap.find cf.cf_name c.cl_fields),c]
					| pairs -> pairs
				in
				match List.fold_left fold_interface [] c.cl_implements with
				| [] -> [cf,c]
				| pairs -> pairs
			in
			let full_pos p = { p with pfile = Path.get_full_path p.pfile } in
			if find_descendants then
				let extends child_cls (_,c) = extends child_cls c in
				List.fold_left (fun acc t ->
					match t with
					| TClassDecl child_cls when List.exists (extends child_cls) field_class_pairs ->
						(try
							let cf = PMap.find cf.cf_name child_cls.cl_fields in
							(name,full_pos cf.cf_name_pos,SKField (cf,Some child_cls)) :: acc
						with Not_found -> acc
						)
					| _ ->
						acc
				) [] com.types
			else
				List.map (fun (cf,c) -> name,full_pos cf.cf_name_pos,SKField (cf,Some c)) field_class_pairs;
		in
		(try collect()
		with Exit -> [name,pos,kind])
	| _ ->
		[name,pos,kind]

let find_references tctx com with_definition =
	let pos_filters =
		List.fold_left (fun acc (_,p,_) ->
			if p = null_pos then acc
			else Statistics.SFPos p :: acc
		) [] (collect_reference_positions com)
	in
	let usages = find_references tctx com with_definition pos_filters in
	let usages =
		List.sort (fun p1 p2 ->
			let c = compare p1.pfile p2.pfile in
			if c <> 0 then c else compare p1.pmin p2.pmin
		) usages
	in
	DisplayException.raise_positions usages

let find_implementations tctx com name pos kind =
	let t = Timer.timer ["display";"implementations";"collect"] in
	let symbols,relations = Statistics.collect_statistics tctx [SFPos pos] false in
	t();
	let rec loop acc relations = match relations with
		| ((Statistics.Implemented | Statistics.Overridden | Statistics.Extended),p) :: relations -> loop (p :: acc) relations
		| _ :: relations -> loop acc relations
		| [] -> acc
	in
	let t = Timer.timer ["display";"implementations";"filter"] in
	let usages = Hashtbl.fold (fun p sym acc ->
		(try loop acc (Hashtbl.find relations p)
		with Not_found -> acc)
	) symbols [] in
	let usages = List.sort (fun p1 p2 ->
		let c = compare p1.pfile p2.pfile in
		if c <> 0 then c else compare p1.pmin p2.pmin
	) usages in
	t();
	Display.ReferencePosition.reset();
	DisplayException.raise_positions usages

let find_implementations tctx com =
	let name,pos,kind = Display.ReferencePosition.get () in
	if pos <> null_pos then find_implementations tctx com name pos kind
	else DisplayException.raise_positions []