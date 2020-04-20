open Globals
open Ast
open DisplayTypes
open Common
open Type
open Typecore
open CompilationServer
open ImportHandling

let find_possible_references tctx cs =
	let name,_,kind = Display.ReferencePosition.get () in
	ignore(SyntaxExplorer.explore_uncached_modules tctx cs [name,kind])

let find_references tctx com with_definition name pos kind =
	let t = Timer.timer ["display";"references";"collect"] in
	let symbols,relations = Statistics.collect_statistics tctx (SFPos pos) true in
	t();
	let rec loop acc relations = match relations with
		| (Statistics.Referenced,p) :: relations -> loop (p :: acc) relations
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
	Display.ReferencePosition.set ("",null_pos,SKOther);
	usages

let collect_reference_positions com =
	let name,pos,kind = Display.ReferencePosition.get () in
	match kind, com.display.dms_kind with
	| SKField (cf,Some cl_path), DMUsage (_,find_descendants,find_base) when find_descendants || find_base ->
		let collect() =
			let c =
				let rec loop = function
					| [] -> raise Exit
					| TClassDecl c :: _ when c.cl_path = cl_path -> c
					| _ :: types -> loop types
				in
				loop com.types
			in
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
			let full_pos p = { p with pfile = Path.unique_full_path p.pfile } in
			if find_descendants then
				List.fold_left (fun acc t ->
					match t with
					| TClassDecl child_cls when extends child_cls c ->
						(try
							let cf = PMap.find cf.cf_name child_cls.cl_fields in
							(name,full_pos cf.cf_name_pos,SKField (cf,Some child_cls.cl_path)) :: acc
						with Not_found -> acc
						)
					| _ ->
						acc
				) [] com.types
			else
				[name,full_pos cf.cf_name_pos,SKField (cf,Some c.cl_path)]
		in
		(try collect()
		with Exit -> [name,pos,kind])
	| _ ->
		[name,pos,kind]

let find_references tctx com with_definition =
	let usages =
		List.fold_left (fun acc (name,pos,kind) ->
			if pos = null_pos then acc
			else acc @ (find_references tctx com with_definition name pos kind)
		) [] (collect_reference_positions com)
	in
	let usages =
		List.sort (fun p1 p2 ->
			let c = compare p1.pfile p2.pfile in
			if c <> 0 then c else compare p1.pmin p2.pmin
		) usages
	in
	Display.ReferencePosition.reset();
	DisplayException.raise_positions usages

let find_implementations tctx com name pos kind =
	let t = Timer.timer ["display";"implementations";"collect"] in
	let symbols,relations = Statistics.collect_statistics tctx (SFPos pos) false in
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