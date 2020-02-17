open Globals
open Ast
open DisplayTypes
open Common
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
	let usages = List.sort (fun p1 p2 ->
		let c = compare p1.pfile p2.pfile in
		if c <> 0 then c else compare p1.pmin p2.pmin
	) usages in
	t();
	Display.ReferencePosition.set ("",null_pos,SKOther);
	DisplayException.raise_positions usages

let find_references tctx com with_definition =
	let name,pos,kind = Display.ReferencePosition.get () in
	if pos <> null_pos then find_references tctx com with_definition name pos kind
	else DisplayException.raise_positions []

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
	Display.ReferencePosition.set ("",null_pos,SKOther);
	DisplayException.raise_positions usages

let find_implementations tctx com =
	let name,pos,kind = Display.ReferencePosition.get () in
	if pos <> null_pos then find_implementations tctx com name pos kind
	else DisplayException.raise_positions []