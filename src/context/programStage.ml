(* The stages of [program] (docs/incremental-compilation.md, phase 3.1), in
   the order they run. A unit added to a stage runs once every earlier stage
   has no unit left. *)
type stage =
	| StageModules		(* the structure of loaded modules and the type parameters of their types *)
	| StageTypes		(* the builds of classes ([members] queries not demanded yet) *)
	| StageMembers		(* the connection of members not attached to the build of a class *)
	| StageSigs			(* the fields whose type is inferred from their body ([sig] queries) *)
	| StageBodies		(* the bodies of fields ([body] queries) *)
	| StageConstraints	(* the type parameter constraint checks, with inferred types *)
	| StageChecks		(* the per-type checks and the forcing of the remaining lazy types *)
	| StageFinal		(* the end of typing *)

let all_stages = [
	StageModules;StageTypes;StageMembers;StageSigs;StageBodies;StageConstraints;StageChecks;StageFinal
]

let stage_count = List.length all_stages

let stage_name = function
	| StageModules -> "modules"
	| StageTypes -> "types"
	| StageMembers -> "members"
	| StageSigs -> "sigs"
	| StageBodies -> "bodies"
	| StageConstraints -> "constraints"
	| StageChecks -> "checks"
	| StageFinal -> "final"
