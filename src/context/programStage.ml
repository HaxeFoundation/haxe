(* The stages of [program], in the order they run. A unit added to a stage
   runs once every earlier stage has no unit left. *)
type stage =
	| StageModules		(* the structure of loaded modules and the type parameters of their types *)
	| StageTypes		(* the builds of classes *)
	| StageMembers		(* the connection of members not attached to the build of a class, which may affect each other (e.g. a property and its getter) *)
	| StageBodies		(* the bodies of fields *)
	| StageConstraints	(* the type parameter constraint checks, with inferred types *)
	| StageChecks		(* the per-type checks and the forcing of the remaining lazy types *)
	| StageFinal		(* the end of typing *)

let all_stages = [
	StageModules;StageTypes;StageMembers;StageBodies;StageConstraints;StageChecks;StageFinal
]

let stage_count = List.length all_stages
