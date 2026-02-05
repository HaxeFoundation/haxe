package cs.system.data;

/** Determines the action that occurs when a mapping is missing from a source table or a source column. */
@:native("System.Data.MissingMappingAction")
extern enum abstract MissingMappingAction(Int) {
	var Error = 3;
	var Ignore = 2;
	var Passthrough = 1;
}
