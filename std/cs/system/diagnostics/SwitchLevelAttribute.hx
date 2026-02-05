package cs.system.diagnostics;

/** Identifies the level type for a switch. */
@:native("System.Diagnostics.SwitchLevelAttribute")
extern class SwitchLevelAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the type that determines whether a trace should be written.
	 * @return The  that determines whether a trace should be written.
	 */
	var SwitchLevelType(default, default):cs.system.Type;
	function new(switchLevelType:cs.system.Type):Void;
}
