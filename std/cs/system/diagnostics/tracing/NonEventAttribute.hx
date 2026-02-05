package cs.system.diagnostics.tracing;

/** Identifies a method that is not generating an event. */
@:native("System.Diagnostics.Tracing.NonEventAttribute")
extern class NonEventAttribute extends cs.system.Attribute {
	function new():Void;
}
