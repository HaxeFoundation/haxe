package cs.system.diagnostics.tracing;

/** Specifies a property should be ignored when writing an event type with the  method. */
@:native("System.Diagnostics.Tracing.EventIgnoreAttribute")
extern class EventIgnoreAttribute extends cs.system.Attribute {
	function new():Void;
}
