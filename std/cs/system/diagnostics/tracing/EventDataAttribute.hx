package cs.system.diagnostics.tracing;

/** Specifies a type to be passed to the  method. */
@:native("System.Diagnostics.Tracing.EventDataAttribute")
extern class EventDataAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the name to apply to an event if the event type or property is not
	 * explicitly named.
	 * @return The name to apply to the event or property.
	 */
	var Name(default, default):String;
	function new():Void;
}
