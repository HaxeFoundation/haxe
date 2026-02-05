package cs.system.diagnostics;

/** Provides a multilevel switch to control tracing and debug output without recompiling your code. */
@:native("System.Diagnostics.SourceSwitch")
extern class SourceSwitch extends cs.system.diagnostics.Switch {
	/**
	 * Gets or sets the level of the switch.
	 * @return One of the  values that represents the event level of the switch.
	 */
	var Level(default, default):cs.system.diagnostics.SourceLevels;
	@:overload(function(name:String):Void {})
	function new(displayName:String, defaultSwitchValue:String):Void;
	/**
	 * Determines if trace listeners should be called, based on the trace event type.
	 * @param eventType One of the  values.
	 * @return if the trace listeners should be called; otherwise, .
	 */
	function ShouldTrace(eventType:cs.system.diagnostics.TraceEventType):Bool;
}
