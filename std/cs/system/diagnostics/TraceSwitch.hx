package cs.system.diagnostics;

/** Provides a multilevel switch to control tracing and debug output without recompiling your code. */
@:native("System.Diagnostics.TraceSwitch")
extern class TraceSwitch extends cs.system.diagnostics.Switch {
	/**
	 * Gets or sets the trace level that determines the messages the switch allows.
	 * @return One of the  values that specifies the level of messages that are allowed
	 * by the switch.
	 */
	var Level(default, default):cs.system.diagnostics.TraceLevel;
	/**
	 * Gets a value indicating whether the switch allows error-handling messages.
	 * @return if the  property is set to , , , or ; otherwise, .
	 */
	var TraceError(default, never):Bool;
	/**
	 * Gets a value indicating whether the switch allows informational messages.
	 * @return if the  property is set to  or ; otherwise, .
	 */
	var TraceInfo(default, never):Bool;
	/**
	 * Gets a value indicating whether the switch allows all messages.
	 * @return if the  property is set to ; otherwise, .
	 */
	var TraceVerbose(default, never):Bool;
	/**
	 * Gets a value indicating whether the switch allows warning messages.
	 * @return if the  property is set to , , or ; otherwise, .
	 */
	var TraceWarning(default, never):Bool;
	@:overload(function(displayName:String, description:String):Void {})
	function new(displayName:String, description:String, defaultSwitchValue:String):Void;
}
