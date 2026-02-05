package cs.system.diagnostics;

/** Modifies code generation for runtime just-in-time (JIT) debugging. This class cannot be inherited. */
@:native("System.Diagnostics.DebuggableAttribute")
extern class DebuggableAttribute extends cs.system.Attribute {
	/**
	 * Gets the debugging modes for the attribute.
	 * @return A bitwise combination of the  values describing the debugging mode for
	 * the just-in-time (JIT) compiler. The default is .
	 */
	var DebuggingFlags(default, never):cs.system.diagnostics.DebuggableAttribute_DebuggingModes;
	/**
	 * Gets a value that indicates whether the runtime optimizer is disabled.
	 * @return if the runtime optimizer is disabled; otherwise, .
	 */
	var IsJITOptimizerDisabled(default, never):Bool;
	/**
	 * Gets a value that indicates whether the runtime will track information during
	 * code generation for the debugger.
	 * @return if the runtime will track information during code generation for the
	 * debugger; otherwise, .
	 */
	var IsJITTrackingEnabled(default, never):Bool;
	@:overload(function(modes:cs.system.diagnostics.DebuggableAttribute_DebuggingModes):Void {})
	function new(isJITTrackingEnabled:Bool, isJITOptimizerDisabled:Bool):Void;
}
