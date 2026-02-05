package cs.system.diagnostics;

/** Determines if and how a member is displayed in the debugger variable windows. This class cannot be inherited. */
@:native("System.Diagnostics.DebuggerBrowsableAttribute")
extern class DebuggerBrowsableAttribute extends cs.system.Attribute {
	/**
	 * Gets the display state for the attribute.
	 * @return One of the  values.
	 */
	var State(default, never):cs.system.diagnostics.DebuggerBrowsableState;
	function new(state:cs.system.diagnostics.DebuggerBrowsableState):Void;
}
