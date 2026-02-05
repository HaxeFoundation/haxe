package cs.system.diagnostics;

/** Determines how a class or field is displayed in the debugger variable windows. */
@:native("System.Diagnostics.DebuggerDisplayAttribute")
extern class DebuggerDisplayAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets the name to display in the debugger variable windows.
	 * @return The name to display in the debugger variable windows.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the type of the attribute's target.
	 * @return The attribute's target type.
	 */
	var Target(default, default):cs.system.Type;
	/**
	 * Gets or sets the type name of the attribute's target.
	 * @return The name of the attribute's target type.
	 */
	var TargetTypeName(default, default):String;
	/**
	 * Gets or sets the string to display in the type column of the debugger variable
	 * windows.
	 * @return The string to display in the type column of the debugger variable
	 * windows.
	 */
	var Type(default, default):String;
	/**
	 * Gets the string to display in the value column of the debugger variable windows.
	 * @return The string to display in the value column of the debugger variable.
	 */
	var Value(default, never):String;
	function new(value:String):Void;
}
