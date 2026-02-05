package cs.system.reflection;

/** Discovers the attributes of a local variable and provides access to local variable metadata. */
@:native("System.Reflection.LocalVariableInfo")
extern class LocalVariableInfo {
	/**
	 * Gets a  value that indicates whether the object referred to by the local
	 * variable is pinned in memory.
	 * @return if the object referred to by the variable is pinned in memory;
	 * otherwise, .
	 */
	var IsPinned(default, never):Bool;
	/**
	 * Gets the index of the local variable within the method body.
	 * @return An integer value that represents the order of declaration of the local
	 * variable within the method body.
	 */
	var LocalIndex(default, never):Int;
	/**
	 * Gets the type of the local variable.
	 * @return The type of the local variable.
	 */
	var LocalType(default, never):cs.system.Type;
	/**
	 * Returns a user-readable string that describes the local variable.
	 * @return A string that displays information about the local variable, including
	 * the type name, index, and pinned status.
	 */
	function ToString():String;
}
