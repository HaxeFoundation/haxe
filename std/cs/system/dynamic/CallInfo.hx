package cs.system.dynamic;

/** Describes arguments in the dynamic binding process. */
@:native("System.Dynamic.CallInfo")
extern class CallInfo {
	/**
	 * The number of arguments.
	 * @return The number of arguments.
	 */
	var ArgumentCount(default, never):Int;
	/**
	 * The argument names.
	 * @return The read-only collection of argument names.
	 */
	var ArgumentNames(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<String>;
	@:overload(function(argCount:Int, argNames:cs.system.collections.generic.IEnumerable<String>):Void {})
	function new(argCount:Int, argNames:cs.NativeArray<String>):Void;
	/**
	 * Determines whether the specified CallInfo instance is considered equal to the
	 * current.
	 * @param obj The instance of  to compare with the current instance.
	 * @return true if the specified instance is equal to the current one otherwise,
	 * false.
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Serves as a hash function for the current .
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
