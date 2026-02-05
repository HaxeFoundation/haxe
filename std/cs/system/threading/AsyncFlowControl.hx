package cs.system.threading;

/** Provides the functionality to restore the migration, or flow, of the execution context between threads. */
@:native("System.Threading.AsyncFlowControl")
extern class AsyncFlowControl extends cs.system.ValueType {
	/**
	 * Compares two  structures to determine whether they are equal.
	 * @param a An  structure.
	 * @param b An  structure.
	 * @return if the two structures are equal; otherwise, .
	 */
	static function op_Equality(a:cs.system.threading.AsyncFlowControl, b:cs.system.threading.AsyncFlowControl):Bool;
	/**
	 * Compares two  structures to determine whether they are not equal.
	 * @param a An  structure.
	 * @param b An  structure.
	 * @return if the structures are not equal; otherwise, .
	 */
	static function op_Inequality(a:cs.system.threading.AsyncFlowControl, b:cs.system.threading.AsyncFlowControl):Bool;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Determines whether the specified object is equal to the current  structure.
	 * @param obj An object to compare with the current structure.
	 * @return if  is an  structure and is equal to the current  structure; otherwise,
	 * .
	 */
	function Equals(obj:cs.system.threading.AsyncFlowControl):Bool;
	/**
	 * Gets a hash code for the current  structure.
	 * @return A hash code for the current  structure.
	 */
	function GetHashCode():Int;
	/** Restores the flow of the execution context between threads. */
	function Undo():Void;
}
