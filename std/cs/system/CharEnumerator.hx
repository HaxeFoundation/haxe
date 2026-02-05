package cs.system;

/** Supports iterating over a  object and reading its individual characters. This class cannot be inherited. */
@:native("System.CharEnumerator")
extern class CharEnumerator {
	/**
	 * Gets the currently referenced character in the string enumerated by this 
	 * object.
	 * @return The Unicode character currently referenced by this  object.
	 */
	var Current(default, never):cs.Char16;
	/**
	 * Creates a copy of the current  object.
	 * @return An  that is a copy of the current  object.
	 */
	function Clone():Dynamic;
	/** Releases all resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Increments the internal index of the current  object to the next character of
	 * the enumerated string.
	 * @return if the index is successfully incremented and within the enumerated
	 * string; otherwise, .
	 */
	function MoveNext():Bool;
	/** Initializes the index to a position logically before the first character of the enumerated string. */
	function Reset():Void;
}
