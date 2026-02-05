package cs.system;

/** Represent a type can be used to index a collection either from the start or the end. */
@:native("System.Index")
extern class Index extends cs.system.ValueType {
	/**
	 * Gets an  that points beyond the last element.
	 * @return an  that points beyond the last element.
	 */
	static var End(default, never):cs.system.Index;
	/**
	 * Gets an  that points to the first element of a collection.
	 * @return An instance that points to the first element of a collection.
	 */
	static var Start(default, never):cs.system.Index;
	/**
	 * Gets a value that indicates whether the index is from the start or the end.
	 * @return if the Index is from the end; otherwise, <see. langword="false"></see.>.
	 */
	var IsFromEnd(default, never):Bool;
	/**
	 * Gets the index value.
	 * @return The index value.
	 */
	var Value(default, never):Int;
	function new(value:Int, ?fromEnd:Bool):Void;
	/**
	 * Creates an  from the end of a collection at a specified index position.
	 * @param value The index value from the end of a collection.
	 * @return The Index value.
	 */
	static function FromEnd(value:Int):cs.system.Index;
	/**
	 * Create an  from the specified index at the start of a collection.
	 * @param value The index position from the start of a collection.
	 * @return The index value.
	 */
	static function FromStart(value:Int):cs.system.Index;
	/**
	 * Converts integer number to an Index.
	 * @param value The integer to convert.
	 * @return An Index representing the integer.
	 */
	static function op_Implicit(value:Int):cs.system.Index;
	@:overload(function(other:cs.system.Index):Bool {})
	/**
	 * Returns a value that indicates whether the current object is equal to another 
	 * object.
	 * @param other The object to compare with this instance.
	 * @return if the current Index object is equal to ;  otherwise.
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Calculates the offset from the start using the given collection length.
	 * @param length The length of the collection that the Index will be used with.
	 * Must be a positive value.
	 * @return The offset.
	 */
	function GetOffset(length:Int):Int;
	/**
	 * Returns the string representation of the current  instance.
	 * @return The string representation of the .
	 */
	function ToString():String;
}
