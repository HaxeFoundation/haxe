package cs.system;

/** Represents a range that has start and end indexes. */
@:native("System.Range")
extern class Range extends cs.system.ValueType {
	/**
	 * Gets a  object that starts from the first element to the end.
	 * @return A range from the start to the end.
	 */
	static var All(default, never):cs.system.Range;
	/**
	 * Gets an  that represents the exclusive end index of the range.
	 * @return The end index of the range.
	 */
	var End(default, never):cs.system.Index;
	/**
	 * Gets the inclusive start index of the .
	 * @return The inclusive start index of the range.
	 */
	var Start(default, never):cs.system.Index;
	function new(start:cs.system.Index, end:cs.system.Index):Void;
	/**
	 * Creates a  object starting from the first element in the collection to a
	 * specified end index.
	 * @param end The position of the last element up to which the  object will be
	 * created.
	 * @return A range that starts from the first element to .
	 */
	static function EndAt(end:cs.system.Index):cs.system.Range;
	/**
	 * Returns a new  instance starting from a specified start index to the end of the
	 * collection.
	 * @param start The position of the first element from which the Range will be
	 * created.
	 * @return A range from  to the end of the collection.
	 */
	static function StartAt(start:cs.system.Index):cs.system.Range;
	@:overload(function(value:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether the current instance is equal to a
	 * specified object.
	 * @param value An object to compare with this Range object.
	 * @return if  is of type  and is equal to the current instance; otherwise, .
	 */
	function Equals(other:cs.system.Range):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a  instance with the starting offset and length of a range.
	 * @param length A positive integer that represents the length of the collection
	 * that the range will be used with.
	 * @return The starting offset and length of the range.
	 */
	function GetOffsetAndLength(length:Int):cs.system.ValueTuple_2<Int, Int>;
	/**
	 * Returns the string representation of the current  object.
	 * @return The string representation of the range.
	 */
	function ToString():String;
}
