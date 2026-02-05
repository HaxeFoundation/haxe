package cs.system.globalization;

/** Represents the result of mapping a string to its sort key. */
@:native("System.Globalization.SortKey")
extern class SortKey {
	/**
	 * Gets the byte array representing the current  object.
	 * @return A byte array representing the current  object.
	 */
	var KeyData(default, never):cs.NativeArray<cs.UInt8>;
	/**
	 * Gets the original string used to create the current  object.
	 * @return The original string used to create the current  object.
	 */
	var OriginalString(default, never):String;
	/**
	 * Compares two sort keys.
	 * @param sortkey1 The first sort key to compare.
	 * @param sortkey2 The second sort key to compare.
	 * @return A signed integer that indicates the relationship between  and . Value
	 * Condition Less than zero is less than . Zero is equal to . Greater than zero is
	 * greater than .
	 */
	static function Compare(sortkey1:cs.system.globalization.SortKey, sortkey2:cs.system.globalization.SortKey):Int;
	/**
	 * Determines whether the specified object is equal to the current  object.
	 * @param value The object to compare with the current  object.
	 * @return if the  parameter is equal to the current  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Serves as a hash function for the current  object that is suitable for hashing
	 * algorithms and data structures such as a hash table.
	 * @return A hash code for the current  object.
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current  object.
	 * @return A string that represents the current  object.
	 */
	function ToString():String;
}
