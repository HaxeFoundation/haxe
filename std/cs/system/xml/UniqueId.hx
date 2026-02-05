package cs.system.xml;

/** A unique identifier optimized for Guids. */
@:native("System.Xml.UniqueId")
extern class UniqueId {
	/**
	 * Gets the length of the string representation of the .
	 * @return The length of the string representation of the .
	 */
	var CharArrayLength(default, never):Int;
	/**
	 * Indicates whether the  is a .
	 * @return if the  is a ; otherwise .
	 */
	var IsGuid(default, never):Bool;
	@:overload(function():Void {})
	@:overload(function(guid:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(guid:cs.system.Guid):Void {})
	@:overload(function(value:String):Void {})
	@:overload(function(guid:cs.NativeArray<cs.UInt8>, offset:Int):Void {})
	function new(chars:cs.NativeArray<cs.Char16>, offset:Int, count:Int):Void;
	/**
	 * Overrides the equality operator to test for equality of two s.
	 * @param id1 The first .
	 * @param id2 The second .
	 * @return if the two s are equal, or are both ;  if they are not equal, or if only
	 * one of them is .
	 */
	static function op_Equality(id1:cs.system.xml.UniqueId, id2:cs.system.xml.UniqueId):Bool;
	/**
	 * Overrides the equality operator to test for inequality of two s.
	 * @param id1 The first .
	 * @param id2 The second .
	 * @return if the overridden equality operator returns ; otherwise .
	 */
	static function op_Inequality(id1:cs.system.xml.UniqueId, id2:cs.system.xml.UniqueId):Bool;
	/**
	 * Tests whether an object equals this .
	 * @param obj The object to compare.
	 * @return if the object equals this ; otherwise .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Creates a hash-code representation of this .
	 * @return An integer hash-code representation of this .
	 */
	function GetHashCode():Int;
	/**
	 * Puts the  value into a  array.
	 * @param chars The  array.
	 * @param offset Position in the  array to start inserting the  value.
	 * @return Number of entries in the  array filled by the  value.
	 */
	function ToCharArray(chars:cs.NativeArray<cs.Char16>, offset:Int):Int;
	/**
	 * Displays the  value in string format.
	 * @return A string representation of the  value.
	 */
	function ToString():String;
	@:overload(function(guid:cs.Ref<cs.system.Guid>):Bool {})
	/**
	 * Tries to get the value of the  as a  and store it in the given byte array at the
	 * specified offset.
	 * @param buffer array that will contain the .
	 * @param offset Position in the  array to start inserting the  value.
	 * @return if the value stored in this instance of  is a ; otherwise .
	 */
	function TryGetGuid(buffer:cs.NativeArray<cs.UInt8>, offset:Int):Bool;
}
