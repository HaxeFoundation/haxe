package cs.system.collections.specialized;

/** Provides a simple structure that stores Boolean values and small integers in 32 bits of memory. */
@:native("System.Collections.Specialized.BitVector32")
extern class BitVector32 extends cs.system.ValueType {
	/**
	 * Gets the value of the  as an integer.
	 * @return The value of the  as an integer.
	 */
	var Data(default, never):Int;
	@:overload(function(index0:cs.system.collections.specialized.BitVector32_Section):Int {})
	@:native("get_Item")
	function get_Item(index0:Int):Bool;
	@:overload(function(index0:cs.system.collections.specialized.BitVector32_Section, value:Int):Void {})
	@:native("set_Item")
	function set_Item(index0:Int, value:Bool):Void;
	@:overload(function(value:cs.system.collections.specialized.BitVector32):Void {})
	function new(data:Int):Void;
	@:overload(function():Int {})
	/**
	 * Creates the first mask in a series of masks that can be used to retrieve
	 * individual bits in a  that is set up as bit flags.
	 * @return A mask that isolates the first bit flag in the .
	 */
	static function CreateMask(previous:Int):Int;
	@:overload(function(maxValue:cs.Int16):cs.system.collections.specialized.BitVector32_Section {})
	/**
	 * Creates the first  in a series of sections that contain small integers.
	 * @param maxValue A 16-bit signed integer that specifies the maximum value for the
	 * new .
	 * @return A  that can hold a number from zero to .
	 */
	static function CreateSection(maxValue:cs.Int16, previous:cs.system.collections.specialized.BitVector32_Section):cs.system.collections.specialized.BitVector32_Section;
	/**
	 * Returns a string that represents the current .
	 * @return A string that represents the current .
	 */
	static function ToString(value:cs.system.collections.specialized.BitVector32):String;
	/**
	 * Determines whether the specified object is equal to the .
	 * @param o The object to compare with the current .
	 * @return if the specified object is equal to the ; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	/**
	 * Serves as a hash function for the .
	 * @return A hash code for the .
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string that represents the current .
	 * @return A string that represents the current .
	 */
	function ToString():String;
}
