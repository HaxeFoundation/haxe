package cs.system;

/** Represents an 8-bit unsigned integer. */
@:native("System.Byte")
extern class Byte extends cs.system.ValueType {
	/** Represents the largest possible value of a . This field is constant. */
	static var MaxValue(default, never):cs.UInt8;
	/** Represents the smallest possible value of a . This field is constant. */
	static var MinValue(default, never):cs.UInt8;
	@:overload(function(s:String):cs.UInt8 {})
	@:overload(function(s:String, style:cs.system.globalization.NumberStyles):cs.UInt8 {})
	@:overload(function(s:String, provider:cs.system.IFormatProvider):cs.UInt8 {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, ?style:cs.system.globalization.NumberStyles, ?provider:cs.system.IFormatProvider):cs.UInt8 {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 */
	static function Parse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider):cs.UInt8;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.UInt8>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.UInt8>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.UInt8>):Bool {})
	/**
	 * @param s 
	 * @param result 
	 */
	static function TryParse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.UInt8>):Bool;
	@:overload(function(value:cs.UInt8):Int {})
	/**
	 * Compares this instance to a specified 8-bit unsigned integer and returns an
	 * indication of their relative values.
	 * @param value An 8-bit unsigned integer to compare.
	 * @return A signed integer that indicates the relative order of this instance and
	 * . Return Value Description Less than zero This instance is less than . Zero This
	 * instance is equal to . Greater than zero This instance is greater than .
	 */
	function CompareTo(value:Dynamic):Int;
	@:overload(function(obj:cs.UInt8):Bool {})
	/**
	 * Returns a value indicating whether this instance and a specified  object
	 * represent the same value.
	 * @param obj An object to compare to this instance.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns the  for value type .
	 * @return The enumerated constant, .
	 */
	function GetTypeCode():cs.system.TypeCode;
	@:overload(function():String {})
	@:overload(function(provider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the value of the current  object to its equivalent string
	 * representation.
	 * @return The string representation of the value of this object, which consists of
	 * a sequence of digits that range from 0 to 9 with no leading zeroes.
	 */
	function ToString(format:String, provider:cs.system.IFormatProvider):String;
	/**
	 * @param destination 
	 * @param charsWritten 
	 * @param format 
	 * @param provider 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?format:cs.system.ReadOnlySpan<cs.Char16>, ?provider:cs.system.IFormatProvider):Bool;
}
