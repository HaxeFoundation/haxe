package cs.system;

/** Represents an 8-bit signed integer. */
@:native("System.SByte")
extern class SByte extends cs.system.ValueType {
	/** Represents the largest possible value of . This field is constant. */
	static var MaxValue(default, never):cs.Int8;
	/** Represents the smallest possible value of . This field is constant. */
	static var MinValue(default, never):cs.Int8;
	@:overload(function(s:String):cs.Int8 {})
	@:overload(function(s:String, style:cs.system.globalization.NumberStyles):cs.Int8 {})
	@:overload(function(s:String, provider:cs.system.IFormatProvider):cs.Int8 {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, ?style:cs.system.globalization.NumberStyles, ?provider:cs.system.IFormatProvider):cs.Int8 {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 */
	static function Parse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider):cs.Int8;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.Int8>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.Int8>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.Int8>):Bool {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 * @param result 
	 */
	static function TryParse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.Int8>):Bool;
	@:overload(function(obj:Dynamic):Int {})
	/**
	 * Compares this instance to a specified object and returns an indication of their
	 * relative values.
	 * @param obj An object to compare, or .
	 * @return A signed number indicating the relative values of this instance and .
	 * Return Value Description Less than zero This instance is less than . Zero This
	 * instance is equal to . Greater than zero This instance is greater than . -or- is
	 * .
	 */
	function CompareTo(value:cs.Int8):Int;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param obj An object to compare with this instance.
	 * @return if  is an instance of  and equals the value of this instance; otherwise,
	 * .
	 */
	function Equals(obj:cs.Int8):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
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
	 * Converts the numeric value of this instance to its equivalent string
	 * representation.
	 * @return The string representation of the value of this instance, consisting of a
	 * negative sign if the value is negative, and a sequence of digits ranging from 0
	 * to 9 with no leading zeroes.
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
