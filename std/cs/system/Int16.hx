package cs.system;

/** Represents a 16-bit signed integer. */
@:native("System.Int16")
extern class Int16 extends cs.system.ValueType {
	/** Represents the largest possible value of an . This field is constant. */
	static var MaxValue(default, never):cs.Int16;
	/** Represents the smallest possible value of . This field is constant. */
	static var MinValue(default, never):cs.Int16;
	@:overload(function(s:String):cs.Int16 {})
	@:overload(function(s:String, style:cs.system.globalization.NumberStyles):cs.Int16 {})
	@:overload(function(s:String, provider:cs.system.IFormatProvider):cs.Int16 {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, ?style:cs.system.globalization.NumberStyles, ?provider:cs.system.IFormatProvider):cs.Int16 {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 */
	static function Parse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider):cs.Int16;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.Int16>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.Int16>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.Int16>):Bool {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 * @param result 
	 */
	static function TryParse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.Int16>):Bool;
	@:overload(function(value:cs.Int16):Int {})
	/**
	 * Compares this instance to a specified 16-bit signed integer and returns an
	 * integer that indicates whether the value of this instance is less than, equal
	 * to, or greater than the value of the specified 16-bit signed integer.
	 * @param value An integer to compare.
	 * @return A signed number indicating the relative values of this instance and .
	 * Return Value Description Less than zero This instance is less than . Zero This
	 * instance is equal to . Greater than zero This instance is greater than .
	 */
	function CompareTo(value:Dynamic):Int;
	@:overload(function(obj:cs.Int16):Bool {})
	/**
	 * Returns a value indicating whether this instance is equal to a specified  value.
	 * @param obj An  value to compare to this instance.
	 * @return if  has the same value as this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
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
	 * minus sign if the value is negative, and a sequence of digits ranging from 0 to
	 * 9 with no leading zeroes.
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
