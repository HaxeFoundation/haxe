package cs.system;

/** Represents a 16-bit unsigned integer. */
@:native("System.UInt16")
extern class UInt16 extends cs.system.ValueType {
	/** Represents the largest possible value of . This field is constant. */
	static var MaxValue(default, never):cs.UInt16;
	/** Represents the smallest possible value of . This field is constant. */
	static var MinValue(default, never):cs.UInt16;
	@:overload(function(s:String):cs.UInt16 {})
	@:overload(function(s:String, style:cs.system.globalization.NumberStyles):cs.UInt16 {})
	@:overload(function(s:String, provider:cs.system.IFormatProvider):cs.UInt16 {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, ?style:cs.system.globalization.NumberStyles, ?provider:cs.system.IFormatProvider):cs.UInt16 {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 */
	static function Parse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider):cs.UInt16;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.UInt16>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.UInt16>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.UInt16>):Bool {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 * @param result 
	 */
	static function TryParse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.UInt16>):Bool;
	@:overload(function(value:Dynamic):Int {})
	/**
	 * Compares this instance to a specified object and returns an indication of their
	 * relative values.
	 * @param value An object to compare, or .
	 * @return A signed number indicating the relative values of this instance and .
	 * Return Value Description Less than zero This instance is less than . Zero This
	 * instance is equal to . Greater than zero This instance is greater than . -or- is
	 * .
	 */
	function CompareTo(value:cs.UInt16):Int;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value indicating whether this instance is equal to a specified object.
	 * @param obj An object to compare to this instance.
	 * @return if  is an instance of  and equals the value of this instance; otherwise,
	 * .
	 */
	function Equals(obj:cs.UInt16):Bool;
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
	 * @return The string representation of the value of this instance, which consists
	 * of a sequence of digits ranging from 0 to 9, without a sign or leading zeros.
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
