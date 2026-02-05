package cs.system.buffers;

/** Represents a standard format string without using an actual string. */
@:native("System.Buffers.StandardFormat")
extern class StandardFormat extends cs.system.ValueType {
	/** Defines the maximum valid precision value. */
	static var MaxPrecision(default, never):cs.UInt8;
	/** Indicates that a format doesn't use a precision or that the precision is unspecified. */
	static var NoPrecision(default, never):cs.UInt8;
	/**
	 * Gets a value that indicates whether a format has a defined precision.
	 * @return if the format has a precision other than ; otherwise, .
	 */
	var HasPrecision(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current instance is a default format.
	 * @return if the current instance is a default format; otherwise, .
	 */
	var IsDefault(default, never):Bool;
	/**
	 * Gets the precision component of the format.
	 * @return The precision component, which can be , or can range from 0 to 9.
	 */
	var Precision(default, never):cs.UInt8;
	/**
	 * Gets the character component of the format.
	 * @return The character component of the format.
	 */
	var Symbol(default, never):cs.Char16;
	function new(symbol:cs.Char16, ?precision:cs.UInt8):Void;
	/**
	 * Returns a value that indicates whether two  instances are equal.
	 * @param left The first format to compare.
	 * @param right The second format to compare.
	 * @return if the two instances are equal; otherwise, .
	 */
	static function op_Equality(left:cs.system.buffers.StandardFormat, right:cs.system.buffers.StandardFormat):Bool;
	/**
	 * Converts a character to a  instance using  precision.
	 * @param symbol The character to convert to a  value.
	 * @return A format with a  property equal to  and a  property equal to .
	 */
	static function op_Implicit(symbol:cs.Char16):cs.system.buffers.StandardFormat;
	/**
	 * Determines whether two  instances are unequal.
	 * @param left The first format to compare.
	 * @param right The second format to compare.
	 * @return if the two formats are unequal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.buffers.StandardFormat, right:cs.system.buffers.StandardFormat):Bool;
	@:overload(function(format:cs.system.ReadOnlySpan<cs.Char16>):cs.system.buffers.StandardFormat {})
	/**
	 * Converts a  into a  instance using  precision.
	 * @param format A read-only span that contains the character to parse.
	 * @return A value whose  property value is the character in  and whose  property
	 * value is .
	 */
	static function Parse(format:String):cs.system.buffers.StandardFormat;
	static function TryParse(format:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.buffers.StandardFormat>):Bool;
	@:overload(function(other:cs.system.buffers.StandardFormat):Bool {})
	/**
	 * Returns a value that indicates whether the specified  is equal to the current
	 * instance.
	 * @param other 
	 * @return if the two instances are equal; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the string representation of this format.
	 * @return The string representation of this format.
	 */
	function ToString():String;
}
