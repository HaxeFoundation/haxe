package cs.system;

/** Represents a decimal floating-point number. */
@:native("System.Decimal")
extern class Decimal extends cs.system.ValueType {
	/** Represents the largest possible value of . This field is constant and read-only. */
	static var MaxValue(default, never):cs.system.Decimal;
	/** Represents the number negative one (-1). */
	static var MinusOne(default, never):cs.system.Decimal;
	/** Represents the smallest possible value of . This field is constant and read-only. */
	static var MinValue(default, never):cs.system.Decimal;
	/** Represents the number one (1). */
	static var One(default, never):cs.system.Decimal;
	/** Represents the number zero (0). */
	static var Zero(default, never):cs.system.Decimal;
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(bits:cs.NativeArray<Int>):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	function new(lo:Int, mid:Int, hi:Int, isNegative:Bool, scale:cs.UInt8):Void;
	/**
	 * Adds two specified  values.
	 * @param d1 The first value to add.
	 * @param d2 The second value to add.
	 * @return The sum of  and .
	 */
	static function Add(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Returns the smallest integral value that is greater than or equal to the
	 * specified decimal number.
	 * @param d A decimal number.
	 * @return The smallest integral value that is greater than or equal to the 
	 * parameter. Note that this method returns a  instead of an integral type.
	 */
	static function Ceiling(d:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Compares two specified  values.
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return A signed number indicating the relative values of  and . Return value
	 * Meaning Less than zero is less than . Zero and  are equal. Greater than zero is
	 * greater than .
	 */
	static function Compare(d1:cs.system.Decimal, d2:cs.system.Decimal):Int;
	/**
	 * Divides two specified  values.
	 * @param d1 The dividend.
	 * @param d2 The divisor.
	 * @return The result of dividing  by .
	 */
	static function Divide(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Returns a value indicating whether this instance and a specified  object
	 * represent the same value.
	 * @param value An object to compare to this instance.
	 * @return if  is equal to this instance; otherwise, .
	 */
	static function Equals(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	/**
	 * Rounds a specified  number to the closest integer toward negative infinity.
	 * @param d The value to round.
	 * @return If  has a fractional part, the next whole  number toward negative
	 * infinity that is less than . -or- If  doesn't have a fractional part,  is
	 * returned unchanged. Note that the method returns an integral value of type .
	 */
	static function Floor(d:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Converts the specified 64-bit signed integer, which contains an OLE Automation
	 * Currency value, to the equivalent  value.
	 * @param cy An OLE Automation Currency value.
	 * @return A  that contains the equivalent of .
	 */
	static function FromOACurrency(cy:haxe.Int64):cs.system.Decimal;
	/**
	 * Converts the value of a specified instance of  to its equivalent binary
	 * representation.
	 * @param d The value to convert.
	 * @return A 32-bit signed integer array with four elements that contain the binary
	 * representation of .
	 */
	static function GetBits(d:cs.system.Decimal):cs.NativeArray<Int>;
	/**
	 * Multiplies two specified  values.
	 * @param d1 The multiplicand.
	 * @param d2 The multiplier.
	 * @return The result of multiplying  and .
	 */
	static function Multiply(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Returns the result of multiplying the specified  value by negative one.
	 * @param d The value to negate.
	 * @return A decimal number with the value of , but the opposite sign. -or- Zero,
	 * if  is zero.
	 */
	static function Negate(d:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Adds two specified  values.
	 * @param d1 The first value to add.
	 * @param d2 The second value to add.
	 * @return The result of adding  and .
	 */
	static function op_Addition(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Decrements the  operand by one.
	 * @param d The value to decrement.
	 * @return The value of  decremented by 1.
	 */
	static function op_Decrement(d:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Divides two specified  values.
	 * @param d1 The dividend.
	 * @param d2 The divisor.
	 * @return The result of dividing  by .
	 */
	static function op_Division(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Returns a value that indicates whether two  values are equal.
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return if  and  are equal; otherwise, .
	 */
	static function op_Equality(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	@:overload(function(value:cs.system.Decimal):cs.UInt8 {})
	@:overload(function(value:cs.system.Decimal):cs.Char16 {})
	@:overload(function(value:cs.system.Decimal):Float {})
	@:overload(function(value:cs.system.Decimal):cs.Int16 {})
	@:overload(function(value:cs.system.Decimal):Int {})
	@:overload(function(value:cs.system.Decimal):haxe.Int64 {})
	@:overload(function(value:cs.system.Decimal):cs.Int8 {})
	@:overload(function(value:cs.system.Decimal):Single {})
	@:overload(function(value:cs.system.Decimal):cs.UInt16 {})
	@:overload(function(value:cs.system.Decimal):cs.UInt {})
	@:overload(function(value:cs.system.Decimal):cs.UInt64 {})
	@:overload(function(value:Float):cs.system.Decimal {})
	/**
	 * Defines an explicit conversion of a  to an 8-bit unsigned integer.
	 * @param value The value to convert.
	 * @return An 8-bit unsigned integer that represents the converted .
	 */
	static function op_Explicit(value:Single):cs.system.Decimal;
	/**
	 * Returns a value indicating whether a specified  is greater than another
	 * specified .
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return if  is greater than ; otherwise, .
	 */
	static function op_GreaterThan(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	/**
	 * Returns a value indicating whether a specified  is greater than or equal to
	 * another specified .
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return if  is greater than or equal to ; otherwise, .
	 */
	static function op_GreaterThanOrEqual(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	@:overload(function(value:cs.UInt8):cs.system.Decimal {})
	@:overload(function(value:cs.Char16):cs.system.Decimal {})
	@:overload(function(value:cs.Int16):cs.system.Decimal {})
	@:overload(function(value:Int):cs.system.Decimal {})
	@:overload(function(value:haxe.Int64):cs.system.Decimal {})
	@:overload(function(value:cs.Int8):cs.system.Decimal {})
	@:overload(function(value:cs.UInt16):cs.system.Decimal {})
	@:overload(function(value:cs.UInt):cs.system.Decimal {})
	/**
	 * Defines an implicit conversion of an 8-bit unsigned integer to a .
	 * @param value The 8-bit unsigned integer to convert.
	 * @return The converted 8-bit unsigned integer.
	 */
	static function op_Implicit(value:cs.UInt64):cs.system.Decimal;
	/**
	 * Increments the  operand by 1.
	 * @param d The value to increment.
	 * @return The value of  incremented by 1.
	 */
	static function op_Increment(d:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Returns a value that indicates whether two  objects have different values.
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	/**
	 * Returns a value indicating whether a specified  is less than another specified .
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return if  is less than ; otherwise, .
	 */
	static function op_LessThan(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	/**
	 * Returns a value indicating whether a specified  is less than or equal to another
	 * specified .
	 * @param d1 The first value to compare.
	 * @param d2 The second value to compare.
	 * @return if  is less than or equal to ; otherwise, .
	 */
	static function op_LessThanOrEqual(d1:cs.system.Decimal, d2:cs.system.Decimal):Bool;
	/**
	 * Returns the remainder resulting from dividing two specified  values.
	 * @param d1 The dividend.
	 * @param d2 The divisor.
	 * @return The remainder resulting from dividing  by .
	 */
	static function op_Modulus(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Multiplies two specified  values.
	 * @param d1 The first value to multiply.
	 * @param d2 The second value to multiply.
	 * @return The result of multiplying  by .
	 */
	static function op_Multiply(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Subtracts two specified  values.
	 * @param d1 The minuend.
	 * @param d2 The subtrahend.
	 * @return The result of subtracting  from .
	 */
	static function op_Subtraction(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Negates the value of the specified  operand.
	 * @param d The value to negate.
	 * @return The result of  multiplied by negative one (-1).
	 */
	static function op_UnaryNegation(d:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Returns the value of the  operand (the sign of the operand is unchanged).
	 * @param d The operand to return.
	 * @return The value of the operand, .
	 */
	static function op_UnaryPlus(d:cs.system.Decimal):cs.system.Decimal;
	@:overload(function(s:String):cs.system.Decimal {})
	@:overload(function(s:String, style:cs.system.globalization.NumberStyles):cs.system.Decimal {})
	@:overload(function(s:String, provider:cs.system.IFormatProvider):cs.system.Decimal {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, ?style:cs.system.globalization.NumberStyles, ?provider:cs.system.IFormatProvider):cs.system.Decimal {})
	/**
	 * @param s 
	 * @param style 
	 * @param provider 
	 */
	static function Parse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider):cs.system.Decimal;
	/**
	 * Computes the remainder after dividing two  values.
	 * @param d1 The dividend.
	 * @param d2 The divisor.
	 * @return The remainder after dividing  by .
	 */
	static function Remainder(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	@:overload(function(d:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(d:cs.system.Decimal, decimals:Int):cs.system.Decimal {})
	@:overload(function(d:cs.system.Decimal, mode:cs.system.MidpointRounding):cs.system.Decimal {})
	/**
	 * Rounds a decimal value to the nearest integer.
	 * @param d A decimal number to round.
	 * @return The integer that is nearest to the  parameter. If  is halfway between
	 * two integers, one of which is even and the other odd, the even number is
	 * returned.
	 */
	static function Round(d:cs.system.Decimal, decimals:Int, mode:cs.system.MidpointRounding):cs.system.Decimal;
	/**
	 * Subtracts one specified  value from another.
	 * @param d1 The minuend.
	 * @param d2 The subtrahend.
	 * @return The result of subtracting  from .
	 */
	static function Subtract(d1:cs.system.Decimal, d2:cs.system.Decimal):cs.system.Decimal;
	/**
	 * Converts the value of the specified  to the equivalent 8-bit unsigned integer.
	 * @param value The decimal number to convert.
	 * @return An 8-bit unsigned integer equivalent to .
	 */
	static function ToByte(value:cs.system.Decimal):cs.UInt8;
	/**
	 * Converts the value of the specified  to the equivalent double-precision
	 * floating-point number.
	 * @param d The decimal number to convert.
	 * @return A double-precision floating-point number equivalent to .
	 */
	static function ToDouble(d:cs.system.Decimal):Float;
	/**
	 * Converts the value of the specified  to the equivalent 16-bit signed integer.
	 * @param value The decimal number to convert.
	 * @return A 16-bit signed integer equivalent to .
	 */
	static function ToInt16(value:cs.system.Decimal):cs.Int16;
	/**
	 * Converts the value of the specified  to the equivalent 32-bit signed integer.
	 * @param d The decimal number to convert.
	 * @return A 32-bit signed integer equivalent to the value of .
	 */
	static function ToInt32(d:cs.system.Decimal):Int;
	/**
	 * Converts the value of the specified  to the equivalent 64-bit signed integer.
	 * @param d The decimal number to convert.
	 * @return A 64-bit signed integer equivalent to the value of .
	 */
	static function ToInt64(d:cs.system.Decimal):haxe.Int64;
	/**
	 * Converts the specified  value to the equivalent OLE Automation Currency value,
	 * which is contained in a 64-bit signed integer.
	 * @param value The decimal number to convert.
	 * @return A 64-bit signed integer that contains the OLE Automation equivalent of .
	 */
	static function ToOACurrency(value:cs.system.Decimal):haxe.Int64;
	/**
	 * Converts the value of the specified  to the equivalent 8-bit signed integer.
	 * @param value The decimal number to convert.
	 * @return An 8-bit signed integer equivalent to .
	 */
	static function ToSByte(value:cs.system.Decimal):cs.Int8;
	/**
	 * Converts the value of the specified  to the equivalent single-precision
	 * floating-point number.
	 * @param d The decimal number to convert.
	 * @return A single-precision floating-point number equivalent to the value of .
	 */
	static function ToSingle(d:cs.system.Decimal):Single;
	/**
	 * Converts the value of the specified  to the equivalent 16-bit unsigned integer.
	 * @param value The decimal number to convert.
	 * @return A 16-bit unsigned integer equivalent to the value of .
	 */
	static function ToUInt16(value:cs.system.Decimal):cs.UInt16;
	/**
	 * Converts the value of the specified  to the equivalent 32-bit unsigned integer.
	 * @param d The decimal number to convert.
	 * @return A 32-bit unsigned integer equivalent to the value of .
	 */
	static function ToUInt32(d:cs.system.Decimal):cs.UInt;
	/**
	 * Converts the value of the specified  to the equivalent 64-bit unsigned integer.
	 * @param d The decimal number to convert.
	 * @return A 64-bit unsigned integer equivalent to the value of .
	 */
	static function ToUInt64(d:cs.system.Decimal):cs.UInt64;
	/**
	 * Returns the integral digits of the specified ; any fractional digits are
	 * discarded.
	 * @param d The decimal number to truncate.
	 * @return The result of  rounded toward zero, to the nearest whole number.
	 */
	static function Truncate(d:cs.system.Decimal):cs.system.Decimal;
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.Decimal>):Bool {})
	@:overload(function(s:String, result:cs.Ref<cs.system.Decimal>):Bool {})
	@:overload(function(s:cs.system.ReadOnlySpan<cs.Char16>, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.system.Decimal>):Bool {})
	/**
	 * @param s 
	 * @param result 
	 */
	static function TryParse(s:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.system.Decimal>):Bool;
	@:overload(function(value:cs.system.Decimal):Int {})
	/**
	 * Compares this instance to a specified  object and returns a comparison of their
	 * relative values.
	 * @param value The object to compare with this instance.
	 * @return A signed number indicating the relative values of this instance and .
	 * Return value Meaning Less than zero This instance is less than . Zero This
	 * instance is equal to . Greater than zero This instance is greater than .
	 */
	function CompareTo(value:Dynamic):Int;
	@:overload(function(value:cs.system.Decimal):Bool {})
	/**
	 * Returns a value indicating whether this instance and a specified  object
	 * represent the same value.
	 * @param value An object to compare to this instance.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the  for value type .
	 * @return The enumerated constant .
	 */
	function GetTypeCode():cs.system.TypeCode;
	@:overload(function():String {})
	@:overload(function(provider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the numeric value of this instance to its equivalent string
	 * representation.
	 * @return A string that represents the value of this instance.
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
