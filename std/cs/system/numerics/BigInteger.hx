package cs.system.numerics;

/** Represents an arbitrarily large signed integer. */
@:native("System.Numerics.BigInteger")
extern class BigInteger extends cs.system.ValueType {
	/**
	 * Gets a value that represents the number negative one (-1).
	 * @return An integer whose value is negative one (-1).
	 */
	static var MinusOne(default, never):cs.system.numerics.BigInteger;
	/**
	 * Gets a value that represents the number one (1).
	 * @return An object whose value is one (1).
	 */
	static var One(default, never):cs.system.numerics.BigInteger;
	/**
	 * Gets a value that represents the number 0 (zero).
	 * @return An integer whose value is 0 (zero).
	 */
	static var Zero(default, never):cs.system.numerics.BigInteger;
	/**
	 * Indicates whether the value of the current  object is an even number.
	 * @return if the value of the  object is an even number; otherwise, .
	 */
	var IsEven(default, never):Bool;
	/**
	 * Indicates whether the value of the current  object is .
	 * @return if the value of the  object is ; otherwise, .
	 */
	var IsOne(default, never):Bool;
	/**
	 * Indicates whether the value of the current  object is a power of two.
	 * @return if the value of the  object is a power of two; otherwise, .
	 */
	var IsPowerOfTwo(default, never):Bool;
	/**
	 * Indicates whether the value of the current  object is .
	 * @return if the value of the  object is ; otherwise, .
	 */
	var IsZero(default, never):Bool;
	/**
	 * Gets a number that indicates the sign (negative, positive, or zero) of the
	 * current  object.
	 * @return A number that indicates the sign of the  object, as shown in the
	 * following table. Number Description -1 The value of this object is negative. 0
	 * The value of this object is 0 (zero). 1 The value of this object is positive.
	 */
	var Sign(default, never):Int;
	@:overload(function(value:cs.NativeArray<cs.UInt8>):Void {})
	@:overload(function(value:cs.system.Decimal):Void {})
	@:overload(function(value:Float):Void {})
	@:overload(function(value:Int):Void {})
	@:overload(function(value:haxe.Int64):Void {})
	@:overload(function(value:Single):Void {})
	@:overload(function(value:cs.UInt):Void {})
	@:overload(function(value:cs.UInt64):Void {})
	function new(value:cs.system.ReadOnlySpan<cs.UInt8>, ?isUnsigned:Bool, ?isBigEndian:Bool):Void;
	/**
	 * Gets the absolute value of a  object.
	 * @param value A number.
	 * @return The absolute value of .
	 */
	static function Abs(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Adds two  values and returns the result.
	 * @param left The first value to add.
	 * @param right The second value to add.
	 * @return The sum of  and .
	 */
	static function Add(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Compares two  values and returns an integer that indicates whether the first
	 * value is less than, equal to, or greater than the second value.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return A signed integer that indicates the relative values of  and , as shown
	 * in the following table. Value Condition Less than zero is less than . Zero
	 * equals . Greater than zero is greater than .
	 */
	static function Compare(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Int;
	/**
	 * Divides one  value by another and returns the result.
	 * @param dividend The value to be divided.
	 * @param divisor The value to divide by.
	 * @return The quotient of the division.
	 */
	static function Divide(dividend:cs.system.numerics.BigInteger, divisor:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Divides one  value by another, returns the result, and returns the remainder in
	 * an output parameter.
	 * @param dividend The value to be divided.
	 * @param divisor The value to divide by.
	 * @param remainder When this method returns, contains a  value that represents the
	 * remainder from the division. This parameter is passed uninitialized.
	 * @return The quotient of the division.
	 */
	static function DivRem(dividend:cs.system.numerics.BigInteger, divisor:cs.system.numerics.BigInteger, remainder:cs.Ref<cs.system.numerics.BigInteger>):cs.system.numerics.BigInteger;
	/**
	 * Finds the greatest common divisor of two  values.
	 * @param left The first value.
	 * @param right The second value.
	 * @return The greatest common divisor of  and .
	 */
	static function GreatestCommonDivisor(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	@:overload(function(value:cs.system.numerics.BigInteger):Float {})
	/**
	 * Returns the natural (base ) logarithm of a specified number.
	 * @param value The number whose logarithm is to be found.
	 * @return The natural (base ) logarithm of , as shown in the table in the Remarks
	 * section.
	 */
	static function Log(value:cs.system.numerics.BigInteger, baseValue:Float):Float;
	/**
	 * Returns the base 10 logarithm of a specified number.
	 * @param value A number whose logarithm is to be found.
	 * @return The base 10 logarithm of , as shown in the table in the Remarks section.
	 */
	static function Log10(value:cs.system.numerics.BigInteger):Float;
	/**
	 * Returns the larger of two  values.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return The  or  parameter, whichever is larger.
	 */
	static function Max(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Returns the smaller of two  values.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return The  or  parameter, whichever is smaller.
	 */
	static function Min(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Performs modulus division on a number raised to the power of another number.
	 * @param value The number to raise to the  power.
	 * @param exponent The exponent to raise  by.
	 * @param modulus The number by which to divide  raised to the  power.
	 * @return The remainder after dividing exponent by .
	 */
	static function ModPow(value:cs.system.numerics.BigInteger, exponent:cs.system.numerics.BigInteger, modulus:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Returns the product of two  values.
	 * @param left The first number to multiply.
	 * @param right The second number to multiply.
	 * @return The product of the  and  parameters.
	 */
	static function Multiply(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Negates a specified  value.
	 * @param value The value to negate.
	 * @return The result of the  parameter multiplied by negative one (-1).
	 */
	static function Negate(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Adds the values of two specified  objects.
	 * @param left The first value to add.
	 * @param right The second value to add.
	 * @return The sum of  and .
	 */
	static function op_Addition(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Performs a bitwise  operation on two  values.
	 * @param left The first value.
	 * @param right The second value.
	 * @return The result of the bitwise  operation.
	 */
	static function op_BitwiseAnd(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Performs a bitwise  operation on two  values.
	 * @param left The first value.
	 * @param right The second value.
	 * @return The result of the bitwise  operation.
	 */
	static function op_BitwiseOr(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Decrements a  value by 1.
	 * @param value The value to decrement.
	 * @return The value of the  parameter decremented by 1.
	 */
	static function op_Decrement(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Divides a specified  value by another specified  value by using integer
	 * division.
	 * @param dividend The value to be divided.
	 * @param divisor The value to divide by.
	 * @return The integral result of the division.
	 */
	static function op_Division(dividend:cs.system.numerics.BigInteger, divisor:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	@:overload(function(left:haxe.Int64, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:haxe.Int64):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.UInt64):Bool {})
	/**
	 * Returns a value that indicates whether a signed long integer value and a  value
	 * are equal.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if the  and  parameters have the same value; otherwise, .
	 */
	static function op_Equality(left:cs.UInt64, right:cs.system.numerics.BigInteger):Bool;
	/**
	 * Performs a bitwise exclusive  () operation on two  values.
	 * @param left The first value.
	 * @param right The second value.
	 * @return The result of the bitwise  operation.
	 */
	static function op_ExclusiveOr(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	@:overload(function(value:cs.system.Decimal):cs.system.numerics.BigInteger {})
	@:overload(function(value:Float):cs.system.numerics.BigInteger {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.UInt8 {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.system.Decimal {})
	@:overload(function(value:cs.system.numerics.BigInteger):Float {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.Int16 {})
	@:overload(function(value:cs.system.numerics.BigInteger):Int {})
	@:overload(function(value:cs.system.numerics.BigInteger):haxe.Int64 {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.Int8 {})
	@:overload(function(value:cs.system.numerics.BigInteger):Single {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.UInt16 {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.UInt {})
	@:overload(function(value:cs.system.numerics.BigInteger):cs.UInt64 {})
	/**
	 * Defines an explicit conversion of a  object to a  value.
	 * @param value The value to convert to a .
	 * @return An object that contains the value of the  parameter.
	 */
	static function op_Explicit(value:Single):cs.system.numerics.BigInteger;
	@:overload(function(left:haxe.Int64, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:haxe.Int64):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.UInt64):Bool {})
	/**
	 * Returns a value that indicates whether a 64-bit signed integer is greater than a
	 * value.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if  is greater than ; otherwise, .
	 */
	static function op_GreaterThan(left:cs.UInt64, right:cs.system.numerics.BigInteger):Bool;
	@:overload(function(left:haxe.Int64, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:haxe.Int64):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.UInt64):Bool {})
	/**
	 * Returns a value that indicates whether a 64-bit signed integer is greater than
	 * or equal to a  value.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if  is greater than ; otherwise, .
	 */
	static function op_GreaterThanOrEqual(left:cs.UInt64, right:cs.system.numerics.BigInteger):Bool;
	@:overload(function(value:cs.UInt8):cs.system.numerics.BigInteger {})
	@:overload(function(value:cs.Int16):cs.system.numerics.BigInteger {})
	@:overload(function(value:Int):cs.system.numerics.BigInteger {})
	@:overload(function(value:haxe.Int64):cs.system.numerics.BigInteger {})
	@:overload(function(value:cs.Int8):cs.system.numerics.BigInteger {})
	@:overload(function(value:cs.UInt16):cs.system.numerics.BigInteger {})
	@:overload(function(value:cs.UInt):cs.system.numerics.BigInteger {})
	/**
	 * Defines an implicit conversion of an unsigned byte to a  value.
	 * @param value The value to convert to a .
	 * @return An object that contains the value of the  parameter.
	 */
	static function op_Implicit(value:cs.UInt64):cs.system.numerics.BigInteger;
	/**
	 * Increments a  value by 1.
	 * @param value The value to increment.
	 * @return The value of the  parameter incremented by 1.
	 */
	static function op_Increment(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	@:overload(function(left:haxe.Int64, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:haxe.Int64):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.UInt64):Bool {})
	/**
	 * Returns a value that indicates whether a 64-bit signed integer and a  value are
	 * not equal.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.UInt64, right:cs.system.numerics.BigInteger):Bool;
	/**
	 * Shifts a  value a specified number of bits to the left.
	 * @param value The value whose bits are to be shifted.
	 * @param shift The number of bits to shift  to the left.
	 * @return A value that has been shifted to the left by the specified number of
	 * bits.
	 */
	static function op_LeftShift(value:cs.system.numerics.BigInteger, shift:Int):cs.system.numerics.BigInteger;
	@:overload(function(left:haxe.Int64, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:haxe.Int64):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.UInt64):Bool {})
	/**
	 * Returns a value that indicates whether a 64-bit signed integer is less than a 
	 * value.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if  is less than ; otherwise, .
	 */
	static function op_LessThan(left:cs.UInt64, right:cs.system.numerics.BigInteger):Bool;
	@:overload(function(left:haxe.Int64, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:haxe.Int64):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(left:cs.system.numerics.BigInteger, right:cs.UInt64):Bool {})
	/**
	 * Returns a value that indicates whether a 64-bit signed integer is less than or
	 * equal to a  value.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if  is less than or equal to ; otherwise, .
	 */
	static function op_LessThanOrEqual(left:cs.UInt64, right:cs.system.numerics.BigInteger):Bool;
	/**
	 * Returns the remainder that results from division with two specified  values.
	 * @param dividend The value to be divided.
	 * @param divisor The value to divide by.
	 * @return The remainder that results from the division.
	 */
	static function op_Modulus(dividend:cs.system.numerics.BigInteger, divisor:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Multiplies two specified  values.
	 * @param left The first value to multiply.
	 * @param right The second value to multiply.
	 * @return The product of  and .
	 */
	static function op_Multiply(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Returns the bitwise one's complement of a  value.
	 * @param value An integer value.
	 * @return The bitwise one's complement of .
	 */
	static function op_OnesComplement(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Shifts a  value a specified number of bits to the right.
	 * @param value The value whose bits are to be shifted.
	 * @param shift The number of bits to shift  to the right.
	 * @return A value that has been shifted to the right by the specified number of
	 * bits.
	 */
	static function op_RightShift(value:cs.system.numerics.BigInteger, shift:Int):cs.system.numerics.BigInteger;
	/**
	 * Subtracts a  value from another  value.
	 * @param left The value to subtract from (the minuend).
	 * @param right The value to subtract (the subtrahend).
	 * @return The result of subtracting  from .
	 */
	static function op_Subtraction(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Negates a specified BigInteger value.
	 * @param value The value to negate.
	 * @return The result of the  parameter multiplied by negative one (-1).
	 */
	static function op_UnaryNegation(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Returns the value of the  operand. (The sign of the operand is unchanged.)
	 * @param value An integer value.
	 * @return The value of the  operand.
	 */
	static function op_UnaryPlus(value:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	@:overload(function(value:String):cs.system.numerics.BigInteger {})
	@:overload(function(value:String, style:cs.system.globalization.NumberStyles):cs.system.numerics.BigInteger {})
	@:overload(function(value:String, provider:cs.system.IFormatProvider):cs.system.numerics.BigInteger {})
	@:overload(function(value:cs.system.ReadOnlySpan<cs.Char16>, ?style:cs.system.globalization.NumberStyles, ?provider:cs.system.IFormatProvider):cs.system.numerics.BigInteger {})
	/**
	 * @param value 
	 * @param style 
	 * @param provider 
	 */
	static function Parse(value:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider):cs.system.numerics.BigInteger;
	/**
	 * Raises a  value to the power of a specified value.
	 * @param value The number to raise to the  power.
	 * @param exponent The exponent to raise  by.
	 * @return The result of raising  to the  power.
	 */
	static function Pow(value:cs.system.numerics.BigInteger, exponent:Int):cs.system.numerics.BigInteger;
	/**
	 * Performs integer division on two  values and returns the remainder.
	 * @param dividend The value to be divided.
	 * @param divisor The value to divide by.
	 * @return The remainder after dividing  by .
	 */
	static function Remainder(dividend:cs.system.numerics.BigInteger, divisor:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	/**
	 * Subtracts one  value from another and returns the result.
	 * @param left The value to subtract from (the minuend).
	 * @param right The value to subtract (the subtrahend).
	 * @return The result of subtracting  from .
	 */
	static function Subtract(left:cs.system.numerics.BigInteger, right:cs.system.numerics.BigInteger):cs.system.numerics.BigInteger;
	@:overload(function(value:cs.system.ReadOnlySpan<cs.Char16>, result:cs.Ref<cs.system.numerics.BigInteger>):Bool {})
	@:overload(function(value:String, result:cs.Ref<cs.system.numerics.BigInteger>):Bool {})
	@:overload(function(value:cs.system.ReadOnlySpan<cs.Char16>, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.system.numerics.BigInteger>):Bool {})
	/**
	 * @param value 
	 * @param style 
	 * @param provider 
	 * @param result 
	 */
	static function TryParse(value:String, style:cs.system.globalization.NumberStyles, provider:cs.system.IFormatProvider, result:cs.Ref<cs.system.numerics.BigInteger>):Bool;
	@:overload(function(other:haxe.Int64):Int {})
	@:overload(function(other:cs.system.numerics.BigInteger):Int {})
	@:overload(function(obj:Dynamic):Int {})
	/**
	 * Compares this instance to a signed 64-bit integer and returns an integer that
	 * indicates whether the value of this instance is less than, equal to, or greater
	 * than the value of the signed 64-bit integer.
	 * @param other The signed 64-bit integer to compare.
	 * @return A signed integer value that indicates the relationship of this instance
	 * to , as shown in the following table. Return value Description Less than zero
	 * The current instance is less than . Zero The current instance equals . Greater
	 * than zero The current instance is greater than .
	 */
	function CompareTo(other:cs.UInt64):Int;
	@:overload(function(other:haxe.Int64):Bool {})
	@:overload(function(other:cs.system.numerics.BigInteger):Bool {})
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether the current instance and a signed 64-bit
	 * integer have the same value.
	 * @param other The signed 64-bit integer value to compare.
	 * @return if the signed 64-bit integer and the current instance have the same
	 * value; otherwise, .
	 */
	function Equals(other:cs.UInt64):Bool;
	/**
	 * Gets the number of bytes that will be output by  and .
	 * @param isUnsigned to use unsigned encoding; otherwise, .
	 * @return The number of bytes.
	 */
	function GetByteCount(?isUnsigned:Bool):Int;
	/**
	 * Returns the hash code for the current  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function():cs.NativeArray<cs.UInt8> {})
	/**
	 * Converts a  value to a byte array.
	 * @return The value of the current  object converted to an array of bytes.
	 */
	function ToByteArray(?isUnsigned:Bool, ?isBigEndian:Bool):cs.NativeArray<cs.UInt8>;
	@:overload(function():String {})
	@:overload(function(provider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the numeric value of the current  object to its equivalent string
	 * representation.
	 * @return The string representation of the current  value.
	 */
	function ToString(format:String, provider:cs.system.IFormatProvider):String;
	/**
	 * @param destination 
	 * @param charsWritten 
	 * @param format 
	 * @param provider 
	 */
	function TryFormat(destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>, ?format:cs.system.ReadOnlySpan<cs.Char16>, ?provider:cs.system.IFormatProvider):Bool;
	/**
	 * Copies the value of this  as little-endian twos-complement bytes, using the
	 * fewest number of bytes possible. If the value is zero, outputs one byte whose
	 * element is 0x00.
	 * @param destination The destination span to which the resulting bytes should be
	 * written.
	 * @param bytesWritten The number of bytes written to .
	 * @param isUnsigned to use unsigned encoding; otherwise, .
	 * @param isBigEndian to write the bytes in a big-endian byte order; otherwise, .
	 * @return if the bytes fit in ;  if not all bytes could be written due to lack of
	 * space.
	 */
	function TryWriteBytes(destination:cs.system.Span<cs.UInt8>, bytesWritten:cs.Ref<Int>, ?isUnsigned:Bool, ?isBigEndian:Bool):Bool;
}
