package cs.system;

/** Provides constants and static methods for trigonometric, logarithmic, and other common mathematical functions. */
@:native("System.Math")
extern class Math {
	/** Represents the natural logarithmic base, specified by the constant, . */
	static var E(default, never):Float;
	/** Represents the ratio of the circumference of a circle to its diameter, specified by the constant, π. */
	static var PI(default, never):Float;
	@:overload(function(value:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(value:Float):Float {})
	@:overload(function(value:cs.Int16):cs.Int16 {})
	@:overload(function(value:Int):Int {})
	@:overload(function(value:haxe.Int64):haxe.Int64 {})
	@:overload(function(value:cs.Int8):cs.Int8 {})
	/**
	 * Returns the absolute value of a  number.
	 * @param value A number that is greater than or equal to , but less than or equal
	 * to .
	 * @return A decimal number, x, such that 0 ≤ x ≤.
	 */
	static function Abs(value:Single):Single;
	/**
	 * Returns the angle whose cosine is the specified number.
	 * @param d A number representing a cosine, where  must be greater than or equal to
	 * -1, but less than or equal to 1.
	 * @return An angle, θ, measured in radians, such that 0 ≤θ≤π -or- if  < -1 or  > 1
	 * or  equals .
	 */
	static function Acos(d:Float):Float;
	/**
	 * Returns the angle whose hyperbolic cosine is the specified number.
	 * @param d A number representing a hyperbolic cosine, where  must be greater than
	 * or equal to 1, but less than or equal to .
	 * @return An angle, θ, measured in radians, such that 0 ≤ θ ≤ ∞. -or- if  < 1 or 
	 * equals .
	 */
	static function Acosh(d:Float):Float;
	/**
	 * Returns the angle whose sine is the specified number.
	 * @param d A number representing a sine, where  must be greater than or equal to
	 * -1, but less than or equal to 1.
	 * @return An angle, θ, measured in radians, such that -π/2 ≤θ≤π/2 -or- if  < -1 or
	 * > 1 or  equals .
	 */
	static function Asin(d:Float):Float;
	/**
	 * Returns the angle whose hyperbolic sine is the specified number.
	 * @param d A number representing a hyperbolic sine, where  must be greater than or
	 * equal to , but less than or equal to .
	 * @return An angle, θ, measured in radians, such that -∞ < θ ≤-1, or 1 ≤ θ < ∞.
	 * -or- if  equals .
	 */
	static function Asinh(d:Float):Float;
	/**
	 * Returns the angle whose tangent is the specified number.
	 * @param d A number representing a tangent.
	 * @return An angle, θ, measured in radians, such that -π/2 ≤θ≤π/2. -or- if  equals
	 * , -π/2 rounded to double precision (-1.5707963267949) if  equals , or π/2
	 * rounded to double precision (1.5707963267949) if  equals .
	 */
	static function Atan(d:Float):Float;
	/**
	 * Returns the angle whose tangent is the quotient of two specified numbers.
	 * @param y The y coordinate of a point.
	 * @param x The x coordinate of a point.
	 * @return An angle, θ, measured in radians, such that -π≤θ≤π, and tan(θ) =  / ,
	 * where (, ) is a point in the Cartesian plane. Observe the following: For (, ) in
	 * quadrant 1, 0 < θ < π/2. For (, ) in quadrant 2, π/2 < θ≤π. For (, ) in quadrant
	 * 3, -π < θ < -π/2. For (, ) in quadrant 4, -π/2 < θ < 0. For points on the
	 * boundaries of the quadrants, the return value is the following: If y is 0 and x
	 * is not negative, θ = 0. If y is 0 and x is negative, θ = π. If y is positive and
	 * x is 0, θ = π/2. If y is negative and x is 0, θ = -π/2. If y is 0 and x is 0, θ
	 * = 0. If  or  is , or if  and  are either  or , the method returns .
	 */
	static function Atan2(y:Float, x:Float):Float;
	/**
	 * Returns the angle whose hyperbolic tangent is the specified number.
	 * @param d A number representing a hyperbolic tangent, where  must be greater than
	 * or equal to -1, but less than or equal to 1.
	 * @return An angle, θ, measured in radians, such that -∞ < θ < -1, or 1 < θ < ∞.
	 * -or- if  < -1 or  > 1 or  equals .
	 */
	static function Atanh(d:Float):Float;
	/**
	 * Produces the full product of two 32-bit numbers.
	 * @param a The first number to multiply.
	 * @param b The second number to multiply.
	 * @return The number containing the product of the specified numbers.
	 */
	static function BigMul(a:Int, b:Int):haxe.Int64;
	/**
	 * Returns the cube root of a specified number.
	 * @param d The number whose cube root is to be found.
	 * @return The cube root of . -or- if  equals .
	 */
	static function Cbrt(d:Float):Float;
	@:overload(function(d:cs.system.Decimal):cs.system.Decimal {})
	/**
	 * Returns the smallest integral value that is greater than or equal to the
	 * specified decimal number.
	 * @param d A decimal number.
	 * @return The smallest integral value that is greater than or equal to . Note that
	 * this method returns a  instead of an integral type.
	 */
	static function Ceiling(a:Float):Float;
	@:overload(function(value:cs.UInt8, min:cs.UInt8, max:cs.UInt8):cs.UInt8 {})
	@:overload(function(value:cs.system.Decimal, min:cs.system.Decimal, max:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(value:Float, min:Float, max:Float):Float {})
	@:overload(function(value:cs.Int16, min:cs.Int16, max:cs.Int16):cs.Int16 {})
	@:overload(function(value:Int, min:Int, max:Int):Int {})
	@:overload(function(value:haxe.Int64, min:haxe.Int64, max:haxe.Int64):haxe.Int64 {})
	@:overload(function(value:cs.Int8, min:cs.Int8, max:cs.Int8):cs.Int8 {})
	@:overload(function(value:Single, min:Single, max:Single):Single {})
	@:overload(function(value:cs.UInt16, min:cs.UInt16, max:cs.UInt16):cs.UInt16 {})
	@:overload(function(value:cs.UInt, min:cs.UInt, max:cs.UInt):cs.UInt {})
	/**
	 * Returns  clamped to the inclusive range of  and .
	 * @param value The value to be clamped.
	 * @param min The lower bound of the result.
	 * @param max The upper bound of the result.
	 * @return if  ≤  ≤ . -or- if  < . -or- if  < .
	 */
	static function Clamp(value:cs.UInt64, min:cs.UInt64, max:cs.UInt64):cs.UInt64;
	/**
	 * Returns the cosine of the specified angle.
	 * @param d An angle, measured in radians.
	 * @return The cosine of . If  is equal to , , or , this method returns .
	 */
	static function Cos(d:Float):Float;
	/**
	 * Returns the hyperbolic cosine of the specified angle.
	 * @param value An angle, measured in radians.
	 * @return The hyperbolic cosine of . If  is equal to  or ,  is returned. If  is
	 * equal to ,  is returned.
	 */
	static function Cosh(value:Float):Float;
	@:overload(function(a:Int, b:Int, result:cs.Ref<Int>):Int {})
	/**
	 * Calculates the quotient of two 32-bit signed integers and also returns the
	 * remainder in an output parameter.
	 * @param a The dividend.
	 * @param b The divisor.
	 * @param result The remainder.
	 * @return The quotient of the specified numbers.
	 */
	static function DivRem(a:haxe.Int64, b:haxe.Int64, result:cs.Ref<haxe.Int64>):haxe.Int64;
	/**
	 * Returns  raised to the specified power.
	 * @param d A number specifying a power.
	 * @return The number  raised to the power . If  equals  or , that value is
	 * returned. If  equals , 0 is returned.
	 */
	static function Exp(d:Float):Float;
	@:overload(function(d:cs.system.Decimal):cs.system.Decimal {})
	/**
	 * Returns the largest integral value less than or equal to the specified decimal
	 * number.
	 * @param d A decimal number.
	 * @return The largest integral value less than or equal to .  Note that the method
	 * returns an integral value of type .
	 */
	static function Floor(d:Float):Float;
	/**
	 * Returns the remainder resulting from the division of a specified number by
	 * another specified number.
	 * @param x A dividend.
	 * @param y A divisor.
	 * @return A number equal to  - ( Q), where Q is the quotient of  /  rounded to the
	 * nearest integer (if  /  falls halfway between two integers, the even integer is
	 * returned). If  - ( Q) is zero, the value +0 is returned if  is positive, or -0
	 * if  is negative. If  = 0,  is returned.
	 */
	static function IEEERemainder(x:Float, y:Float):Float;
	@:overload(function(d:Float):Float {})
	/**
	 * Returns the natural (base ) logarithm of a specified number.
	 * @param d The number whose logarithm is to be found.
	 * @return One of the values in the following table. parameter Return value
	 * Positive The natural logarithm of ; that is, ln , or log e  Zero Negative Equal
	 * to  Equal to
	 */
	static function Log(a:Float, newBase:Float):Float;
	/**
	 * Returns the base 10 logarithm of a specified number.
	 * @param d A number whose logarithm is to be found.
	 * @return One of the values in the following table. parameter Return value
	 * Positive The base 10 log of ; that is, log 10. Zero Negative Equal to  Equal to
	 */
	static function Log10(d:Float):Float;
	@:overload(function(val1:cs.UInt8, val2:cs.UInt8):cs.UInt8 {})
	@:overload(function(val1:cs.system.Decimal, val2:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(val1:Float, val2:Float):Float {})
	@:overload(function(val1:cs.Int16, val2:cs.Int16):cs.Int16 {})
	@:overload(function(val1:Int, val2:Int):Int {})
	@:overload(function(val1:haxe.Int64, val2:haxe.Int64):haxe.Int64 {})
	@:overload(function(val1:cs.Int8, val2:cs.Int8):cs.Int8 {})
	@:overload(function(val1:Single, val2:Single):Single {})
	@:overload(function(val1:cs.UInt16, val2:cs.UInt16):cs.UInt16 {})
	@:overload(function(val1:cs.UInt, val2:cs.UInt):cs.UInt {})
	/**
	 * Returns the larger of two 8-bit unsigned integers.
	 * @param val1 The first of two 8-bit unsigned integers to compare.
	 * @param val2 The second of two 8-bit unsigned integers to compare.
	 * @return Parameter  or , whichever is larger.
	 */
	static function Max(val1:cs.UInt64, val2:cs.UInt64):cs.UInt64;
	@:overload(function(val1:cs.UInt8, val2:cs.UInt8):cs.UInt8 {})
	@:overload(function(val1:cs.system.Decimal, val2:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(val1:Float, val2:Float):Float {})
	@:overload(function(val1:cs.Int16, val2:cs.Int16):cs.Int16 {})
	@:overload(function(val1:Int, val2:Int):Int {})
	@:overload(function(val1:haxe.Int64, val2:haxe.Int64):haxe.Int64 {})
	@:overload(function(val1:cs.Int8, val2:cs.Int8):cs.Int8 {})
	@:overload(function(val1:Single, val2:Single):Single {})
	@:overload(function(val1:cs.UInt16, val2:cs.UInt16):cs.UInt16 {})
	@:overload(function(val1:cs.UInt, val2:cs.UInt):cs.UInt {})
	/**
	 * Returns the smaller of two 8-bit unsigned integers.
	 * @param val1 The first of two 8-bit unsigned integers to compare.
	 * @param val2 The second of two 8-bit unsigned integers to compare.
	 * @return Parameter  or , whichever is smaller.
	 */
	static function Min(val1:cs.UInt64, val2:cs.UInt64):cs.UInt64;
	/**
	 * Returns a specified number raised to the specified power.
	 * @param x A double-precision floating-point number to be raised to a power.
	 * @param y A double-precision floating-point number that specifies a power.
	 * @return The number  raised to the power .
	 */
	static function Pow(x:Float, y:Float):Float;
	@:overload(function(d:cs.system.Decimal):cs.system.Decimal {})
	@:overload(function(a:Float):Float {})
	@:overload(function(d:cs.system.Decimal, decimals:Int):cs.system.Decimal {})
	@:overload(function(d:cs.system.Decimal, mode:cs.system.MidpointRounding):cs.system.Decimal {})
	@:overload(function(value:Float, digits:Int):Float {})
	@:overload(function(value:Float, mode:cs.system.MidpointRounding):Float {})
	@:overload(function(d:cs.system.Decimal, decimals:Int, mode:cs.system.MidpointRounding):cs.system.Decimal {})
	/**
	 * Rounds a decimal value to the nearest integral value, and rounds midpoint values
	 * to the nearest even number.
	 * @param d A decimal number to be rounded.
	 * @return The integer nearest the  parameter. If the fractional component of  is
	 * halfway between two integers, one of which is even and the other odd, the even
	 * number is returned. Note that this method returns a  instead of an integral
	 * type.
	 */
	static function Round(value:Float, digits:Int, mode:cs.system.MidpointRounding):Float;
	@:overload(function(value:cs.system.Decimal):Int {})
	@:overload(function(value:Float):Int {})
	@:overload(function(value:cs.Int16):Int {})
	@:overload(function(value:Int):Int {})
	@:overload(function(value:haxe.Int64):Int {})
	@:overload(function(value:cs.Int8):Int {})
	/**
	 * Returns an integer that indicates the sign of a decimal number.
	 * @param value A signed decimal number.
	 * @return A number that indicates the sign of , as shown in the following table.
	 * Return value Meaning -1 is less than zero. 0 is equal to zero. 1 is greater than
	 * zero.
	 */
	static function Sign(value:Single):Int;
	/**
	 * Returns the sine of the specified angle.
	 * @param a An angle, measured in radians.
	 * @return The sine of . If  is equal to , , or , this method returns .
	 */
	static function Sin(a:Float):Float;
	/**
	 * Returns the hyperbolic sine of the specified angle.
	 * @param value An angle, measured in radians.
	 * @return The hyperbolic sine of . If  is equal to , , or , this method returns a 
	 * equal to .
	 */
	static function Sinh(value:Float):Float;
	/**
	 * Returns the square root of a specified number.
	 * @param d The number whose square root is to be found.
	 * @return One of the values in the following table. parameter Return value Zero or
	 * positive The positive square root of . Negative Equals  Equals
	 */
	static function Sqrt(d:Float):Float;
	/**
	 * Returns the tangent of the specified angle.
	 * @param a An angle, measured in radians.
	 * @return The tangent of . If  is equal to , , or , this method returns .
	 */
	static function Tan(a:Float):Float;
	/**
	 * Returns the hyperbolic tangent of the specified angle.
	 * @param value An angle, measured in radians.
	 * @return The hyperbolic tangent of . If  is equal to , this method returns -1. If
	 * value is equal to , this method returns 1. If  is equal to , this method returns
	 * .
	 */
	static function Tanh(value:Float):Float;
	@:overload(function(d:cs.system.Decimal):cs.system.Decimal {})
	/**
	 * Calculates the integral part of a specified decimal number.
	 * @param d A number to truncate.
	 * @return The integral part of ; that is, the number that remains after any
	 * fractional digits have been discarded.
	 */
	static function Truncate(d:Float):Float;
}
