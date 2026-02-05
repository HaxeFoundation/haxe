package cs.system;

/** Provides constants and static methods for trigonometric, logarithmic, and other common mathematical functions. */
@:native("System.MathF")
extern class MathF {
	/** Represents the natural logarithmic base, specified by the constant, . */
	static var E(default, never):Single;
	/** Represents the ratio of the circumference of a circle to its diameter, specified by the constant, π. */
	static var PI(default, never):Single;
	/**
	 * Returns the absolute value of a single-precision floating-point number.
	 * @param x A number that is greater than or equal to , but less than or equal to .
	 * @return A single-precision floating-point number, x, such that 0 ≤ x ≤ .
	 */
	static function Abs(x:Single):Single;
	/**
	 * Returns the angle whose cosine is the specified number.
	 * @param x A number representing a cosine, where  must be greater than or equal to
	 * -1, but less than or equal to 1.
	 * @return An angle, θ, measured in radians, such that 0 ≤ θ ≤ π. -or- if  < -1 or 
	 * > 1 or  equals .
	 */
	static function Acos(x:Single):Single;
	/**
	 * Returns the angle whose hyperbolic cosine is the specified number.
	 * @param x A number representing a hyperbolic cosine, where  must be greater than
	 * or equal to 1, but less than or equal to .
	 * @return An angle, θ, measured in radians, such that 0 ≤ θ ≤ ∞. -or- if  < 1 or 
	 * equals .
	 */
	static function Acosh(x:Single):Single;
	/**
	 * Returns the angle whose sine is the specified number.
	 * @param x A number representing a sine, where  must be greater than or equal to
	 * -1, but less than or equal to 1.
	 * @return An angle, θ, measured in radians, such that -π/2 ≤ θ ≤ π/2. -or- if  <
	 * -1 or  > 1 or  equals .
	 */
	static function Asin(x:Single):Single;
	/**
	 * Returns the angle whose hyperbolic sine is the specified number.
	 * @param x A number representing a hyperbolic sine, where  must be greater than or
	 * equal to , but less than or equal to .
	 * @return An angle, θ, measured in radians, such that -∞ < θ ≤ -1, or 1 ≤ θ < ∞.
	 * -or- if  equals .
	 */
	static function Asinh(x:Single):Single;
	/**
	 * Returns the angle whose tangent is the specified number.
	 * @param x A number representing a tangent.
	 * @return An angle, θ, measured in radians, such that -π/2 ≤ θ ≤ π/2. -or- if 
	 * equals , -π/2 rounded to double precision (-1.5707963267949) if  equals , or π/2
	 * rounded to double precision (1.5707963267949) if  equals .
	 */
	static function Atan(x:Single):Single;
	/**
	 * Returns the angle whose tangent is the quotient of two specified numbers.
	 * @param y The y coordinate of a point.
	 * @param x The x coordinate of a point.
	 * @return An angle, θ, measured in radians, such that -π ≤ θ ≤ π, and tan(θ) =  /
	 * , where (, ) is a point in the Cartesian plane. Observe the following: For (, )
	 * in quadrant 1, 0 < θ < π/2. For (, ) in quadrant 2, π/2 < θ ≤ π. For (, ) in
	 * quadrant 3, -π < θ < -π/2. For (, ) in quadrant 4, -π/2 < θ < 0. For points on
	 * the boundaries of the quadrants, the return value is the following: If y is 0
	 * and x is not negative, θ = 0. If y is 0 and x is negative, θ = π. If y is
	 * positive and x is 0, θ = π/2. If y is negative and x is 0, θ = -π/2. If y is 0
	 * and x is 0, θ = 0. If  or  is , or if  and  are either  or , the method returns
	 * .
	 */
	static function Atan2(y:Single, x:Single):Single;
	/**
	 * Returns the angle whose hyperbolic tangent is the specified number.
	 * @param x A number representing a hyperbolic tangent, where  must be greater than
	 * or equal to -1, but less than or equal to 1.
	 * @return An angle, θ, measured in radians, such that -∞ < θ <-1, or 1 < θ < ∞.
	 * -or- if  < -1 or  > 1 or  equals .
	 */
	static function Atanh(x:Single):Single;
	/**
	 * Returns the cube root of a specified number.
	 * @param x The number whose cube root is to be found.
	 * @return The cube root of . -or- if  is equals .
	 */
	static function Cbrt(x:Single):Single;
	/**
	 * Returns the smallest integral value that is greater than or equal to the
	 * specified single-precision floating-point number.
	 * @param x A single-precision floating-point number.
	 * @return The smallest integral value that is greater than or equal to . If  is
	 * equal to , , or , that value is returned. Note that this method returns a 
	 * instead of an integral type.
	 */
	static function Ceiling(x:Single):Single;
	/**
	 * Returns the cosine of the specified angle.
	 * @param x An angle, measured in radians.
	 * @return The cosine of . If  is equal to , , or , this method returns .
	 */
	static function Cos(x:Single):Single;
	/**
	 * Returns the hyperbolic cosine of the specified angle.
	 * @param x An angle, measured in radians.
	 * @return The hyperbolic cosine of . If  is equal to  or ,  is returned. If  is
	 * equal to ,  is returned.
	 */
	static function Cosh(x:Single):Single;
	/**
	 * Returns  raised to the specified power.
	 * @param x A number specifying a power.
	 * @return The number  raised to the power . If  equals  or , that value is
	 * returned. If  equals , 0 is returned.
	 */
	static function Exp(x:Single):Single;
	/**
	 * Returns the largest integral value less than or equal to the specified
	 * single-precision floating-point number.
	 * @param x A single-precision floating-point number.
	 * @return The largest integral value less than or equal to . If  is equal to , ,
	 * or , that value is returned.
	 */
	static function Floor(x:Single):Single;
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
	static function IEEERemainder(x:Single, y:Single):Single;
	@:overload(function(x:Single):Single {})
	/**
	 * Returns the natural (base ) logarithm of a specified number.
	 * @param x The number whose logarithm is to be found.
	 * @return One of the values in the following table. parameter Return value
	 * Positive The natural logarithm of ; that is, ln , or log e  Zero Negative Equal
	 * to  Equal to
	 */
	static function Log(x:Single, y:Single):Single;
	/**
	 * Returns the base 10 logarithm of a specified number.
	 * @param x A number whose logarithm is to be found.
	 * @return One of the values in the following table. parameter Return value
	 * Positive The base 10 log of ; that is, log 10. Zero Negative Equal to  Equal to
	 */
	static function Log10(x:Single):Single;
	/**
	 * Returns the larger of two single-precision floating-point numbers.
	 * @param x The first of two single-precision floating-point numbers to compare.
	 * @param y The second of two single-precision floating-point numbers to compare.
	 * @return Parameter  or , whichever is larger. If , or , or both  and  are equal
	 * to ,  is returned.
	 */
	static function Max(x:Single, y:Single):Single;
	/**
	 * Returns the smaller of two single-precision floating-point numbers.
	 * @param x The first of two single-precision floating-point numbers to compare.
	 * @param y The second of two single-precision floating-point numbers to compare.
	 * @return Parameter  or , whichever is smaller. If , , or both  and  are equal to
	 * ,  is returned.
	 */
	static function Min(x:Single, y:Single):Single;
	/**
	 * Returns a specified number raised to the specified power.
	 * @param x A single-precision floating-point number to be raised to a power.
	 * @param y A single-precision floating-point number that specifies a power.
	 * @return The number  raised to the power .
	 */
	static function Pow(x:Single, y:Single):Single;
	@:overload(function(x:Single):Single {})
	@:overload(function(x:Single, digits:Int):Single {})
	@:overload(function(x:Single, mode:cs.system.MidpointRounding):Single {})
	/**
	 * Rounds a single-precision floating-point value to the nearest integral value,
	 * and rounds midpoint values to the nearest even number.
	 * @param x A single-precision floating-point number to be rounded.
	 * @return The integer nearest . If the fractional component of  is halfway between
	 * two integers, one of which is even and the other odd, then the even number is
	 * returned. Note that this method returns a  instead of an integral type.
	 */
	static function Round(x:Single, digits:Int, mode:cs.system.MidpointRounding):Single;
	/**
	 * Returns an integer that indicates the sign of a single-precision floating-point
	 * number.
	 * @param x A signed number.
	 * @return A number that indicates the sign of , as shown in the following table.
	 * Return value Meaning -1 is less than zero. 0 is equal to zero. 1 is greater than
	 * zero.
	 */
	static function Sign(x:Single):Int;
	/**
	 * Returns the sine of the specified angle.
	 * @param x An angle, measured in radians.
	 * @return The sine of . If  is equal to , , or , this method returns .
	 */
	static function Sin(x:Single):Single;
	/**
	 * Returns the hyperbolic sine of the specified angle.
	 * @param x An angle, measured in radians.
	 * @return The hyperbolic sine of . If  is equal to , , or , this method returns a 
	 * equal to .
	 */
	static function Sinh(x:Single):Single;
	/**
	 * Returns the square root of a specified number.
	 * @param x The number whose square root is to be found.
	 * @return One of the values in the following table. parameter Return value Zero or
	 * positive The positive square root of . Negative Equals  Equals
	 */
	static function Sqrt(x:Single):Single;
	/**
	 * Returns the tangent of the specified angle.
	 * @param x An angle, measured in radians.
	 * @return The tangent of . If  is equal to , , or , this method returns .
	 */
	static function Tan(x:Single):Single;
	/**
	 * Returns the hyperbolic tangent of the specified angle.
	 * @param x An angle, measured in radians.
	 * @return The hyperbolic tangent of . If  is equal to , this method returns -1. If
	 * value is equal to , this method returns 1. If  is equal to , this method returns
	 * .
	 */
	static function Tanh(x:Single):Single;
	/**
	 * Calculates the integral part of a specified single-precision floating-point
	 * number.
	 * @param x A number to truncate.
	 * @return The integral part of ; that is, the number that remains after any
	 * fractional digits have been discarded, or one of the values listed in the
	 * following table. Return value
	 */
	static function Truncate(x:Single):Single;
}
