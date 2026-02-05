package cs.system.numerics;

/** Represents a complex number. */
@:native("System.Numerics.Complex")
extern class Complex extends cs.system.ValueType {
	/** Returns a new  instance with a real number equal to zero and an imaginary number equal to one. */
	static var ImaginaryOne(default, never):cs.system.numerics.Complex;
	/** Returns a new  instance with a real number equal to one and an imaginary number equal to zero. */
	static var One(default, never):cs.system.numerics.Complex;
	/** Returns a new  instance with a real number equal to zero and an imaginary number equal to zero. */
	static var Zero(default, never):cs.system.numerics.Complex;
	/**
	 * Gets the imaginary component of the current  object.
	 * @return The imaginary component of a complex number.
	 */
	var Imaginary(default, never):Float;
	/**
	 * Gets the magnitude (or absolute value) of a complex number.
	 * @return The magnitude of the current instance.
	 */
	var Magnitude(default, never):Float;
	/**
	 * Gets the phase of a complex number.
	 * @return The phase of a complex number, in radians.
	 */
	var Phase(default, never):Float;
	/**
	 * Gets the real component of the current  object.
	 * @return The real component of a complex number.
	 */
	var Real(default, never):Float;
	function new(real:Float, imaginary:Float):Void;
	/**
	 * Gets the absolute value (or magnitude) of a complex number.
	 * @param value A complex number.
	 * @return The absolute value of .
	 */
	static function Abs(value:cs.system.numerics.Complex):Float;
	/**
	 * Returns the angle that is the arc cosine of the specified complex number.
	 * @param value A complex number that represents a cosine.
	 * @return The angle, measured in radians, which is the arc cosine of .
	 */
	static function Acos(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Adds two complex numbers and returns the result.
	 * @param left The first complex number to add.
	 * @param right The second complex number to add.
	 * @return The sum of  and .
	 */
	static function Add(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the angle that is the arc sine of the specified complex number.
	 * @param value A complex number.
	 * @return The angle which is the arc sine of .
	 */
	static function Asin(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the angle that is the arc tangent of the specified complex number.
	 * @param value A complex number.
	 * @return The angle that is the arc tangent of .
	 */
	static function Atan(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Computes the conjugate of a complex number and returns the result.
	 * @param value A complex number.
	 * @return The conjugate of .
	 */
	static function Conjugate(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the cosine of the specified complex number.
	 * @param value A complex number.
	 * @return The cosine of .
	 */
	static function Cos(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the hyperbolic cosine of the specified complex number.
	 * @param value A complex number.
	 * @return The hyperbolic cosine of .
	 */
	static function Cosh(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Divides one complex number by another and returns the result.
	 * @param dividend The complex number to be divided.
	 * @param divisor The complex number to divide by.
	 * @return The quotient of the division.
	 */
	static function Divide(dividend:cs.system.numerics.Complex, divisor:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns  raised to the power specified by a complex number.
	 * @param value A complex number that specifies a power.
	 * @return The number  raised to the power .
	 */
	static function Exp(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Creates a complex number from a point's polar coordinates.
	 * @param magnitude The magnitude, which is the distance from the origin (the
	 * intersection of the x-axis and the y-axis) to the number.
	 * @param phase The phase, which is the angle from the line to the horizontal axis,
	 * measured in radians.
	 * @return A complex number.
	 */
	static function FromPolarCoordinates(magnitude:Float, phase:Float):cs.system.numerics.Complex;
	@:overload(function(value:cs.system.numerics.Complex):cs.system.numerics.Complex {})
	/**
	 * Returns the natural (base ) logarithm of a specified complex number.
	 * @param value A complex number.
	 * @return The natural (base ) logarithm of .
	 */
	static function Log(value:cs.system.numerics.Complex, baseValue:Float):cs.system.numerics.Complex;
	/**
	 * Returns the base-10 logarithm of a specified complex number.
	 * @param value A complex number.
	 * @return The base-10 logarithm of .
	 */
	static function Log10(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the product of two complex numbers.
	 * @param left The first complex number to multiply.
	 * @param right The second complex number to multiply.
	 * @return The product of the  and  parameters.
	 */
	static function Multiply(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the additive inverse of a specified complex number.
	 * @param value A complex number.
	 * @return The result of the  and  components of the  parameter multiplied by -1.
	 */
	static function Negate(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Adds two complex numbers.
	 * @param left The first complex value to add.
	 * @param right The second complex value to add.
	 * @return The sum of  and .
	 */
	static function op_Addition(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Divides a specified complex number by another specified complex number.
	 * @param left The complex value to be divided.
	 * @param right The complex value to divide by.
	 * @return The result of dividing  by .
	 */
	static function op_Division(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns a value that indicates whether two complex numbers are equal.
	 * @param left The first complex number to compare.
	 * @param right The second complex number to compare.
	 * @return if the  and  parameters have the same value; otherwise, .
	 */
	static function op_Equality(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):Bool;
	@:overload(function(value:cs.system.Decimal):cs.system.numerics.Complex {})
	/**
	 * Defines an explicit conversion of a  value to a complex number.
	 * @param value The value to convert to a complex number.
	 * @return A complex number that has a real component equal to  and an imaginary
	 * component equal to zero.
	 */
	static function op_Explicit(value:cs.system.numerics.BigInteger):cs.system.numerics.Complex;
	@:overload(function(value:cs.UInt8):cs.system.numerics.Complex {})
	@:overload(function(value:Float):cs.system.numerics.Complex {})
	@:overload(function(value:cs.Int16):cs.system.numerics.Complex {})
	@:overload(function(value:Int):cs.system.numerics.Complex {})
	@:overload(function(value:haxe.Int64):cs.system.numerics.Complex {})
	@:overload(function(value:cs.Int8):cs.system.numerics.Complex {})
	@:overload(function(value:Single):cs.system.numerics.Complex {})
	@:overload(function(value:cs.UInt16):cs.system.numerics.Complex {})
	@:overload(function(value:cs.UInt):cs.system.numerics.Complex {})
	/**
	 * Defines an implicit conversion of an unsigned byte to a complex number.
	 * @param value The value to convert to a complex number.
	 * @return An object that contains the value of the  parameter as its real part and
	 * zero as its imaginary part.
	 */
	static function op_Implicit(value:cs.UInt64):cs.system.numerics.Complex;
	/**
	 * Returns a value that indicates whether two complex numbers are not equal.
	 * @param left The first value to compare.
	 * @param right The second value to compare.
	 * @return if  and  are not equal; otherwise, .
	 */
	static function op_Inequality(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):Bool;
	/**
	 * Multiplies two specified complex numbers.
	 * @param left The first complex value to multiply.
	 * @param right The second complex value to multiply.
	 * @return The product of  and .
	 */
	static function op_Multiply(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Subtracts a complex number from another complex number.
	 * @param left The value to subtract from (the minuend).
	 * @param right The value to subtract (the subtrahend).
	 * @return The result of subtracting  from .
	 */
	static function op_Subtraction(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the additive inverse of a specified complex number.
	 * @param value The value to negate.
	 * @return The result of the  and  components of the  parameter multiplied by -1.
	 */
	static function op_UnaryNegation(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	@:overload(function(value:cs.system.numerics.Complex, power:Float):cs.system.numerics.Complex {})
	/**
	 * Returns a specified complex number raised to a power specified by a
	 * double-precision floating-point number.
	 * @param value A complex number to be raised to a power.
	 * @param power A double-precision floating-point number that specifies a power.
	 * @return The complex number  raised to the power .
	 */
	static function Pow(value:cs.system.numerics.Complex, power:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the multiplicative inverse of a complex number.
	 * @param value A complex number.
	 * @return The reciprocal of .
	 */
	static function Reciprocal(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the sine of the specified complex number.
	 * @param value A complex number.
	 * @return The sine of .
	 */
	static function Sin(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the hyperbolic sine of the specified complex number.
	 * @param value A complex number.
	 * @return The hyperbolic sine of .
	 */
	static function Sinh(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the square root of a specified complex number.
	 * @param value A complex number.
	 * @return The square root of .
	 */
	static function Sqrt(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Subtracts one complex number from another and returns the result.
	 * @param left The value to subtract from (the minuend).
	 * @param right The value to subtract (the subtrahend).
	 * @return The result of subtracting  from .
	 */
	static function Subtract(left:cs.system.numerics.Complex, right:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the tangent of the specified complex number.
	 * @param value A complex number.
	 * @return The tangent of .
	 */
	static function Tan(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	/**
	 * Returns the hyperbolic tangent of the specified complex number.
	 * @param value A complex number.
	 * @return The hyperbolic tangent of .
	 */
	static function Tanh(value:cs.system.numerics.Complex):cs.system.numerics.Complex;
	@:overload(function(value:cs.system.numerics.Complex):Bool {})
	/**
	 * Returns a value that indicates whether the current instance and a specified
	 * complex number have the same value.
	 * @param value The complex number to compare.
	 * @return if this complex number and  have the same value; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for the current  object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function():String {})
	@:overload(function(provider:cs.system.IFormatProvider):String {})
	@:overload(function(format:String):String {})
	/**
	 * Converts the value of the current complex number to its equivalent string
	 * representation in Cartesian form.
	 * @return The string representation of the current instance in Cartesian form.
	 */
	function ToString(format:String, provider:cs.system.IFormatProvider):String;
}
