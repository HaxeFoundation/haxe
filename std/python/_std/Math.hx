
package;

import python.lib.Builtin;

/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
/**
	This class defines mathematical functions and constants.
**/
@:pythonImport("math")
@:coreApi
extern class Math
{
	static var PI(default,null) : Float;

	/**
		A special Float constant which denotes negative infinity.

		For example, this is the result of -1.0 / 0.0.

		Operations with NEGATIVE_INFINITY as an operand may result in
		Operations with NEGATIVE_INFINITY as an operand may result in
		NEGATIVE_INFINITY, POSITIVE_INFINITY or NaN. For detailed information,
		see ...

		If this constant is converted to an Int, e.g. through Std.int(), the
		result is unspecified.
	**/
	static var NEGATIVE_INFINITY(default, null) : Float;

	/**
		A special Float constant which denotes negative infinity.

		For example, this is the result of 1.0 / 0.0.

		Operations with POSITIVE_INFINITY as an operand may result in
		NEGATIVE_INFINITY, POSITIVE_INFINITY or NaN. For detailed information,
		see ...

		If this constant is converted to an Int, e.g. through Std.int(), the
		result is unspecified.
	**/
	static var POSITIVE_INFINITY(default,null) : Float;

	/**
		A special Float constant which denotes an invalid number.

		NaN stands for "Not a Number". It occurs when a mathematically incorrect
		operation is executed, such as taking the square root of a negative
		number: Math.sqrt(-1).

		All further operations with NaN as an operand will result in NaN.

		If this constant is converted to an Int, e.g. through Std.int(), the
		result is unspecified.

		In order to test if a value is NaN, you should use Math.isNaN() function.

		(Php) In PHP versions prior to 5.3.1 VC 9 there may be unexpected
		results when performing arithmetic operations with NaN on Windows, see:
			https://bugs.php.net/bug.php?id=42143
	**/
	static var NaN(default, null) : Float;

	/**
		Returns the absolute value of `v`.

		If `v` is positive or 0, the result is unchanged. Otherwise the result
		is -`v`.

		If `v` is NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is
		POSITIVE_INFINITY.

		If `v` is NaN, the result is NaN.
	**/
	public static inline function abs(v:Float):Float
	{
		return (Math:Dynamic).fabs(v);
	}

	/**
		Returns the smaller of values `a` and `b`.

		If `a` or `b` are NaN, the result is NaN.

		If `a` or `b` are NEGATIVE_INFINITY, the result is NEGATIVE_INFINITY.

		If `a` and `b` are POSITIVE_INFINITY, the result is POSITIVE_INFINITY.
	**/

	public static inline function min(a:Float, b:Float):Float {
		return if (isNaN(a)) a else if (isNaN(b)) b else Builtin.min(a,b);
	}

	/**
		Returns the greater of values `a` and `b`.

		If `a` or `b` are NaN, the result is NaN.

		If `a` or `b` are POSITIVE_INFINITY, the result is POSITIVE_INFINITY.

		If `a` and `b` are NEGATIVE_INFINITY, the result is NEGATIVE_INFINITY.
	**/
	public static inline function max(a:Float, b:Float):Float
	{
		return if (isNaN(a)) a else if (isNaN(b)) b else Builtin.max(a,b);
	}

	/**
		Returns the trigonometric sine of `v`.

		The unit of `v` is radians.

		If `v` is NaN or infinite, the result is NaN.
	**/
	public static inline function sin(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.sin(v);
	}

	/**
		Returns the trigonometric cosine of `v`.

		The unit of `v` is radians.

		If `v` is NaN or infinite, the result is NaN.
	**/
	public static inline function cos(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.cos(v);
	}


	static function tan(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.tan(v);
	}
	static function asin(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.asin(v);
	}
	static function acos(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.acos(v);
	}
	static function atan(v:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.atan(v);
	}
	static function atan2(y:Float, x:Float):Float {
		return if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) NaN else python.lib.Math.atan2(v);
	}

	/**
		Returns Euler's number, raised to the power of `v`.

		exp(1.0) is approximately 2.718281828459.

		If `v` is POSITIVE_INFINITY, the result is POSITIVE_INFINITY.

		If `v` is NEGATIVE_INFINITY, the result is 0.0.

		If `v` is NaN, the result is NaN.
	**/
	public static inline function exp(v:Float):Float
	{
		if (v == NEGATIVE_INFINITY) {
			return 0.0;
		} else if (v == POSITIVE_INFINITY) {
			return POSITIVE_INFINITY;
		} else {
			return (Math:Dynamic).exp(v);
		}
	}

	/**
		Returns the natural logarithm of `v`.

		If `v` is negative (including NEGATIVE_INFINITY) or NaN, the result is
		NaN.

		If `v` is POSITIVE_INFINITY, the result is POSITIVE_INFINITY.

		If `v` is 0.0, the result is NEGATIVE_INFINITY.

		This is the inverse operation of exp, i.e. log(exp(v)) == v always
		holds.
	**/
	public static inline function log(v:Float):Float {
		return if (v == 0.0) NEGATIVE_INFINITY else if (v < 0.0) NaN else python.lib.Math.log(v);
	}

	// TODO
	// http://docs.oracle.com/javase/1.4.2/docs/api/java/lang/Math.html#pow(double, double) <-- wtf?
	static function pow(v:Float, exp:Float):Float;

	/**
		Returns the square root of `v`.

		If `v` is negative (including NEGATIVE_INFINITY) or NaN, the result is
		NaN.

		If `v` is POSITIVE_INFINITY, the result is POSITIVE_INFINITY.

		If `v` is 0.0, the result is 0.0.
	**/
	public static inline function sqrt(v:Float):Float
	{
		return if (v < 0) NaN else python.lib.Math.sqrt(v);
	}

	/**
		Rounds `v` to the nearest Int value.

		If v is outside of the signed Int32 range, or is NaN, NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is unspecified.

		TODO: need spec
	**/
	public static inline function round(v:Float):Int {
		return Math.floor(v + 0.5);
	}

	/**
		Returns the largest Int value that is not greater than `v`.

		If v is outside of the signed Int32 range, or is NaN, NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is unspecified.

		TODO: need spec
	**/
	static function floor(v:Float):Int;

	/**
		Returns the smallest Int value that is not less than `v`.

		If v is outside of the signed Int32 range, or is NaN, NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is unspecified.

		TODO: need spec
	**/
	static function ceil(v:Float):Int;

	/**
		Returns a pseudo-random number which is greater than or equal to 0.0,
		and less than 1.0.
	**/
	inline static function random() : Float {
		return python.lib.Random.random();
	}



	static inline function ffloor( v : Float ) : Float
	{
		if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) return v;
		if (isNaN(v)) return NaN;
		return floor(v);
	}

	static inline function fceil( v : Float ) : Float
	{
		if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) return v;
		if (isNaN(v)) return NaN;
		return ceil(v);
	}

	static inline function fround( v : Float ) : Float
	{
		if (v == POSITIVE_INFINITY || v == NEGATIVE_INFINITY) return v;
		if (isNaN(v)) return NaN;
		return round(v);
	}




	/**
		Tells if `f` is a finite number.

		If `f` is POSITIVE_INFINITY, NEGATIVE_INFINITY or NaN, the result is
		false.

		Otherwise the result is true.
	**/
	static inline function isFinite( f : Float ) : Bool return f != POSITIVE_INFINITY && f != NEGATIVE_INFINITY && !isNaN(f);

	/**
		Tells if `f` is not a valid number.

		If `f` is NaN, the result is true.

		Otherwise the result is false. In particular, both POSITIVE_INFINITY and
		NEGATIVE_INFINITY are not considered NaN.
	**/
	static inline function isNaN( f : Float ) : Bool {

		return python.lib.Math.isnan(f);
	}

	static function __init__():Void {
		NEGATIVE_INFINITY = Builtin.float('-inf');
		POSITIVE_INFINITY = Builtin.float('inf');
		NaN = Builtin.float("nan");
		PI = python.lib.Math.pi;
	}

}