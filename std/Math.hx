/*
 * Copyright (C)2005-2017 Haxe Foundation
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

	@see https://haxe.org/manual/std-math.html
**/
#if cpp @:include("hxMath.h") #end
@:pure
extern class Math
{
	/**
		Represents the ratio of the circumference of a circle to its diameter,
		specified by the constant, π. `PI` is approximately 3.141592653589793.
	**/
	static var PI(default,null) : Float;

	/**
		A special `Float` constant which denotes negative infinity.

		For example, this is the result of -1.0 / 0.0.

		Operations with `NEGATIVE_INFINITY` as an operand may result in
		`NEGATIVE_INFINITY`, `POSITIVE_INFINITY` or `NaN`.

		If this constant is converted to an `Int`, e.g. through `Std.int()`, the
		result is unspecified.
	**/
	static var NEGATIVE_INFINITY(default, null) : Float;

	/**
		A special `Float` constant which denotes positive infinity.

		For example, this is the result of 1.0 / 0.0.

		Operations with `POSITIVE_INFINITY` as an operand may result in
		`NEGATIVE_INFINITY`, `POSITIVE_INFINITY` or `NaN`.

		If this constant is converted to an `Int`, e.g. through `Std.int()`, the
		result is unspecified.
	**/
	static var POSITIVE_INFINITY(default,null) : Float;

	/**
		A special `Float` constant which denotes an invalid number.

		NaN stands for "Not a Number". It occurs when a mathematically incorrect
		operation is executed, such as taking the square root of a negative
		number: Math.sqrt(-1).

		All further operations with `NaN` as an operand will result in `NaN`.

		If this constant is converted to an `Int`, e.g. through `Std.int()`, the
		result is unspecified.

		In order to test if a value is `NaN`, you should use `Math.isNaN()` function.

		@php In PHP versions prior to 5.3.1 VC 9 there may be unexpected
		results when performing arithmetic operations with `NaN` on Windows,
		see <https://bugs.php.net/bug.php?id=42143>
	**/
	static var NaN(default, null) : Float;

	/**
		Returns the absolute value of `v`.

		If `v` is positive or 0, the result is unchanged. Otherwise the result
		is -`v`.

		If `v` is `NEGATIVE_INFINITY` or `POSITIVE_INFINITY`, the result is
		`POSITIVE_INFINITY`.

		If `v` is `NaN`, the result is `NaN`.
	**/
	static function abs(v:Float):Float;

	/**
		Returns the smaller of values `a` and `b`.

		If `a` or `b` are `NaN`, the result is `NaN`.
		If `a` or `b` are `NEGATIVE_INFINITY`, the result is `NEGATIVE_INFINITY`.
		If `a` and `b` are `POSITIVE_INFINITY`, the result is `POSITIVE_INFINITY`.
	**/
	static function min(a:Float, b:Float):Float;

	/**
		Returns the greater of values `a` and `b`.

		If `a` or `b` are `NaN`, the result is `NaN`.
		If `a` or `b` are `POSITIVE_INFINITY`, the result is `POSITIVE_INFINITY`.
		If `a` and `b` are `NEGATIVE_INFINITY`, the result is `NEGATIVE_INFINITY`.
	**/
	static function max(a:Float, b:Float):Float;

	/**
		Returns the trigonometric sine of the specified angle `v`, in radians.

		If `v` is `NaN` or infinite, the result is `NaN`.
	**/
	static function sin(v:Float):Float;

	/**
		Returns the trigonometric cosine of the specified angle `v`, in radians.

		If `v` is `NaN` or infinite, the result is `NaN`.
	**/
	static function cos(v:Float):Float;

	/**
		Returns the trigonometric tangent of the specified angle `v`, in radians.

		If `v` is `NaN` or infinite, the result is `NaN`.
	**/
	static function tan(v:Float):Float;

	/**
		Returns the trigonometric arc of the specified angle `v`, in radians.

		If `v` is `NaN` or infinite, the result is `NaN`.
	**/
	static function asin(v:Float):Float;

	/**
		Returns the trigonometric arc cosine of the specified angle `v`,
		in radians.

		If `v` is `NaN` or infinite, the result is `NaN`.
	**/
	static function acos(v:Float):Float;

	/**
		Returns the trigonometric arc tangent of the specified angle `v`,
		in radians.

		If `v` is `NaN` or infinite, the result is `NaN`.
	**/
	static function atan(v:Float):Float;

	/**
		Returns the trigonometric arc tangent whose tangent is the quotient of
		two specified numbers, in radians.

		If parameter `x` or `y`  is `NaN`, `NEGATIVE_INFINITY` or `POSITIVE_INFINITY`,
		the result is `NaN`.
	**/
	static function atan2(y:Float, x:Float):Float;

	/**
		Returns Euler's number, raised to the power of `v`.

		exp(1.0) is approximately 2.718281828459.

		If `v` is `POSITIVE_INFINITY`, the result is `POSITIVE_INFINITY`.
		If `v` is `NEGATIVE_INFINITY`, the result is `0.0`.
		If `v` is `NaN`, the result is `NaN`.
	**/
	static function exp(v:Float):Float;

	/**
		Returns the natural logarithm of `v`.

		This is the mathematical inverse operation of exp,
		i.e. `log(exp(v)) == v` always holds.

		If `v` is negative (including `NEGATIVE_INFINITY`) or `NaN`, the result
		is `NaN`.
		If `v` is `POSITIVE_INFINITY`, the result is `POSITIVE_INFINITY`.
		If `v` is `0.0`, the result is `NEGATIVE_INFINITY`.
	**/
	static function log(v:Float):Float;

	/**
		Returns a specified base `v` raised to the specified power `exp`.
	**/
	static function pow(v:Float, exp:Float):Float;

	/**
		Returns the square root of `v`.

		If `v` is negative (including `NEGATIVE_INFINITY`) or `NaN`, the result
		is `NaN`.
		If `v` is `POSITIVE_INFINITY`, the result is `POSITIVE_INFINITY`.
		If `v` is `0.0`, the result is `0.0`.
	**/
	static function sqrt(v:Float):Float;

	/**
		Rounds `v` to the nearest integer value.

		Ties are rounded up, so that `0.5` becomes `1` and `-0.5` becomes `0`.

		If `v` is outside of the signed `Int32` range, or is `NaN`, `NEGATIVE_INFINITY`
		or `POSITIVE_INFINITY`, the result is unspecified.
	**/
	static function round(v:Float):Int;

	/**
		Returns the largest integer value that is not greater than `v`.

		If `v` is outside of the signed `Int32` range, or is `NaN`, `NEGATIVE_INFINITY`
		or `POSITIVE_INFINITY`, the result is unspecified.
	**/
	static function floor(v:Float):Int;

	/**
		Returns the smallest integer value that is not less than `v`.

		If `v` is outside of the signed `Int32` range, or is `NaN`, `NEGATIVE_INFINITY`
		or `POSITIVE_INFINITY`, the result is unspecified.
	**/
	static function ceil(v:Float):Int;

	/**
		Returns a pseudo-random number which is greater than or equal to 0.0,
		and less than 1.0.
	**/
	static function random() : Float;

	#if ((flash && !as3) || cpp)
	/**
		Returns the largest integer value that is not greater than `v`, as a `Float`.

		If `v` is is `NaN`, `NEGATIVE_INFINITY` or `POSITIVE_INFINITY`,
		the result is unspecified.
	**/
	static function ffloor( v : Float ) : Float;

	/**
		Returns the smallest integer value that is not less than `v`, as a `Float`.

		If `v` is is `NaN`, `NEGATIVE_INFINITY` or `POSITIVE_INFINITY`,
		the result is unspecified.
	**/
	static function fceil( v : Float ) : Float;

	/**
		Rounds `v` to the nearest integer value, as a Float.

		Ties are rounded up, so that `0.5` becomes `1` and `-0.5` becomes `0`.

		If `v` is is `NaN`, `NEGATIVE_INFINITY` or `POSITIVE_INFINITY`,
		the result is unspecified.
	**/
	static function fround( v : Float ) : Float;

	#else

	static inline function ffloor( v : Float ) : Float {
		return floor(v);
	}

	static inline function fceil( v : Float ) : Float {
		return ceil(v);
	}

	static inline function fround( v : Float ) : Float {
		return round(v);
	}

	#end


	/**
		Tells if `f` is a finite number.

		If `f` is `POSITIVE_INFINITY`, `NEGATIVE_INFINITY` or `NaN`, the result
		is `false`, otherwise the result is `true`.
	**/
	static function isFinite( f : Float ) : Bool;

	/**
		Tells if `f` is not a valid number.

		If `f` is `NaN`, the result is `true`, otherwise the result is `false`.
		In particular, both `POSITIVE_INFINITY` and `NEGATIVE_INFINITY` are
		not considered `NaN`.
	**/
	static function isNaN( f : Float ) : Bool;

	private static function __init__() : Void untyped {
	#if flash
		NaN = __global__["Number"].NaN;
		NEGATIVE_INFINITY = __global__["Number"].NEGATIVE_INFINITY;
		POSITIVE_INFINITY = __global__["Number"].POSITIVE_INFINITY;
	#else
		Math.__name__ = ["Math"];
		Math.NaN = Number["NaN"];
		Math.NEGATIVE_INFINITY = Number["NEGATIVE_INFINITY"];
		Math.POSITIVE_INFINITY = Number["POSITIVE_INFINITY"];
	#end
		Math.isFinite = function(i) {
			return
			#if flash
			__global__["isFinite"](i);
			#else
			false;
			#end
		};
		Math.isNaN = function(i) {
			return
			#if flash
			__global__["isNaN"](i);
			#else
			false;
			#end
		};
	}

}
