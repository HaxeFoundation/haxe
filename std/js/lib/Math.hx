/*
 * Copyright (C)2005-2019 Haxe Foundation
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
package js.lib;

import haxe.extern.Rest;

/**
	Math is a built-in object that has properties and methods for mathematical constants and functions.
	Not a function object.

	Documentation [Math](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Math")
extern class Math {
	/**
		Euler's constant and the base of natural logarithms, approximately 2.718.
	**/
	static var E(default,never):Float;

	/**
		Natural logarithm of 2, approximately 0.693.
	**/
	static var LN2(default,never):Float;
	
	/**
		Natural logarithm of 10, approximately 2.303.
	**/
	static var LN10(default,never):Float;
	
	/**
		Base 2 logarithm of E, approximately 1.443.
	**/
	static var LOG2E(default,never):Float;
	
	/**
		Base 10 logarithm of E, approximately 0.434.
	**/
	static var LOG10E(default,never):Float;

	/**
		Ratio of the circumference of a circle to its diameter, approximately 3.14159.
	**/
	static var PI(default,never):Float;
	
	/**
		Square root of 1/2; equivalently, 1 over the square root of 2, approximately 0.707.
	**/
	static var SQRT1_2(default,never):Float;
	
	/**
		Square root of 2, approximately 1.414.
	**/
	static var SQRT2(default,never):Float;

	/**
		Returns the absolute value of a number.
	**/
	@:overload(function(x:Float):Float {})
	@:pure static function abs(x:Int):Int;

	/**
		Returns the arccosine of a number.
	**/
	@:pure static function acos(x:Float):Float;

	/**
		Returns the hyperbolic arccosine of a number.
	**/
	@:pure static function acosh(x:Float):Float;

	/**
		Returns the arcsine of a number.
	**/
	@:pure static function asin(x:Float):Float;

	/**
		Returns the hyperbolic arcsine of a number.
	**/
	@:pure static function asinh(x:Float):Float;

	/**
		Returns the arctangent of a number.
	**/
	@:pure static function atan(x:Float):Float;

	/**
		Returns the hyperbolic arctangent of a number.
	**/
	@:pure static function atanh(x:Float):Float;

	/**
		Returns the arctangent of the quotient of its arguments.
	**/
	@:pure static function atan2(y:Float, x:Float):Float;

	/**
		Returns the cube root of a number.
	**/
	@:pure static function cbrt(x:Float):Float;

	/**
		Returns the smallest integer greater than or equal to a number.
	**/
	@:pure static function ceil(x:Float):Int;

	/**
		Returns the number of leading zeroes of a 32-bit integer.
	**/
	@:pure static function clz32(x:Int):Int;

	/**
		Returns the cosine of a number.
	**/
	@:pure static function cos(x:Float):Float;

	/**
		Returns the hyperbolic cosine of a number.
	**/
	@:pure static function cosh(x:Float):Float;

	/**
		Returns Ex, where x is the argument, and E is Euler's constant (2.718…), the base of the natural logarithm.
	**/
	@:pure static function exp(x:Float):Float;

	/**
		Returns subtracting 1 from exp(x).
	**/
	@:pure static function expm1(x:Float):Float;

	/**
		Returns the largest integer less than or equal to a number.
	**/
	@:pure static function floor(x:Float):Int;

	/**
		Returns the nearest single precision float representation of a number.
	**/
	@:pure static function fround(x:Float):Float;

	/**
		Returns the square root of the sum of squares of its arguments.
	**/
	@:pure static function hypot(args:Rest<Float>):Float;

	/**
		Returns the result of a 32-bit integer multiplication.
	**/
	@:pure static function imul(x:Int, y:Int):Int;

	/**
		Returns the natural logarithm (loge, also ln) of a number.
	**/
	@:pure static function log(x:Float):Float;

	/**
		Returns the natural logarithm (loge, also ln) of 1 + x for a number x.
	**/
	@:pure static function log1p(x:Float):Float;

	/**
		Returns the base 10 logarithm of a number.
	**/
	@:pure static function log10(x:Float):Float;

	/**
		Returns the base 2 logarithm of a number.
	**/
	@:pure static function log2(x:Float):Float;

	/**
		Returns the largest of zero or more numbers.
	**/
	@:overload(function(args:Rest<Float>):Float {})
	@:pure static function max(args:Rest<Int>):Int;

	/**
		Returns the smallest of zero or more numbers.
	**/
	@:overload(function(args:Rest<Float>):Float {})
	@:pure static function min(args:Rest<Int>):Int;

	/**
		Returns base to the exponent power, that is, baseexponent.
	**/
	@:pure static function pow(x:Float, y:Float):Float;

	/**
		Returns a pseudo-random number between 0 and 1.
	**/
	@:pure static function random():Float;

	/**
		Returns the value of a number rounded to the nearest integer.
	**/
	@:pure static function round(x:Float):Int;

	/**
		Returns the sign of the x, indicating whether x is positive, negative or zero.
	**/
	@:pure static function sign(x:Float):Int;

	/**
		Returns the sine of a number.
	**/
	@:pure static function sin(x:Float):Float;

	/**
		Returns the hyperbolic sine of a number.
	**/
	@:pure static function sinh(x:Float):Float;

	/**
		Returns the positive square root of a number.
	**/
	@:pure static function sqrt(x:Float):Float;

	/**
		Returns the tangent of a number.
	**/
	@:pure static function tan(x:Float):Float;

	/**
		Returns the hyperbolic tangent of a number.
	**/
	@:pure static function tanh(x:Float):Float;

	/**
		Returns the integer part of the number x, removing any fractional digits. 
	**/
	@:pure static function trunc(x:Float):Int;
}