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
#if cpp @:include("hxMath") #end
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
		
		(Php) In PHP versions prior to 5.3.1 VC 9 there may be unexpected
		results when performing arithmetic operations with NaN on Windows, see:
			https://bugs.php.net/bug.php?id=42143
	**/
	static var NaN(default, null) : Float;

	/**
		Returns the absolute value of [v].
		
		If [v] is positive or 0, the result is unchanged. Otherwise the result
		is -[v].
		
		If [v] is NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is
		POSITIVE_INFINITY.
		
		If [v] is NaN, the result is NaN.
	**/
	static function abs(v:Float):Float;
	
	/**
		Returns the smaller of values [a] and [b].
		
		If [a] or [b] are NaN, the result is NaN.
		
		If [a] or [b] are NEGATIVE_INFINITY, the result is NEGATIVE_INFINITY.
		
		If [a] and [b] are POSITIVE_INFINITY, the result is POSITIVE_INFINITY.
	**/
	static function min(a:Float, b:Float):Float;
	
	/**
		Returns the greater of values [a] and [b].
		
		If [a] or [b] are NaN, the result is NaN.
		
		If [a] or [b] are POSITIVE_INFINITY, the result is POSITIVE_INFINITY.
		
		If [a] and [b] are NEGATIVE_INFINITY, the result is NEGATIVE_INFINITY.
	**/
	static function max(a:Float, b:Float):Float;
	
	/**
		Returns the trigonometric sine of [v].
		
		The unit of [v] is radians.
		
		If [v] is NaN or infinite, the result is NaN.
	**/
	static function sin(v:Float):Float;
	
	/**
		Returns the trigonometric cosine of [v].
		
		The unit of [v] is radians.
		
		If [v] is NaN or infinite, the result is NaN.
	**/
	static function cos(v:Float):Float;
	
	// TODO
	static function tan(v:Float):Float;
	static function asin(v:Float):Float;
	static function acos(v:Float):Float;
	static function atan(v:Float):Float;
	static function atan2(y:Float, x:Float):Float;
	
	/**
		Returns Euler's number, raised to the power of [v].
		
		exp(1.0) is approximately 2.718281828459.
		
		If [v] is POSITIVE_INFINITY, the result is POSITIVE_INFINITY.
		
		If [v] is NEGATIVE_INFINITY, the result is 0.0.
		
		If [v] is NaN, the result is NaN.
	**/
	static function exp(v:Float):Float;
	
	/**
		Returns the natural logarithm of [v].
		
		If [v] is negative (including NEGATIVE_INFINITY) or NaN, the result is
		NaN.
		
		If [v] is POSITIVE_INFINITY, the result is POSITIVE_INFINITY.
		
		If [v] is 0.0, the result is NEGATIVE_INFINITY.
		
		This is the inverse operation of exp, i.e. log(exp(v)) == v always
		holds.
	**/
	static function log(v:Float):Float;
	
	// TODO
	// http://docs.oracle.com/javase/1.4.2/docs/api/java/lang/Math.html#pow(double, double) <-- wtf?
	static function pow(v:Float, exp:Float):Float;
	
	/**
		Returns the square root of [v].
		
		If [v] is negative (including NEGATIVE_INFINITY) or NaN, the result is
		NaN.
		
		If [v] is POSITIVE_INFINITY, the result is POSITIVE_INFINITY.
		
		If [v] is 0.0, the result is 0.0.
	**/
	static function sqrt(v:Float):Float;
	
	/**
		Rounds [v] to the nearest Int value.
		
		TODO: need spec
	**/
	static function round(v:Float):Int;
	
	/**
		Returns the largest Int value that is not greater than [v].
		
		TODO: need spec
	**/
	static function floor(v:Float):Int;
	
	/**
		Returns the smallest Int value that is not less than [v].
		
		TODO: need spec
	**/
	static function ceil(v:Float):Int;
	
	/**
		Returns a pseudo-random number which is greater than or equal to 0.0,
		and less than 1.0.
	**/
	static function random() : Float;
	
	#if ((flash9 && !as3) || cpp)
	
	static function ffloor( v : Float ) : Float;
	static function fceil( v : Float ) : Float;
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
		Tells if [f] is a finite number.
		
		If [f] is POSITIVE_INFINITY, NEGATIVE_INFINITY or NaN, the result is
		false.
		
		Otherwise the result is true.
	**/
	static function isFinite( f : Float ) : Bool;
	
	/**
		Tells if [f] is not a valid number.
		
		If [f] is NaN, the result is true.
		
		Otherwise the result is false. In particular, both POSITIVE_INFINITY and
		NEGATIVE_INFINITY are not considered invalid numbers.
	**/
	static function isNaN( f : Float ) : Bool;

	private static function __init__() : Void untyped {
	#if flash9
		NaN = __global__["Number"].NaN;
		NEGATIVE_INFINITY = __global__["Number"].NEGATIVE_INFINITY;
		POSITIVE_INFINITY = __global__["Number"].POSITIVE_INFINITY;
	#else
		Math.__name__ = ["Math"];
		Math.NaN = Number["NaN"];
		Math.NEGATIVE_INFINITY = Number["NEGATIVE_INFINITY"];
		Math.POSITIVE_INFINITY = Number["POSITIVE_INFINITY"];
	#end
	#if js
		__feature__("Type.resolveClass",$hxClasses['Math'] = Math);
	#end
		Math.isFinite = function(i) {
			return
			#if flash9
			__global__["isFinite"](i);
			#elseif flash
			_global["isFinite"](i);
			#elseif js
			__js__("isFinite")(i);
			#else
			false;
			#end
		};
		Math.isNaN = function(i) {
			return
			#if flash9
			__global__["isNaN"](i);
			#elseif flash
			_global["isNaN"](i);
			#elseif js
			__js__("isNaN")(i);
			#else
			false;
			#end
		};
	}

}


