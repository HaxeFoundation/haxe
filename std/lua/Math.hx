package lua;

import haxe.extern.Rest;

/**
	Mathematical Functions
**/
@:native("_G.math")
extern class Math {
	/**
		The value of pi.
	**/
	static var pi(default,never) : Float;

	/**
		The value HUGE_VAL, a value larger than or equal to any other numerical value.
	**/
	static var huge(default,never) : Float;

	/**
		Returns the absolute value of x.
	**/
	static function abs  (x : Float) : Float;

	/**
		Returns the smallest integer larger than or equal to x.
	**/
	static function ceil (x : Float) : Int;

	/**
		Returns the largest integer smaller than or equal to x.
	**/
	static function floor(x : Float) : Int;

	/**
		Returns the arc cosine of x (in radians).
	**/
	static function acos (x : Float) : Float;

	/**
		Returns the arc sine of x (in radians).
	**/
	static function asin (x : Float) : Float;

	/**
		Returns the arc tangent of x (in radians).
	**/
	static function atan (x : Float) : Float;

	/**
		Returns the arc tangent of y/x (in radians), but uses the signs of both parameters to find the quadrant of the result.
		(It also handles correctly the case of x being zero.)
	**/
	static function atan2(y : Float, x : Float) : Float;

	/**
		Returns the cosine of x (assumed to be in radians).
	**/
	static function cos  (x : Float) : Float;

	/**
		Returns the hyperbolic cosine of x.
	**/
	static function cosh (x : Float) : Float;

	/**
		Returns the sine of x (assumed to be in radians).
	**/
	static function sin  (x : Float) : Float;

	/**
		Returns the hyperbolic sine of x.
	**/
	static function sinh (x : Float) : Float;

	/**
		Returns the tangent of x (assumed to be in radians)
	**/
	static function tan  (x : Float) : Float;

	/**
		Returns the hyperbolic tangent of x.
	**/
	static function tanh (x : Float) : Float;

	/**
		Returns the angle x (given in degrees) in radians.
	**/
	static function rad  (x : Float) : Float;

	/**
		Returns two numbers, the integral part of x and the fractional part of x.
	**/
	static function modf (x : Float) : Float;

	/**
		Returns the remainder of the division of x by y that rounds the quotient towards zero.
	**/
	static function fmod (x : Float) : Float;

	/**
		Returns y-th power of x.
	**/
	static function pow  (x : Float, y : Float) : Float;

	/**
		Returns the square root of x.
	**/
	static function sqrt (x : Float) : Float;

	/**
		Returns the value e^x.
	**/
	static function exp  (x : Float) : Float;

	/**
		Returns m and e such that x = m2^e, e is an integer and the absolute value of m is in the range [0.5, 1) (or zero when x is zero).
	**/
	static function frexp(x : Float) : MathFrexpResult;

	/**
		Returns m2^e (e should be an integer).
	**/
	static function ldexp(m : Float, e : Int) : Float;

	/**
		Returns the natural logarithm of x.
	**/
	static function log  (x : Float) : Float;

	/**
		Returns the base-10 logarithm of x.
	**/
	static function log10(x : Float) : Float;

	/**
		Returns the maximum value among its arguments.
	**/
	static function max  (x : Float, numbers : Rest<Float>) : Float;

	/**
		Returns the minimum value among its arguments.
	**/
	static function min  (x : Float, numbers : Rest<Float>) : Float;

	/**
		Returns the angle x (given in radians) in degrees.
	**/
	static function deg  (x : Float) : Float;

	/**
		This function is an interface to the simple pseudo-random generator function rand provided by ANSI C.
		(No guarantees can be given for its statistical properties.)

		When called without arguments, returns a uniform pseudo-random real number in the range [0,1).
		When called with an integer number `m`, returns a uniform pseudo-random integer in the range [1, m].
		When called with two integer numbers `m` and `n`, returns a uniform pseudo-random integer in the range [m, n].
	**/
	static function random(?m:Float, ?n:Float) : Float;

	/**
		Sets `x` as the "seed" for the pseudo-random generator: equal seeds produce equal sequences of numbers.
	**/
	static function randomseed(x : Float) : Float;
}

/**
	The return value of `Math.frexp`.
**/
@:multiReturn extern class MathFrexpResult {
	var m:Float;
	var e:Int;
}
