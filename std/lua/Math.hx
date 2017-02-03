package lua;

@:native("_G.math")
extern class Math {
	public static var pi   : Float;
	public static var huge : Float;
	public static var NaN  : Float;

	public static function abs  (i : Float) : Float;
	public static function ceil (i : Float) : Int;
	public static function floor(i : Float) : Int;

	public static function acos (i : Float) : Float;
	public static function asin (i : Float) : Float;
	public static function atan (i : Float) : Float;
	public static function atan2(i : Float, j : Float) : Float;
	public static function cos  (i : Float) : Float;
	public static function cosh (i : Float) : Float;
	public static function sin  (i : Float) : Float;
	public static function sinh (i : Float) : Float;
	public static function tan  (i : Float) : Float;
	public static function tanh (i : Float) : Float;
	public static function rad  (i : Float) : Float;

	public static function modf (i : Float) : Float;
	public static function fmod (i : Float) : Float;

	public static function pow  (i : Float, j : Float) : Float;
	public static function sqrt (i : Float) : Float;
	public static function exp  (i : Float) : Float;
	public static function frexp(i : Float) : Float;
	public static function ldexp(i : Float) : Float;

	public static function log  (i : Float) : Float;
	public static function log10(i : Float) : Float;

	public static function max  (i : Float, j :Float) : Float;
	public static function min  (i : Float, j :Float) : Float;

	/**
		This function is an interface to the simple pseudo-random generator function rand provided by ANSI C.
		(No guarantees can be given for its statistical properties.)

		When called without arguments, returns a uniform pseudo-random real number in the range [0,1).
		When called with an integer number `m`, returns a uniform pseudo-random integer in the range [1, m].
		When called with two integer numbers `m` and `n`, returns a uniform pseudo-random integer in the range [m, n].
	**/
	public static function random(?m:Float, ?n:Float) : Float;

	/**
		Sets `x` as the "seed" for the pseudo-random generator: equal seeds produce equal sequences of numbers.
	**/
	public static function randomseed(x : Float) : Float;
}

