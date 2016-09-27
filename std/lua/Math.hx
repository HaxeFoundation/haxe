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

	public static function random() : Float;
	public static function randomseed(i : Float) : Float;
}

