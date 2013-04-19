@:nativeGen @:native("f64") extern class Math {
	static inline var PI:Float = 3.14159265358979;
	static var NaN(default, null):Float;
	static var infinity(default, null):Float;
	static var neg_infinity(default, null):Float;
	static var POSITIVE_INFINITY(default, null):Float;
	static var NEGATIVE_INFINITY(default, null):Float;
	static function abs(v:Float):Float;
	static function max(a:Float,b:Float):Float;
	static function min(a:Float,b:Float):Float;
	static function sin(v:Float):Float;
	static function cos(v:Float):Float;
	static function atan2(y:Float,x:Float):Float;
	static function tan(v:Float):Float;
	static function exp(v:Float):Float;
	static function log(v:Float):Float;
	static function sqrt(v:Float):Float;
	static function round(v:Float):Int;
	static function floor(v:Float):Int;
	static function ceil(v:Float):Int;
	static function atan(v:Float):Float;
	static function fround(v:Float):Float;
	static function ffloor(v:Float):Float;
	static function fceil(v:Float):Float;
	static function asin(v:Float):Float;
	static function acos(v:Float):Float;
	static function pow(v:Float,exp:Float):Float;
	static function random() : Float;

	static function is_finite(f:Float): Bool;
	static function is_NaN(f:Float): Bool;
}