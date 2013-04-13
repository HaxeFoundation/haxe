@:native("f32") extern class Math {
	static inline var PI:Float = 3.14159265358979;
	static var NaN(default, null):Float;
	static var infinity(default, null):Float;
	static var neg_infinity(default, null):Float;
	static var POSITIVE_INFINITY(get, null):Float;
	static var NEGATIVE_INFINITY(get, null):Float;
	static inline function get_POSITIVE_INFINITY():Float {
		return infinity;
	}
	static inline function get_NEGATIVE_INFINITY():Float {
		return neg_infinity;
	}
	static function abs(v:Float):Float;
	static function fmax(a:Float,b:Float):Float;
	static function fmin(a:Float,b:Float):Float;
	static inline function min(a:Float, b:Float):Float {
		return fmin(a, b);
	}
	static inline function max(a:Float, b:Float):Float {
		return fmax(a, b);
	}
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
	static inline function isFinite(f:Float):Bool {
		return is_finite(f);
	}
	static inline function isNaN(f:Float):Bool {
		return is_NaN(f);
	}
}