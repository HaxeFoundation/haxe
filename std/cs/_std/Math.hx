package;
import cs.system.Random;

@:core_api @:nativegen class Math
{
	public static inline function __init__():Void
	{
		PI = cs.system.Math.PI;
		NaN = untyped __cs__("double.NaN");
		NEGATIVE_INFINITY = untyped __cs__("double.NegativeInfinity");
		POSITIVE_INFINITY = untyped __cs__("double.PositiveInfinity");
		rand = new Random();
		
	}
	
	private static var rand:Random;
	public static var PI(default, null) : Float;
	public static var NaN(default,null) : Float;
	public static var NEGATIVE_INFINITY(default,null) : Float;
	public static var POSITIVE_INFINITY(default,null) : Float;

	public static inline function abs(v:Float):Float
	{
		return cs.system.Math.Abs(v);
	}
	
	public static inline function min(a:Float, b:Float):Float
	{
		return (a < b) ? a : b;
	}
	
	public static inline function max(a:Float, b:Float):Float
	{
		return (a > b) ? a : b;
	}
	
	public static inline function sin(v:Float):Float
	{
		return cs.system.Math.Sin(v);
	}
	
	public static inline function cos(v:Float):Float
	{
		return cs.system.Math.Cos(v);
	}
	
	public static inline function atan2(y:Float, x:Float):Float
	{
		return cs.system.Math.Atan2(y, x);
	}
	
	public static inline function tan(v:Float):Float
	{
		return cs.system.Math.Tan(v);
	}
	
	public static inline function exp(v:Float):Float
	{
		return cs.system.Math.Exp(v);
	}
	
	public static inline function log(v:Float):Float
	{
		return cs.system.Math.Log(v);
	}
	
	public static inline function sqrt(v:Float):Float
	{
		return cs.system.Math.Sqrt(v);
	}
	
	public static function round(v:Float):Int
	{
		var vint = Std.int(v);
		var dec = v - vint;
		if (dec >= 1 || dec <= -1)
			return vint; //overflow
		if (dec >= .5)
			return vint + 1;
		if (dec < -.5)
			return vint - 1;
		return vint;
	}
	
	public static inline function floor(v:Float):Int
	{
		return Std.int(cs.system.Math.Floor(v));
	}
	
	public static inline function ceil(v:Float):Int
	{
		return Std.int(cs.system.Math.Ceiling(v));
	}
	
	public static inline function atan(v:Float):Float
	{
		return cs.system.Math.Atan(v);
	}
	
	public static inline function asin(v:Float):Float
	{
		return cs.system.Math.Asin(v);
	}
	
	public static inline function acos(v:Float):Float
	{
		return cs.system.Math.Acos(v);
	}
	
	public static inline function pow(v:Float, exp:Float):Float
	{
		return cs.system.Math.Pow(v, exp);
	}
	
	public static inline function random() : Float
	{
		return rand.NextDouble();
	}

	public static function isFinite( f : Float ) : Bool
	{
		return untyped __cs__("!double.IsInfinity(f)");
	}
	
	public static function isNaN( f : Float ) : Bool
	{
		return untyped __cs__("double.IsNaN(f)");
	}
}