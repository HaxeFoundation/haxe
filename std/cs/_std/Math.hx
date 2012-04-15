package;
import cs.native.Random;

/**
 * ...
 * @author waneck
 */

@:core_api @:nativegen class Math
{
	public static inline function __init__():Void
	{
		PI = cs.native.Math.PI;
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
		return cs.native.Math.Abs(v);
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
		return cs.native.Math.Sin(v);
	}
	
	public static inline function cos(v:Float):Float
	{
		return cs.native.Math.Cos(v);
	}
	
	public static inline function atan2(y:Float, x:Float):Float
	{
		return cs.native.Math.Atan2(y, x);
	}
	
	public static inline function tan(v:Float):Float
	{
		return cs.native.Math.Tan(v);
	}
	
	public static inline function exp(v:Float):Float
	{
		return cs.native.Math.Exp(v);
	}
	
	public static inline function log(v:Float):Float
	{
		return cs.native.Math.Log(v);
	}
	
	public static inline function sqrt(v:Float):Float
	{
		return cs.native.Math.Sqrt(v);
	}
	
	public static inline function round(v:Float):Int
	{
		return Std.int(cs.native.Math.Round(v)) ;
	}
	
	public static inline function floor(v:Float):Int
	{
		return Std.int(cs.native.Math.Floor(v));
	}
	
	public static inline function ceil(v:Float):Int
	{
		return Std.int(cs.native.Math.Ceiling(v));
	}
	
	public static inline function atan(v:Float):Float
	{
		return cs.native.Math.Atan(v);
	}
	
	public static inline function asin(v:Float):Float
	{
		return cs.native.Math.Asin(v);
	}
	
	public static inline function acos(v:Float):Float
	{
		return cs.native.Math.Acos(v);
	}
	
	public static inline function pow(v:Float, exp:Float):Float
	{
		return cs.native.Math.Pow(v, exp);
	}
	
	public static inline function random() : Float
	{
		return rand.NextDouble();
	}

	public static function isFinite( f : Float ) : Bool
	{
		return untyped __cs__("double.IsInfinity(f)");
	}
	
	public static function isNaN( f : Float ) : Bool
	{
		return untyped __cs__("double.IsNaN(f)");
	}
}