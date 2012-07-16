package cs.system;

@:native("System.Math") @:nativegen extern class Math
{
	public static var PI(default, null) : Float;

	public static function Abs(v:Float):Float;
	public static function Min(a:Float,b:Float):Float;
	public static function Max(a:Float,b:Float):Float;
	public static function Sin(v:Float):Float;
	public static function Cos(v:Float):Float;
	public static function Atan2(y:Float,x:Float):Float;
	public static function Tan(v:Float):Float;
	public static function Exp(v:Float):Float;
	public static function Log(v:Float):Float;
	public static function Sqrt(v:Float):Float;
	public static function Round(v:Float):Float;
	public static function Floor(v:Float):Float;
	public static function Ceiling(v:Float):Float;
	public static function Atan(v:Float):Float;
	public static function Asin(v:Float):Float;
	public static function Acos(v:Float):Float;
	public static function Pow(v:Float,exp:Float):Float;
}