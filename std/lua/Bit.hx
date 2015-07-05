package lua;
/**
  Externs for the "bit" class that is required for haxe lua
**/

@:native("_G.bit")
extern class Bit {
	public static function bnot(x:Float) : Float;
	public static function band(a:Float, b:Float) : Float;
	public static function bor(a:Float, b:Float) : Float;
	public static function bxor(a:Float, b:Float) : Float;
	public static function lshift(x:Float, places:Int) : Float;
	public static function rshift(x:Float, places:Int) : Float;
	public static function arshift(x:Float, places:Int) : Float;
	public static function mod(numerator:Float, denominator:Float) : Float;
}
