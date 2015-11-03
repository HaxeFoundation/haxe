package lua;
/**
  Externs for the "bit" class that is required for haxe lua
**/

@:native("_G.bit")
extern class Bit {
	public static function bnot(x:Float) : Int;
	public static function band(a:Float, b:Float) : Int;
	public static function bor(a:Float, b:Float) : Int;
	public static function bxor(a:Float, b:Float) : Int;
	public static function lshift(x:Float, places:Int) : Int;
	public static function rshift(x:Float, places:Int) : Int;
	public static function arshift(x:Float, places:Int) : Int;
	public static function mod(numerator:Float, denominator:Float) : Int;
}
