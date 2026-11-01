package haxe.io;

import haxe.Int32;

extern class FPHelper {
	public static function i32ToFloat(i:Int32):Float;

	public static function floatToI32(f:Float):Int32;

	public static function i64ToDouble(low:Int32, high:Int32):Float;

	public static function doubleToI64(v:Float):Int64;
}
