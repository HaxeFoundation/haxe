package haxe.io;

extern class FPHelper {
	public static function i32ToFloat(i:Int):Float;

	public static function floatToI32(f:Float):Int;

	public static function i64ToDouble(low:Int, high:Int):Float;

	public static function doubleToI64(v:Float):Int64;
}
