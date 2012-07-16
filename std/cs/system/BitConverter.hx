package cs.system;
import cs.NativeArray;
import cs.NativeArray;
import cs.StdTypes;
import haxe.Int64;

@:native('System.BitConverter') extern class BitConverter 
{
	static var IsLittleEndian(default, null):Bool;
	static function DoubleToInt64Bits(v:Float):Int64;
	static function Int64BitsToDouble(v:Int64):Float;
	static function GetBytes(d:Dynamic):NativeArray<UInt8>;
	static function ToBoolean(b:NativeArray<UInt8>, startIndex:Int):Bool;
	static function ToChar(b:NativeArray<UInt8>, startIndex:Int):Char16;
	static function ToDouble(b:NativeArray<UInt8>, startIndex:Int):Float;
	static function ToInt16(b:NativeArray<UInt8>, startIndex:Int):Int16;
	static function ToInt32(b:NativeArray<UInt8>, startIndex:Int):Int;
	static function ToInt64(b:NativeArray<UInt8>, startIndex:Int):Int64;
	static function ToSingle(b:NativeArray<UInt8>, startIndex:Int):Single;
}