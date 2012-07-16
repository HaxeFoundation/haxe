package cs.system.text;
import cs.NativeArray;
import cs.StdTypes;

@:native("System.Text.Encoding") extern class Encoding
{
	static var UTF8(default, null):Encoding;
	
	function GetString(b:NativeArray<UInt8>, index:Int, count:Int):String;
	function GetBytes(s:String):NativeArray<UInt8>;
	
}