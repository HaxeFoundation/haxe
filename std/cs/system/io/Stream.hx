package cs.system.io;

@:native("System.IO.Stream")
extern class Stream {
	function ReadByte():Int;
	function Read(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Int;
	function WriteByte(value:Int):Void;
	function Write(buffer:cs.NativeArray<cs.UInt8>, offset:Int, count:Int):Void;
	function Flush():Void;
	function Close():Void;
	function Seek(offset:haxe.Int64, origin:SeekOrigin):haxe.Int64;
	var Length(default, never):haxe.Int64;
	var Position(default, default):haxe.Int64;
	var CanRead(default, never):Bool;
	var CanWrite(default, never):Bool;
	var CanSeek(default, never):Bool;
}
