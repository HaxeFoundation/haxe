package cs.system.io;
import cs.StdTypes;
import haxe.Int64;
import haxe.io.BytesData;

@:native('System.IO.Stream') extern class Stream 
{
	var CanRead(default, null):Bool;
	var CanSeek(default, null):Bool;
	var CanTimeout(default, null):Bool;
	var CanWrite(default, null):Bool;
	var Length(default, null):Int64;
	var Position(default, null):Int64;
	var ReadTimeout:Bool;
	var WriteTimeout:Bool;
	
	function Close():Void;
	function CopyTo(dest:Stream):Void;
	function Dispose():Void;
	function Flush():Void;
	function Read(buf:BytesData, offset:Int, count:Int):Int;
	function ReadByte():Int;
	function Seek(offset:Int64, origin:SeekOrigin):Int64;
	function SetLength(value:Int64):Void;
	function Write(buf:BytesData, offset:Int, count:Int):Void;
	function WriteByte(value:UInt8):Void;
}