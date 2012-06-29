package java.io;
import haxe.io.BytesData;

extern class InputStream 
{
	function close():Void;
	function available():Int;
	function flush():Void;
	@:overload(function():Int { })
	@:overload(function(b:BytesData, off:Int, len:Int):Int { })
	function read(b:BytesData):Int;
}