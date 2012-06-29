package java.io;
import haxe.io.BytesData;

extern class OutputStream 
{
	function close():Void;
	function flush():Void;
	@:overload(function(b:Int):Void { })
	@:overload(function(b:BytesData, off:Int, len:Int):Void { })
	function write(b:BytesData):Void;
}