package java.io;
import haxe.io.BytesData;

extern class ByteArrayOutputStream extends OutputStream
{
	@:overload(function(size:Int):Void {})
	function new():Void;
	function toByteArray():BytesData;
}