package java.nio;
import haxe.Int64;
import haxe.io.BytesData;
import java.StdTypes;

extern class ByteBuffer extends Buffer
{
	static function allocate(capacity:Int):ByteBuffer;
	static function allocateDirect(capacity:Int):ByteBuffer;
	@:overload(function(arr:BytesData, offset:Int, length:Int):ByteBuffer {})
	static function wrap(arr:BytesData):ByteBuffer;
	
	function array():BytesData;
	function arrayOffset():Int;
	
	function compact():ByteBuffer;
	function compareTo(obj:Dynamic):Int;
	
	function duplicate():ByteBuffer;
	
	@:overload(function (dst:BytesData, offset:Int, length:Int):ByteBuffer {})
	@:overload(function (dst:BytesData):ByteBuffer {})
	@:overload(function (idx:Int):Int8 {})
	function get():Int8;
	
	@:overload(function (index:Int):Char16 {})
	function getChar():Char16;
	
	@:overload(function (index:Int):Float {})
	function getDouble():Float;
	
	@:overload(function (index:Int):Single {})
	function getFloat():Single;
	
	@:overload(function (index:Int):Int {})
	function getInt():Int;
	
	@:overload(function (index:Int):Int64 {})
	function getLong():Int64;
	
	@:overload(function (index:Int):Int16 {})
	function getShort():Int16;
	
	function hasArray():Bool;
	function isDirect():Bool;
	
	@:overload(function (index:ByteOrder):ByteBuffer {})
	function order():ByteOrder;
	
	@:overload(function (index:Int, b:Int8):ByteBuffer {})
	@:overload(function (src:ByteBuffer):ByteBuffer {})
	@:overload(function (src:BytesData, offset:Int, length:Int):ByteBuffer {})
	@:overload(function (src:BytesData):ByteBuffer {})
	function put(b:Int8):ByteBuffer;
	
	@:overload(function (index:Int, value:Char16):ByteBuffer {})
	function putChar(value:Char16):ByteBuffer;
	
	@:overload(function (index:Int, value:Float):ByteBuffer {})
	function putDouble(value:Float):ByteBuffer;
	
	@:overload(function (index:Int, value:Single):ByteBuffer {})
	function putFloat(value:Single):ByteBuffer;
	
	@:overload(function (index:Int, value:Int):ByteBuffer {})
	function putInt(value:Int):ByteBuffer;
	
	@:overload(function (index:Int, value:Int64):ByteBuffer {})
	function putLong(value:Int64):ByteBuffer;
	
	@:overload(function (index:Int, value:Int16):ByteBuffer {})
	function putShort(value:Int16):ByteBuffer;
}