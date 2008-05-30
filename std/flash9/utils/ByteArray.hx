package flash.utils;

extern class ByteArray implements IDataInput, implements IDataOutput, implements ArrayAccess<Int> {
	var bytesAvailable(default,null) : UInt;
	var endian : Endian;
	var length : UInt;
	var objectEncoding : UInt;
	var position : UInt;
	function new() : Void;
	function compress() : Void;
	function readBoolean() : Bool;
	function readByte() : Int;
	function readBytes(bytes : ByteArray, ?offset : UInt, ?length : UInt) : Void;
	function readDouble() : Float;
	function readFloat() : Float;
	function readInt() : Int;
	function readMultiByte(length : UInt, charSet : String) : String;
	function readObject() : Dynamic;
	function readShort() : Int;
	function readUTF() : String;
	function readUTFBytes(length : UInt) : String;
	function readUnsignedByte() : UInt;
	function readUnsignedInt() : UInt;
	function readUnsignedShort() : UInt;
	function toString() : String;
	function uncompress() : Void;
	function writeBoolean(value : Bool) : Void;
	function writeByte(value : Int) : Void;
	function writeBytes(bytes : ByteArray, ?offset : UInt, ?length : UInt) : Void;
	function writeDouble(value : Float) : Void;
	function writeFloat(value : Float) : Void;
	function writeInt(value : Int) : Void;
	function writeMultiByte(value : String, charSet : String) : Void;
	function writeObject(object : Dynamic) : Void;
	function writeShort(value : Int) : Void;
	function writeUTF(value : String) : Void;
	function writeUTFBytes(value : String) : Void;
	function writeUnsignedInt(value : UInt) : Void;

	#if flash10
	function clear():Void;
	function deflate():Void;
	function inflate():Void;
	#end

	static var defaultObjectEncoding : UInt;
}
