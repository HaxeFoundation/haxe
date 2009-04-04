package flash.net;

extern class Socket extends flash.events.EventDispatcher, implements flash.utils.IDataInput, implements flash.utils.IDataOutput {
	var bytesAvailable(default,null) : UInt;
	var connected(default,null) : Bool;
	var endian : flash.utils.Endian;
	var objectEncoding : UInt;
	#if flash10
	var timeout : UInt;
	#end
	function new(?host : String, ?port : Int) : Void;
	function close() : Void;
	function connect(host : String, port : Int) : Void;
	function flush() : Void;
	function readBoolean() : Bool;
	function readByte() : Int;
	function readBytes(bytes : flash.utils.ByteArray, ?offset : UInt, ?length : UInt) : Void;
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
	function writeBoolean(value : Bool) : Void;
	function writeByte(value : Int) : Void;
	function writeBytes(bytes : flash.utils.ByteArray, ?offset : UInt, ?length : UInt) : Void;
	function writeDouble(value : Float) : Void;
	function writeFloat(value : Float) : Void;
	function writeInt(value : Int) : Void;
	function writeMultiByte(value : String, charSet : String) : Void;
	function writeObject(object : Dynamic) : Void;
	function writeShort(value : Int) : Void;
	function writeUTF(value : String) : Void;
	function writeUTFBytes(value : String) : Void;
	function writeUnsignedInt(value : UInt) : Void;
}
