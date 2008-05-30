package flash.net;

extern class URLStream extends flash.events.EventDispatcher, implements flash.utils.IDataInput {
	var bytesAvailable(default,null) : UInt;
	var connected(default,null) : Bool;
	var endian : flash.utils.Endian;
	var objectEncoding : UInt;
	function new() : Void;
	function close() : Void;
	function load(request : URLRequest) : Void;
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
}
