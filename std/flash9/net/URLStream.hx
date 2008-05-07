package flash.net;

extern class URLStream extends flash.events.EventDispatcher, implements flash.utils.IDataInput {
	function new() : Void;
	var bytesAvailable(default,null) : UInt;
	function close() : Void;
	var connected(default,null) : Bool;
	var endian : String;
	function load(request : flash.net.URLRequest) : Void;
	var objectEncoding : UInt;
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
