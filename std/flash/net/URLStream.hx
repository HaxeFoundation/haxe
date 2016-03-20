package flash.net;

extern class URLStream extends flash.events.EventDispatcher implements flash.utils.IDataInput {
	var bytesAvailable(default,never) : UInt;
	var connected(default,never) : Bool;
	@:require(flash11_4) var diskCacheEnabled(default,never) : Bool;
	var endian : flash.utils.Endian;
	@:require(flash11_4) var length(default,never) : Float;
	var objectEncoding : UInt;
	@:require(flash11_4) var position : Float;
	function new() : Void;
	function close() : Void;
	function load(request : URLRequest) : Void;
	function readBoolean() : Bool;
	function readByte() : Int;
	function readBytes(bytes : flash.utils.ByteArray, offset : UInt = 0, length : UInt = 0) : Void;
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
	@:require(flash11_4) function stop() : Void;
}
