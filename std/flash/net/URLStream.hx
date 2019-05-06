package flash.net;

extern class URLStream extends flash.events.EventDispatcher implements flash.utils.IDataInput {
	@:flash.property var bytesAvailable(get,never) : UInt;
	@:flash.property var connected(get,never) : Bool;
	@:flash.property @:require(flash11_4) var diskCacheEnabled(get,never) : Bool;
	@:flash.property var endian(get,set) : flash.utils.Endian;
	@:flash.property @:require(flash11_4) var length(get,never) : Float;
	@:flash.property var objectEncoding(get,set) : UInt;
	@:flash.property @:require(flash11_4) var position(get,set) : Float;
	function new() : Void;
	function close() : Void;
	private function get_bytesAvailable() : UInt;
	private function get_connected() : Bool;
	private function get_diskCacheEnabled() : Bool;
	private function get_endian() : flash.utils.Endian;
	private function get_length() : Float;
	private function get_objectEncoding() : UInt;
	private function get_position() : Float;
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
	private function set_endian(value : flash.utils.Endian) : flash.utils.Endian;
	private function set_objectEncoding(value : UInt) : UInt;
	private function set_position(value : Float) : Float;
	@:require(flash11_4) function stop() : Void;
}
