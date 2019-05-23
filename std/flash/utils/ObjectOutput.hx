package flash.utils;

extern class ObjectOutput implements IDataOutput {
	@:flash.property var endian(get,set) : Endian;
	@:flash.property var objectEncoding(get,set) : UInt;
	function new() : Void;
	private function get_endian() : Endian;
	private function get_objectEncoding() : UInt;
	private function set_endian(value : Endian) : Endian;
	private function set_objectEncoding(value : UInt) : UInt;
	function writeBoolean(value : Bool) : Void;
	function writeByte(value : Int) : Void;
	function writeBytes(bytes : ByteArray, offset : UInt = 0, length : UInt = 0) : Void;
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
