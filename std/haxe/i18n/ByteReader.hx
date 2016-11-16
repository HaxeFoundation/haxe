package haxe.i18n;

class ByteReader {
	var offset : Int;
	var bytes : ByteAccess;

	public function new (bytes:ByteAccess, offset:Int) {
		this.offset = offset;
		this.bytes = bytes;
	}

	public var length(get, never):Int;

	function get_length () {
		return this.bytes.length - offset;
	}
	public function fastGet (pos:Int) {
		return this.bytes.fastGet(pos + this.offset);
	}
}