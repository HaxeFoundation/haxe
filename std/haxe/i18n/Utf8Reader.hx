package haxe.i18n;

class Utf8Reader {

	var bytes : ByteAccess;

	public inline function new (bytes:ByteAccess) {
		this.bytes = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return bytes.length;
	}

	public inline function fastGet (pos:Int) {
		return bytes.fastGet(pos);
	}
}