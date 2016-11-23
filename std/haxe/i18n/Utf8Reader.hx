package haxe.i18n;

abstract Utf8Reader(ByteAccess) {

	public inline function new (bytes:ByteAccess) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}

	public inline function fastGet (pos:Int) {
		return this.fastGet(pos);
	}
}