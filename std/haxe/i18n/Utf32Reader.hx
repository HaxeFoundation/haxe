package haxe.i18n;

abstract Utf32Reader(ByteAccess) {

	public inline function new (bytes:ByteAccess) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}
	
	public inline function getInt32 (pos:Int) {
		return this.getInt32(pos);
	}
}

