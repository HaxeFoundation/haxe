package haxe.i18n;

abstract Utf16Reader(ByteAccess) {

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
	
	public inline function getInt16 (pos:Int) {
		return this.getInt16(pos);
	}
}