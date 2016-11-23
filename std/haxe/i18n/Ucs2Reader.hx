package haxe.i18n;

#if (flash || js || hl || java || cs)

abstract Ucs2Reader(String) {

	public inline function new (s:String) {
		this = s;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length << 1;
	}
	
	public inline function getInt16 (pos:Int) {
		return StringTools.fastCodeAt(this, pos);
	}
}

#else

abstract Ucs2Reader(ByteAccess) {

	public inline function new (bytes:ByteAccess) {
		this = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return this.length;
	}
	
	public inline function getInt16 (pos:Int) {
		return this.getInt16(pos);
	}
}

#end