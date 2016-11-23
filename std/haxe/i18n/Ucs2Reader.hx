package haxe.i18n;

#if (flash || js || hl || java || cs)

class Ucs2Reader {

	var s : String;

	public inline function new (s:String) {
		this.s = s;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return s.length << 1;
	}
	
	public inline function getInt16 (pos:Int) {
		return StringTools.fastCodeAt(s, pos);
	}

}

#else

class Ucs2Reader {

	var bytes : ByteAccess;

	public inline function new (bytes:ByteAccess) {
		this.bytes = bytes;
	}

	public var length(get, never):Int;

	inline function get_length () {
		return bytes.length;
	}
	
	public inline function getInt16 (pos:Int) {
		return bytes.getInt16(pos);
	}
}

#end