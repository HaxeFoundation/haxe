
@:coreApi
class String {

	var bytes : hl.types.Bytes;
	var size : Int;
	public var length(default,null) : Int;

	public function new(string:String) : Void {
		bytes = string.bytes;
		length = string.length;
		size = string.size;
	}

	public function toUpperCase() : String {
		throw "TODO";
		return null;
	}

	public function toLowerCase() : String {
		throw "TODO";
		return null;
	}

	public function charAt(index : Int) : String {
		throw "TODO";
		return null;
	}

	public function charCodeAt( index : Int) : Null<Int> {
		var idx : UInt = index;
		if( idx >= length )
			return null;
		return @:privateAccess bytes.utf8Char(0,index);
	}

	public function indexOf( str : String, ?startIndex : Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function lastIndexOf( str : String, ?startIndex : Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function split( delimiter : String ) : Array<String> {
		throw "TODO";
		return null;
	}

	public function substr( pos : Int, ?len : Int ) : String @:privateAccess {
		var sl = length;
		var len = if( len == null ) sl else len;
		if( len == 0 ) return "";

		if( pos != 0 && len < 0 )
			return "";

		if( pos < 0 ){
			pos = sl + pos;
			if( pos < 0 ) pos = 0;
		}else if( len < 0 ){
			len = sl + len - pos;
		}

		if( pos + len > sl ){
			len = sl - pos;
		}

		if( pos < 0 || len <= 0 ) return "";

		var bytes = bytes;
		var start = pos == 0 ? 0 : bytes.utf8Pos(0, pos);
		var end = pos + len == sl ? size : bytes.utf8Pos(start, len);
		var size = end - start;
		var b = new hl.types.Bytes(size + 1);
		b.blit(0, bytes, start, size);
		b[size] = 0;
		return __alloc__(b, size, len);
	}

	public function substring( startIndex : Int, ?endIndex : Int ) : String {
		throw "TODO";
		return null;
	}

	public function toString() : String {
		return this;
	}

	public static function fromCharCode( code : Int ) : String {
		throw "TODO";
		return null;
	}

	@:keep function __string() : hl.types.Bytes {
		return bytes;
	}

	@:keep function __compare( s : String ) : Int {
		var v = bytes.compare(0, s.bytes, 0, size < s.size ? size : s.size);
		return v == 0 ? size - s.size : v;
	}

	@:keep static inline function __alloc__( b : hl.types.Bytes, blen : Int, clen : Int ) : String {
		var s : String = untyped $new(String);
		s.bytes = b;
		s.length = clen;
		s.size = blen;
		return s;
	}

	@:keep static function __add__( a : String, b : String ) : String {
		if( a == null ) a = "null";
		if( b == null ) b = "null";
		var asize = a.size, bsize = b.size, tot = asize + bsize;
		var bytes = new hl.types.Bytes(tot+1);
		bytes.blit(0,a.bytes,0,asize);
		bytes.blit(asize,b.bytes,0,bsize);
		bytes[tot] = 0;
		return __alloc__(bytes, tot, a.length + b.length);
	}
}
