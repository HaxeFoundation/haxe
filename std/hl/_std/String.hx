
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
		return @:privateAccess bytes.utf8Char(0,size,index);
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

	public function substr( pos : Int, ?len : Int ) : String {
		throw "TODO";
		return null;
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
