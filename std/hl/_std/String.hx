
@:coreApi
class String {

	var bytes : hl.types.Bytes;
	public var length(default,null) : Int;

	public function new(string:String) : Void {
		bytes = string.bytes;
		length = string.length;
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
		throw "TODO";
		return null;
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
	
	@:keep static function alloc( b : hl.types.Bytes, len : Int ) : String {
		var s : String = untyped $new(String);
		s.bytes = b;
		s.length = len;
		return s;
	}
	
}
