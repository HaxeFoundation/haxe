package hl.types;

@:keep
class ArrayI32 {

	var bytes : hl.types.Bytes;
	var size : Int;
	public var length(default,null) : Int;

	public function new() {
		size = length = 0;
		bytes = new Bytes(0);
	}

	public function concat( a : ArrayI32 ) : ArrayI32 {
		throw "TODO";
		return null;
	}

	public function join( sep : String ) : String {
		throw "TODO";
		return null;
	}

	public function pop() : Null<Int> {
		throw "TODO";
		return null;
	}

	public function push(x : Int) : Int {
		throw "TODO";
		return length;
	}

	public function reverse() : Void {
		throw "TODO";
	}

	public function shift() : Null<Int> {
		throw "TODO";
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : ArrayI32 {
		throw "TODO";
		return null;
	}

	public function sort( f : Int -> Int -> Int ) : Void {
		throw "TODO";
	}

	public function splice( pos : Int, len : Int ) : ArrayI32 {
		throw "TODO";
		return null;
	}

	public function toString() : String {
		var b = new StringBuf();
		b.addChar("[".code);
		for( i in 0...length ) {
			if( i > 0 ) b.addChar(",".code);
			b.add(bytes.getI32(i<<2));
		}
		b.addChar("]".code);
		return b.toString();
	}

	public function unshift( x : Int ) : Void {
		throw "TODO";
	}

	public function insert( pos : Int, x : Int ) : Void {
		throw "TODO";
	}

	public function remove( x : Int ) : Bool {
		throw "TODO";
		return false;
	}

	public function indexOf( x : Int, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function lastIndexOf( x : Int, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function copy() : ArrayI32 {
		throw "TODO";
		return null;
	}

	public function iterator() : Iterator<Int> {
		throw "TODO";
		return null;
	}

	public function map<S>( f : Int -> S ) : ArrayImpl<S> {
		throw "TODO";
		return null;
	}

	public function filter( f : Int -> Bool ) : ArrayI32 {
		throw "TODO";
		return null;
	}
	
	// called by compiler when accessing the array outside of its bounds, might trigger resize
	function __expand( index : Int ) {
		if( index < 0 ) throw "Invalid array access";
		var newlen = index + 1;
		if( newlen > size ) {
			var next = (size * 3) >> 1;
			if( next < newlen ) next = newlen;
			var bytes2 = new hl.types.Bytes(next<<2);
			if( length > 0 ) bytes2.blit(0,bytes,0,length<<2);
			bytes = bytes2;
			size = next;
		}
		length = newlen;
	}
	
	public static function alloc( a : hl.types.Bytes, length : Int ) {
		var arr : ArrayI32 = untyped $new(ArrayI32);
		arr.bytes = a;
		arr.length = length;
		arr.size = length>>2;
		return arr;
	}

}
