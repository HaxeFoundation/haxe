package hl.types;

@:keep
class ArrayF64 {

	var bytes : hl.types.Bytes;
	var size : Int;
	public var length(default,null) : Int;

	public function new() {
		size = length = 0;
		bytes = new Bytes(0);
	}

	public function concat( a : ArrayF64 ) : ArrayF64 {
		throw "TODO";
		return null;
	}

	public function join( sep : String ) : String {
		throw "TODO";
		return null;
	}

	public function pop() : Null<Float> {
		if( length == 0 )
			return null;
		length--;
		return bytes.getF64(length << 3);
	}

	public function push(x : Float) : Int {
		var len = length;
		if( size == len )
			__expand(len);
		else
			length++;
		bytes.setF64(len<<3,x);
		return length;
	}

	public function reverse() : Void {
		throw "TODO";
	}

	public function shift() : Null<Float> {
		throw "TODO";
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : ArrayF64 {
		throw "TODO";
		return null;
	}

	public function sort( f : Float -> Float -> Int ) : Void {
		throw "TODO";
	}

	public function splice( pos : Int, len : Int ) : ArrayF64 {
		throw "TODO";
		return null;
	}

	public function toString() : String {
		var b = new StringBuf();
		b.addChar("[".code);
		for( i in 0...length ) {
			if( i > 0 ) b.addChar(",".code);
			b.add(bytes.getF64(i<<3));
		}
		b.addChar("]".code);
		return b.toString();
	}

	public function unshift( x : Float ) : Void {
		throw "TODO";
	}

	public function insert( pos : Int, x : Float ) : Void {
		throw "TODO";
	}

	public function remove( x : Float ) : Bool {
		throw "TODO";
		return false;
	}

	public function indexOf( x : Float, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function lastIndexOf( x : Float, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function copy() : ArrayF64 {
		throw "TODO";
		return null;
	}

	public function iterator() : Iterator<Float> {
		throw "TODO";
		return null;
	}

	public function map<S>( f : Float -> S ) : ArrayObj<S> {
		throw "TODO";
		return null;
	}

	public function filter( f : Float -> Bool ) : ArrayF64 {
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
			var bytes2 = new hl.types.Bytes(next<<3);
			if( length > 0 ) bytes2.blit(0,bytes,0,length<<3);
			bytes = bytes2;
			size = next;
		}
		length = newlen;
	}
	
	public static function alloc( a : hl.types.Bytes, length : Int ) {
		var arr : ArrayF64 = untyped $new(ArrayF64);
		arr.bytes = a;
		arr.length = length;
		arr.size = length;
		return arr;
	}

}
