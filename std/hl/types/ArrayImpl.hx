package hl.types;

@:keep
class ArrayImpl<T> {

	var array : hl.types.ArrayObject<Dynamic>;
	public var length(default,null) : Int;

	public function new() {
	}

	public function concat( a : Array<T> ) : Array<T> {
		throw "TODO";
		return null;
	}

	public function join( sep : String ) : String {
		throw "TODO";
		return null;
	}

	public function pop() : Null<T> {
		throw "TODO";
		return null;
	}

	public function push(x : T) : Int {
		throw "TODO";
		return length;
	}

	public function reverse() : Void {
		throw "TODO";
	}

	public function shift() : Null<T> {
		throw "TODO";
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : Array<T> {
		throw "TODO";
		return null;
	}

	public function sort( f : T -> T -> Int ) : Void {
		throw "TODO";
	}

	public function splice( pos : Int, len : Int ) : Array<T> {
		throw "TODO";
		return null;
	}

	/*
	public function toString() : String {
		var b = new StringBuf();
		b.addChar("[".code);
		for( i in 0...length ) {
			if( i > 0 ) b.addChar(",".code);
			b.add(array[i]);
		}
		b.addChar("]".code);
		return b.toString();
	}*/

	public function unshift( x : T ) : Void {
		throw "TODO";
	}

	public function insert( pos : Int, x : T ) : Void {
		throw "TODO";
	}

	public function remove( x : T ) : Bool {
		throw "TODO";
		return false;
	}

	public function indexOf( x : T, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function lastIndexOf( x : T, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function copy() : Array<T> {
		throw "TODO";
		return null;
	}

	public function iterator() : Iterator<T> {
		throw "TODO";
		return null;
	}

	public function map<S>( f : T -> S ) : Array<S> {
		throw "TODO";
		return null;
	}

	public function filter( f : T -> Bool ) : Array<T> {
		throw "TODO";
		return null;
	}
	
	// called by compiler when accessing the array outside of its bounds, might trigger resize
	function __expand( index : Int ) {
		if( index < 0 ) throw "Invalid array access";
		var newlen = index + 1;
		var size : Int = array.length; 
		if( newlen > size ) {
			var next = (size * 3) >> 1;
			if( next < newlen ) next = newlen;
			var arr2 = new hl.types.ArrayObject<Dynamic>(next);
			arr2.blit(0,array,0,length);
			array = arr2;
		}
		length = newlen;
	}
	
	public static function alloc( a : hl.types.ArrayObject<Dynamic> ) {
		var arr : ArrayImpl<Dynamic> = untyped $new(ArrayImpl);
		arr.array = a;
		arr.length = a.length;
		return arr;
	}

}
