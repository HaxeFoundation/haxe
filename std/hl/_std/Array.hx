
@:coreApi
class Array<T> {

	var array : hl.types.ArrayObject<T>;
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

	public function toString() : String {
		throw "TODO";
		return null;
	}

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
	
}
