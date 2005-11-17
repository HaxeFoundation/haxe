native class Array<T> {

	public var length : Int;

	public function new() { }
	function push(x : T) : Void { }
	function pop() : T { }
	function unshift( x : T ) : Void { }

	function join( sep : String ) : String { }
	function toString() : String { }

	function sort( f : T -> T -> Int ) : Void { }
	function insert( pos : Int, x : T ) : Void { }


}
