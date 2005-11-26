extern class Array<T> {

	var length : Int;

	function new() : Void;

	function concat( a : Array<T> ) : Array<T>;
	function join( sep : String ) : String;
	function pop() : T;
	function push(x : T) : Int;
	function reverse() : Array<T>;
	function shift() : T;
	function slice( pos : Int, end : Int ) : Array<T>; // sub
	function sort( f : T -> T -> Int ) : Void;
	function splice( pos : Int, len : Int ) : Array<T>; // removed elts
	// no toSource (js specific)
	function toString() : String;
	function unshift( x : T ) : Void;
	// no valueOf (js specific)

	/** added **/
	function insert( pos : Int, x : T ) : Void;
	function remove( x : T ) : Bool;
	function copy() : Array<T>;

}
