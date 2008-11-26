package flash;

/**
	The Vector class is very similar to Array but is only supported by the Flash Player 10+
**/
extern class Vector<T> implements ArrayAccess<T> {

	var length : UInt;
	var fixed : Bool;

	function new( ?length : UInt, ?fixed : Bool ) : Void;
	function concat( a : Vector<T> ) : Vector<T>;
	function join( sep : String ) : String;
	function pop() : Null<T>;
	function push(x : T) : Int;
	function reverse() : Void;
	function shift() : Null<T>;
	function unshift( x : T ) : Void;
	function slice( pos : Int, ?end : Int ) : Vector<T>;
	function sort( f : T -> T -> Int ) : Void;
	function splice( pos : Int, len : Int ) : Vector<T>;
	function toString() : String;

	function iterator() : Iterator<T>;

	function indexOf( x : T, ?from : Int ) : Int;
	function lastIndexOf( x : T, ?from : Int ) : Int;

}
