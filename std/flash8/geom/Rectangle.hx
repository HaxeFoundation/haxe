package flash.geom;

extern class Rectangle<T> {

	var left : T;
	var top : T;
	var right : T;
	var bottom : T;

	// OR
	var x : T;
	var y : T;
	var width : T;
	var height : T;

	// OR
	var size : Point<T>;
	var bottomRight : Point<T>;
	var topLeft : Point<T>;

	function new( x : T, y : T, w : T, h : T ) : Void;

	function equals( r : Rectangle<T> ) : Bool;
	function union( r : Rectangle<T> ) : Rectangle<T>;
	function intersects( r : Rectangle<T> ) : Bool;
	function intersection( r : Rectangle<T> ) : Rectangle<T>;
	function containsRectangle( r : Rectangle<T> ) : Bool;
	function containsPoint( p : Point<T> ) : Bool;
	function contains( x : Float, y : Float ) : Bool;
	function offsetPoint( p : Point<T> ) : Void;
	function offset( x : T, y : T ) : Void;

	function inflatePoint( p : Point<T> ) : Void;
	function inflate( x : T, y : T ) : Void;
	function isEmpty() : Bool;
	function setEmpty() : Void;
	function clone() : Rectangle<T>;

	function toString() : String;

}