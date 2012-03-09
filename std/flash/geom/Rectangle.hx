package flash.geom;

extern class Rectangle {
	var bottom : Float;
	var bottomRight : Point;
	var height : Float;
	var left : Float;
	var right : Float;
	var size : Point;
	var top : Float;
	var topLeft : Point;
	var width : Float;
	var x : Float;
	var y : Float;
	function new(x : Float = 0, y : Float = 0, width : Float = 0, height : Float = 0) : Void;
	function clone() : Rectangle;
	function contains(x : Float, y : Float) : Bool;
	function containsPoint(point : Point) : Bool;
	function containsRect(rect : Rectangle) : Bool;
	@:require(flash11) function copyFrom(sourceRect : Rectangle) : Void;
	function equals(toCompare : Rectangle) : Bool;
	function inflate(dx : Float, dy : Float) : Void;
	function inflatePoint(point : Point) : Void;
	function intersection(toIntersect : Rectangle) : Rectangle;
	function intersects(toIntersect : Rectangle) : Bool;
	function isEmpty() : Bool;
	function offset(dx : Float, dy : Float) : Void;
	function offsetPoint(point : Point) : Void;
	function setEmpty() : Void;
	@:require(flash11) function setTo(xa : Float, ya : Float, widtha : Float, heighta : Float) : Void;
	function toString() : String;
	function union(toUnion : Rectangle) : Rectangle;
}
