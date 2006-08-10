package flash.geom;

extern class Rectangle {
	function new(?x : Float, ?y : Float, ?width : Float, ?height : Float) : Void;
	var bottom : Float;
	var bottomRight : flash.geom.Point;
	function clone() : flash.geom.Rectangle;
	function contains(x : Float, y : Float) : Bool;
	function containsPoint(point : flash.geom.Point) : Bool;
	function containsRect(rect : flash.geom.Rectangle) : Bool;
	function equals(toCompare : flash.geom.Rectangle) : Bool;
	var height : Float;
	function inflate(dx : Float, dy : Float) : Void;
	function inflatePoint(point : flash.geom.Point) : Void;
	function intersection(toIntersect : flash.geom.Rectangle) : flash.geom.Rectangle;
	function intersects(toIntersect : flash.geom.Rectangle) : Bool;
	function isEmpty() : Bool;
	var left : Float;
	function offset(dx : Float, dy : Float) : Void;
	function offsetPoint(point : flash.geom.Point) : Void;
	var right : Float;
	function setEmpty() : Void;
	var size : flash.geom.Point;
	function toString() : String;
	var top : Float;
	var topLeft : flash.geom.Point;
	function union(toUnion : flash.geom.Rectangle) : flash.geom.Rectangle;
	var width : Float;
	var x : Float;
	var y : Float;
}
