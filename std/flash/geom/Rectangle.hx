package flash.geom;

extern class Rectangle {
	var bottom(get,set) : Float;
	var bottomRight(get,set) : Point;
	var height : Float;
	var left(get,set) : Float;
	var right(get,set) : Float;
	var size(get,set) : Point;
	var top(get,set) : Float;
	var topLeft(get,set) : Point;
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
	private function get_bottom() : Float;
	private function get_bottomRight() : Point;
	private function get_left() : Float;
	private function get_right() : Float;
	private function get_size() : Point;
	private function get_top() : Float;
	private function get_topLeft() : Point;
	function inflate(dx : Float, dy : Float) : Void;
	function inflatePoint(point : Point) : Void;
	function intersection(toIntersect : Rectangle) : Rectangle;
	function intersects(toIntersect : Rectangle) : Bool;
	function isEmpty() : Bool;
	function offset(dx : Float, dy : Float) : Void;
	function offsetPoint(point : Point) : Void;
	function setEmpty() : Void;
	@:require(flash11) function setTo(xa : Float, ya : Float, widtha : Float, heighta : Float) : Void;
	private function set_bottom(value : Float) : Float;
	private function set_bottomRight(value : Point) : Point;
	private function set_left(value : Float) : Float;
	private function set_right(value : Float) : Float;
	private function set_size(value : Point) : Point;
	private function set_top(value : Float) : Float;
	private function set_topLeft(value : Point) : Point;
	function toString() : String;
	function union(toUnion : Rectangle) : Rectangle;
}
