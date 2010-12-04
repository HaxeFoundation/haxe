package flash.geom;

extern class Point {
	var length(default,null) : Float;
	var x : Float;
	var y : Float;
	function new(x : Float = 0, y : Float = 0) : Void;
	function add(v : Point) : Point;
	function clone() : Point;
	function equals(toCompare : Point) : Bool;
	function normalize(thickness : Float) : Void;
	function offset(dx : Float, dy : Float) : Void;
	function subtract(v : Point) : Point;
	function toString() : String;
	static function distance(pt1 : Point, pt2 : Point) : Float;
	static function interpolate(pt1 : Point, pt2 : Point, f : Float) : Point;
	static function polar(len : Float, angle : Float) : Point;
}
