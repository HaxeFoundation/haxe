package flash.geom;

extern class Point {
	function new(?x : Float, ?y : Float) : Void;
	function add(v : flash.geom.Point) : flash.geom.Point;
	function clone() : flash.geom.Point;
	function equals(toCompare : flash.geom.Point) : Bool;
	var length(default,null) : Float;
	function normalize(thickness : Float) : Void;
	function offset(dx : Float, dy : Float) : Void;
	function subtract(v : flash.geom.Point) : flash.geom.Point;
	function toString() : String;
	var x : Float;
	var y : Float;
	static function distance(pt1 : flash.geom.Point, pt2 : flash.geom.Point) : Float;
	static function interpolate(pt1 : flash.geom.Point, pt2 : flash.geom.Point, f : Float) : flash.geom.Point;
	static function polar(len : Float, angle : Float) : flash.geom.Point;
}
