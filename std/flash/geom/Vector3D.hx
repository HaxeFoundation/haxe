package flash.geom;

@:require(flash10) extern class Vector3D {
	var length(default,never) : Float;
	var lengthSquared(default,never) : Float;
	var w : Float;
	var x : Float;
	var y : Float;
	var z : Float;
	function new(x : Float = 0, y : Float = 0, z : Float = 0, w : Float = 0) : Void;
	function add(a : Vector3D) : Vector3D;
	function clone() : Vector3D;
	@:require(flash11) function copyFrom(sourceVector3D : Vector3D) : Void;
	function crossProduct(a : Vector3D) : Vector3D;
	function decrementBy(a : Vector3D) : Void;
	function dotProduct(a : Vector3D) : Float;
	function equals(toCompare : Vector3D, allFour : Bool = false) : Bool;
	function incrementBy(a : Vector3D) : Void;
	function nearEquals(toCompare : Vector3D, tolerance : Float, allFour : Bool = false) : Bool;
	function negate() : Void;
	function normalize() : Float;
	function project() : Void;
	function scaleBy(s : Float) : Void;
	@:require(flash11) function setTo(xa : Float, ya : Float, za : Float) : Void;
	function subtract(a : Vector3D) : Vector3D;
	function toString() : String;
	static var X_AXIS(default,never) : Vector3D;
	static var Y_AXIS(default,never) : Vector3D;
	static var Z_AXIS(default,never) : Vector3D;
	static function angleBetween(a : Vector3D, b : Vector3D) : Float;
	static function distance(pt1 : Vector3D, pt2 : Vector3D) : Float;
}
