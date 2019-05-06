package flash.geom;

@:require(flash10) extern class Vector3D {
	@:flash.property var length(get,never) : Float;
	@:flash.property var lengthSquared(get,never) : Float;
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
	private function get_length() : Float;
	private function get_lengthSquared() : Float;
	function incrementBy(a : Vector3D) : Void;
	function nearEquals(toCompare : Vector3D, tolerance : Float, allFour : Bool = false) : Bool;
	function negate() : Void;
	function normalize() : Float;
	function project() : Void;
	function scaleBy(s : Float) : Void;
	@:require(flash11) function setTo(xa : Float, ya : Float, za : Float) : Void;
	function subtract(a : Vector3D) : Vector3D;
	function toString() : String;
	static final X_AXIS : Vector3D;
	static final Y_AXIS : Vector3D;
	static final Z_AXIS : Vector3D;
	static function angleBetween(a : Vector3D, b : Vector3D) : Float;
	static function distance(pt1 : Vector3D, pt2 : Vector3D) : Float;
}
