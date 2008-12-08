package flash.geom;

extern class Vector3D {

	static var XAXIS(default,null) : Vector3D;
	static var YAXIS(default,null) : Vector3D;
	static var ZAXIS(default,null) : Vector3D;

	var x : Float;
	var y : Float;
	var z : Float;
	var w : Float;

	var lengthSquared(default,null) : Float;
	var length(default,null) : Float;

	function new( ?ax:Float, ?ay:Float, ?az:Float, ?aw:Float ) : Void;
	function scaleBy( s : Float ) : Void;
	function negate() : Void;
	function nearEquals( ?toCompare : Vector3D, ?tolerance : Float, ?allFour : Bool ) : Bool;
	function decrementBy( a : Vector3D ) : Void;
	function normalize() : Float;
	function crossProduct( a : Vector3D) : Vector3D;
	function subtract( a : Vector3D) : Vector3D;
	function project() : Void;
	function clone() : Vector3D;
	function dotProduct( a : Vector3D ) : Float;
	function add( a : Vector3D ) : Vector3D;
	function toString() : String;
	function incrementBy( a : Vector3D ) : Void;
	function equals( ?toCompare:Vector3D, ?allFour:Bool ) : Bool;

	static function angleBetween( a : Vector3D, b : Vector3D ) : Float;
	static function distance( pt1 : Vector3D, pt2 : Vector3D ) : Float;

}
