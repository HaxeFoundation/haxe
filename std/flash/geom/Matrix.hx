package flash.geom;

extern class Matrix {

	// 3x2 affine 2D matrix
	var a : Float;
	var b : Float;
	var c : Float;
	var d : Float;
	var tx : Float;
	var ty : Float;

	function new(a : Float, b : Float, c : Float, d : Float, tx : Float, ty : Float) : Void;

	function transformPoint( p : Point<Float> ) : Point<Float>;
	function deltaTransformPoint( p : Point<Float> ) : Void; // does not apply translation
	function toString() : String;
	function scale( sx : Float, sy : Float ) : Void;
	function translate( tx : Float, ty : Float ) : Void;
	function rotate( r : Float ) : Void;
	function identity() : Void;
	function invert() : Void;
	function concat( m : Matrix ) : Void;
	function clone() : Matrix;

	function createGradientBox( width : Float, height : Float, rot : Float, tx : Float, ty : Float ) : Void;
	function createBox( scalex : Float, scaley : Float, rot : Float, tx : Float, ty : Float ) : Void;


}