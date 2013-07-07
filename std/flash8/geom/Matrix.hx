package flash.geom;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class Matrix {

	// 3x2 affine 2D matrix
	var a : Float;
	var b : Float;
	var c : Float;
	var d : Float;
	var tx : Float;
	var ty : Float;

	function new( ?a : Float, ?b : Float, ?c : Float, ?d : Float, ?tx : Float, ?ty : Float) : Void;

	function transformPoint( p : Point<Float> ) : Point<Float>;
	function deltaTransformPoint( p : Point<Float> ) : Point<Float>;
	function toString() : String;
	function scale( sx : Float, sy : Float ) : Void;
	function translate( tx : Float, ty : Float ) : Void;
	function rotate( r : Float ) : Void;
	function identity() : Void;
	function invert() : Void;
	function concat( m : Matrix ) : Void;
	function clone() : Matrix;

	function createGradientBox( width : Float, height : Float, ?rot : Float, ?tx : Float, ?ty : Float ) : Void;
	function createBox( scalex : Float, scaley : Float, ?rot : Float, ?tx : Float, ?ty : Float ) : Void;


}