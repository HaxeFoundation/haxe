package flash.geom;

extern class Matrix {
	function new(?a : Float, ?b : Float, ?c : Float, ?d : Float, ?tx : Float, ?ty : Float) : Void;
	var a : Float;
	var b : Float;
	var c : Float;
	function clone() : flash.geom.Matrix;
	function concat(m : flash.geom.Matrix) : Void;
	function createBox(scaleX : Float, scaleY : Float, ?rotation : Float, ?tx : Float, ?ty : Float) : Void;
	function createGradientBox(width : Float, height : Float, ?rotation : Float, ?tx : Float, ?ty : Float) : Void;
	var d : Float;
	function deltaTransformPoint(point : flash.geom.Point) : flash.geom.Point;
	function identity() : Void;
	function invert() : Void;
	function rotate(angle : Float) : Void;
	function scale(sx : Float, sy : Float) : Void;
	function toString() : String;
	function transformPoint(point : flash.geom.Point) : flash.geom.Point;
	function translate(dx : Float, dy : Float) : Void;
	var tx : Float;
	var ty : Float;
}
