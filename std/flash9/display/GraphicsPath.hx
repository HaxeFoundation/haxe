package flash.display;

@:final extern class GraphicsPath implements IGraphicsData, implements IGraphicsPath {
	var commands : flash.Vector<Int>;
	var data : flash.Vector<Float>;
	var winding : GraphicsPathWinding;
	function new(?commands : flash.Vector<Int>, ?data : flash.Vector<Float>, ?winding : GraphicsPathWinding) : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
	function lineTo(x : Float, y : Float) : Void;
	function moveTo(x : Float, y : Float) : Void;
	function wideLineTo(x : Float, y : Float) : Void;
	function wideMoveTo(x : Float, y : Float) : Void;
}
