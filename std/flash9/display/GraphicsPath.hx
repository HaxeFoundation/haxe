package flash.display;

extern class GraphicsPath implements IGraphicsPath, implements IGraphicsData {

	var data : flash.Vector<Float>;
	var commands : flash.Vector<Int>;
	var winding : GraphicsPathWinding;

	function new( ?commands : flash.Vector<Int>, ?data : flash.Vector<Float>, ?winding : GraphicsPathWinding ) : Void;
	function wideLineTo( x:Float, y:Float ) : Void;
	function wideMoveTo( x:Float, y:Float ) : Void;
	function curveTo( controlX:Float, controlY:Float, anchorX:Float, anchorY:Float ) : Void;
	function moveTo( x:Float, y:Float ) : Void;
	function lineTo( x:Float, y:Float ) : Void;
}
