package flash.display;

extern final class GraphicsPath implements IGraphicsData implements IGraphicsPath {
	var commands : flash.Vector<Int>;
	var data : flash.Vector<Float>;
	@:flash.property var winding(get,set) : GraphicsPathWinding;
	function new(?commands : flash.Vector<Int>, ?data : flash.Vector<Float>, ?winding : GraphicsPathWinding) : Void;
	@:require(flash11) function cubicCurveTo(controlX1 : Float, controlY1 : Float, controlX2 : Float, controlY2 : Float, anchorX : Float, anchorY : Float) : Void;
	function curveTo(controlX : Float, controlY : Float, anchorX : Float, anchorY : Float) : Void;
	private function get_winding() : GraphicsPathWinding;
	function lineTo(x : Float, y : Float) : Void;
	function moveTo(x : Float, y : Float) : Void;
	private function set_winding(value : GraphicsPathWinding) : GraphicsPathWinding;
	function wideLineTo(x : Float, y : Float) : Void;
	function wideMoveTo(x : Float, y : Float) : Void;
}
