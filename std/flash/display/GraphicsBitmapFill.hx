package flash.display;

@:final extern class GraphicsBitmapFill implements IGraphicsData implements IGraphicsFill {
	var bitmapData : BitmapData;
	var matrix : flash.geom.Matrix;
	var repeat : Bool;
	var smooth : Bool;
	function new(?bitmapData : BitmapData, ?matrix : flash.geom.Matrix, repeat : Bool = true, smooth : Bool = false) : Void;
}
