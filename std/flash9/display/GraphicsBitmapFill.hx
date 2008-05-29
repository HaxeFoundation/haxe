package flash.display;

extern class GraphicsBitmapFill implements IGraphicsFill, implements IGraphicsData {

	var matrix : flash.geom.Matrix;
	var bitmapData : BitmapData;
	var repeat : Bool;
	var smooth : Bool;

	function new( ?bitmapData : BitmapData, ?matrix : flash.geom.Matrix, ?repeat : Bool, ?smooth : Bool ) : Void;
}
