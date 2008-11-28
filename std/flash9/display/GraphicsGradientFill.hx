package flash.display;

extern class GraphicsGradientFill implements IGraphicsFill, implements IGraphicsData {
	var type : GradientType;
	var colors : Array<UInt>;
	var alphas : Array<Float>;
	var ratios : Array<Float>;
	var matrix : flash.geom.Matrix;
	var spreadMethod : SpreadMethod;
	var interpolationMethod : InterpolationMethod;
	var focalPointRatio : Float;
	function new( ?type : GradientType, ?colors : Array<UInt>, ?alphas : Array<Float>, ?ratios : Array<Float>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, ?focalPointRatio : Float) : Void;
}
