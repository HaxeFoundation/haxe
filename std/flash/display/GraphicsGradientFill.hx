package flash.display;

@:final extern class GraphicsGradientFill implements IGraphicsData implements IGraphicsFill {
	var alphas : Array<Float>;
	var colors : Array<UInt>;
	var focalPointRatio : Float;
	var interpolationMethod : InterpolationMethod;
	var matrix : flash.geom.Matrix;
	var ratios : Array<Float>;
	var spreadMethod : SpreadMethod;
	var type : GradientType;
	function new(?type : GradientType, ?colors : Array<UInt>, ?alphas : Array<Float>, ?ratios : Array<Float>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, focalPointRatio : Float = 0) : Void;
}
