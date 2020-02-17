package flash.display;

extern final class GraphicsGradientFill implements IGraphicsData implements IGraphicsFill {
	var alphas : Array<Float>;
	var colors : Array<UInt>;
	var focalPointRatio : Float;
	@:flash.property var interpolationMethod(get,set) : InterpolationMethod;
	var matrix : flash.geom.Matrix;
	var ratios : Array<Float>;
	@:flash.property var spreadMethod(get,set) : SpreadMethod;
	@:flash.property var type(get,set) : GradientType;
	function new(?type : GradientType, ?colors : Array<UInt>, ?alphas : Array<Float>, ?ratios : Array<Float>, ?matrix : flash.geom.Matrix, ?spreadMethod : SpreadMethod, ?interpolationMethod : InterpolationMethod, focalPointRatio : Float = 0) : Void;
	private function get_interpolationMethod() : InterpolationMethod;
	private function get_spreadMethod() : SpreadMethod;
	private function get_type() : GradientType;
	private function set_interpolationMethod(value : InterpolationMethod) : InterpolationMethod;
	private function set_spreadMethod(value : SpreadMethod) : SpreadMethod;
	private function set_type(value : GradientType) : GradientType;
}
