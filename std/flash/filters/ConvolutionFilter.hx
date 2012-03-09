package flash.filters;

extern class ConvolutionFilter extends BitmapFilter {
	var alpha : Float;
	var bias : Float;
	var clamp : Bool;
	var color : UInt;
	var divisor : Float;
	var matrix : Array<Dynamic>;
	var matrixX : Float;
	var matrixY : Float;
	var preserveAlpha : Bool;
	function new(matrixX : Float = 0, matrixY : Float = 0, ?matrix : Array<Dynamic>, divisor : Float = 1, bias : Float = 0, preserveAlpha : Bool = true, clamp : Bool = true, color : UInt = 0, alpha : Float = 0) : Void;
}
