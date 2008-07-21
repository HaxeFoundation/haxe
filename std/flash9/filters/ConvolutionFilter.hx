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
	function new(?matrixX : Float, ?matrixY : Float, ?matrix : Array<Dynamic>, ?divisor : Float, ?bias : Float, ?preserveAlpha : Bool, ?clamp : Bool, ?color : UInt, ?alpha : Float) : Void;
}
