package flash.filters;

extern class ConvolutionFilter extends BitmapFilter {

	var alpha : Float;
	var color : Float;
	var clamp : Bool;
	var preserveAlpha : Bool;
	var bias : Float;
	var divisor : Float;
	var matrix : Array<Dynamic>;
	var matrixX : Float;
	var matrixY : Float;

	function new( ?matrixX : Float, ?matrixY : Float, ?matrix : Array<Dynamic>, ?divisor : Float, ?bias : Float, ?preserveAlpha : Bool, ?clamp : Bool, ?color : Float, ?alpha : Float) : Void;
	function clone() : ConvolutionFilter;

}