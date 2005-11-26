package flash.filters;

extern class GradientGlowFilter extends BitmapFilter {

	var type : String;
	var knockout : Bool;
	var strength : Float;
	var quality : Float;
	var blurX : Float;
	var blurY : Float;
	var ratios : Array<Float>;
	var alphas : Array<Float>;
	var colors : Array<Float>;
	var angle : Float;
	var distance : Float;

	function new(distance : Float, angle : Float, colors : Array<Dynamic>, alphas : Array<Dynamic>, ratios : Array<Dynamic>, blurX : Float, blurY : Float, strength : Float, quality : Float, type : String, knockout : Bool) : Void;
	function clone() : GradientGlowFilter;

}