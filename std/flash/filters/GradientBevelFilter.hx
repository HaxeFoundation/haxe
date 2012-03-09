package flash.filters;

@:final extern class GradientBevelFilter extends BitmapFilter {
	var alphas : Array<Dynamic>;
	var angle : Float;
	var blurX : Float;
	var blurY : Float;
	var colors : Array<Dynamic>;
	var distance : Float;
	var knockout : Bool;
	var quality : Int;
	var ratios : Array<Dynamic>;
	var strength : Float;
	var type : String;
	function new(distance : Float = 4, angle : Float = 45, ?colors : Array<Dynamic>, ?alphas : Array<Dynamic>, ?ratios : Array<Dynamic>, blurX : Float = 4, blurY : Float = 4, strength : Float = 1, quality : Int = 1, ?type : String, knockout : Bool = false) : Void;
}
