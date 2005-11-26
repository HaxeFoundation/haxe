package flash.filters;

extern class BevelFilter extends BitmapFilter {

	var type : String;
	var blurX : Float;
	var blurY : Float;
	var knockout : Bool;
	var strength : Float;
	var quality : Float;
	var shadowAlpha : Float;
	var shadowColor : Float;
	var highlightAlpha : Float;
	var highlightColor : Float;
	var angle : Float;
	var distance : Float;

	function new(distance : Float, angle : Float, highlightColor : Float, highlightAlpha : Float, shadowColor : Float, shadowAlpha : Float, blurX : Float, blurY : Float, strength : Float, quality : Float, type : String, knockout : Bool) : Void;
	function clone() : BevelFilter;

}
