package flash.filters;

extern class GlowFilter extends BitmapFilter {

	var blurX : Float;
	var blurY : Float;
	var knockout : Bool;
	var strength : Float;
	var quality : Float;
	var inner : Bool;
	var alpha : Float;
	var color : Float;

	function new(color : Float, alpha : Float, blurX : Float, blurY : Float, strength : Float, quality : Float, inner : Bool, knockout : Bool) : Void;
	function clone() : GlowFilter;

}
