package flash.filters;

extern class GlowFilter extends flash.filters.BitmapFilter {
	function new(?color : UInt, ?alpha : Float, ?blurX : Float, ?blurY : Float, ?strength : Float, ?quality : Int, ?inner : Bool, ?knockout : Bool) : Void;
	var alpha : Float;
	var blurX : Float;
	var blurY : Float;
	var color : UInt;
	var inner : Bool;
	var knockout : Bool;
	var quality : Int;
	var strength : Float;
}
