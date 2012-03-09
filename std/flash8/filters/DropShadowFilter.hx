package flash.filters;

extern class DropShadowFilter extends BitmapFilter {

	var hideObject : Bool;
	var blurX : Float;
	var blurY : Float;
	var knockout : Bool;
	var strength : Float;
	var inner : Bool;
	var quality : Float;
	var alpha : Float;
	var color : Float;
	var angle : Float;
	var distance : Float;

	function new( ?distance : Float, ?angle : Float, ?color : Float, ?alpha : Float, ?blurX : Float, ?blurY : Float, ?strength : Float, ?quality : Float, ?inner : Bool, ?knockout : Bool, ?hideObject : Bool ) : Void;
	function clone() : DropShadowFilter;

}
