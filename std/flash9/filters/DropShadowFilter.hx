package flash.filters;

extern class DropShadowFilter extends flash.filters.BitmapFilter {
	function new(?distance : Float, ?angle : Float, ?color : UInt, ?alpha : Float, ?blurX : Float, ?blurY : Float, ?strength : Float, ?quality : Int, ?inner : Bool, ?knockout : Bool, ?hideObject : Bool) : Void;
	var alpha : Float;
	var angle : Float;
	var blurX : Float;
	var blurY : Float;
	var color : UInt;
	var distance : Float;
	var hideObject : Bool;
	var inner : Bool;
	var knockout : Bool;
	var quality : Int;
	var strength : Float;
}
