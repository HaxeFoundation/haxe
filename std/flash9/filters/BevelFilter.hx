package flash.filters;

extern class BevelFilter extends BitmapFilter {
	var angle : Float;
	var blurX : Float;
	var blurY : Float;
	var distance : Float;
	var highlightAlpha : Float;
	var highlightColor : UInt;
	var knockout : Bool;
	var quality : Int;
	var shadowAlpha : Float;
	var shadowColor : UInt;
	var strength : Float;
	var type : BitmapFilterType;
	function new(?distance : Float, ?angle : Float, ?highlightColor : UInt, ?highlightAlpha : Float, ?shadowColor : UInt, ?shadowAlpha : Float, ?blurX : Float, ?blurY : Float, ?strength : Float, ?quality : Int, ?type : BitmapFilterType, ?knockout : Bool) : Void;
}
