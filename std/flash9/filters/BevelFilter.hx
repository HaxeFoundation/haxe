package flash.filters;

@:final extern class BevelFilter extends BitmapFilter {
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
	function new(distance : Float = 4, angle : Float = 45, highlightColor : UInt = 0xFFFFFF, highlightAlpha : Float = 1, shadowColor : UInt = 0, shadowAlpha : Float = 1, blurX : Float = 4, blurY : Float = 4, strength : Float = 1, quality : Int = 1, ?type : BitmapFilterType, knockout : Bool = false) : Void;
}
