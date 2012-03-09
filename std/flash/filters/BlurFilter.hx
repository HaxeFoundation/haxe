package flash.filters;

@:final extern class BlurFilter extends BitmapFilter {
	var blurX : Float;
	var blurY : Float;
	var quality : Int;
	function new(blurX : Float = 4, blurY : Float = 4, quality : Int = 1) : Void;
}
