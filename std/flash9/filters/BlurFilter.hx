package flash.filters;

extern class BlurFilter extends BitmapFilter {
	var blurX : Float;
	var blurY : Float;
	var quality : Int;
	function new(?blurX : Float, ?blurY : Float, ?quality : Int) : Void;
}
