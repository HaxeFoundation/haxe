package flash.filters;

extern class BlurFilter extends flash.filters.BitmapFilter {
	function new(?blurX : Float, ?blurY : Float, ?quality : Int) : Void;
	var blurX : Float;
	var blurY : Float;
	var quality : Int;
}
