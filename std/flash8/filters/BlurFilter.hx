package flash.filters;

extern class BlurFilter extends BitmapFilter {

	var quality : Float;
	var blurX : Float;
	var blurY : Float;

	function new( ?blurX : Float, ?blurY : Float, ?quality : Float ) : Void;
	function clone() : BlurFilter;

}
