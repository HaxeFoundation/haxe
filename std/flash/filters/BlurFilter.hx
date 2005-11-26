package flash.filters;

extern class BlurFilter extends BitmapFilter {

	var quality : Float;
	var blurX : Float;
	var blurY : Float;

	function new( bx : Float, by : Float, qual : Float ) : Void;
	function clone() : BlurFilter;

}
