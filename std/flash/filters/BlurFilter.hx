package flash.filters;

extern final class BlurFilter extends BitmapFilter {
	var blurX(get,set) : Float;
	var blurY(get,set) : Float;
	var quality(get,set) : Int;
	function new(blurX : Float = 4, blurY : Float = 4, quality : Int = 1) : Void;
	private function get_blurX() : Float;
	private function get_blurY() : Float;
	private function get_quality() : Int;
	private function set_blurX(value : Float) : Float;
	private function set_blurY(value : Float) : Float;
	private function set_quality(value : Int) : Int;
}
