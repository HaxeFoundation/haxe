package flash.media;

extern final class SoundTransform {
	var leftToLeft(get,set) : Float;
	var leftToRight(get,set) : Float;
	var pan(get,set) : Float;
	var rightToLeft(get,set) : Float;
	var rightToRight(get,set) : Float;
	var volume(get,set) : Float;
	function new(vol : Float = 1, panning : Float = 0) : Void;
	private function get_leftToLeft() : Float;
	private function get_leftToRight() : Float;
	private function get_pan() : Float;
	private function get_rightToLeft() : Float;
	private function get_rightToRight() : Float;
	private function get_volume() : Float;
	private function set_leftToLeft(value : Float) : Float;
	private function set_leftToRight(value : Float) : Float;
	private function set_pan(value : Float) : Float;
	private function set_rightToLeft(value : Float) : Float;
	private function set_rightToRight(value : Float) : Float;
	private function set_volume(value : Float) : Float;
}
