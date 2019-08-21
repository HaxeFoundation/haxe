package flash.media;

extern final class SoundTransform {
	@:flash.property var leftToLeft(get,set) : Float;
	@:flash.property var leftToRight(get,set) : Float;
	@:flash.property var pan(get,set) : Float;
	@:flash.property var rightToLeft(get,set) : Float;
	@:flash.property var rightToRight(get,set) : Float;
	@:flash.property var volume(get,set) : Float;
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
