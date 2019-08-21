package flash.media;

extern final class SoundChannel extends flash.events.EventDispatcher {
	@:flash.property var leftPeak(get,never) : Float;
	@:flash.property var position(get,never) : Float;
	@:flash.property var rightPeak(get,never) : Float;
	@:flash.property var soundTransform(get,set) : SoundTransform;
	function new() : Void;
	private function get_leftPeak() : Float;
	private function get_position() : Float;
	private function get_rightPeak() : Float;
	private function get_soundTransform() : SoundTransform;
	private function set_soundTransform(value : SoundTransform) : SoundTransform;
	function stop() : Void;
}
