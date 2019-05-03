package flash.media;

extern final class SoundChannel extends flash.events.EventDispatcher {
	var leftPeak(get,never) : Float;
	var position(get,never) : Float;
	var rightPeak(get,never) : Float;
	var soundTransform(get,set) : SoundTransform;
	function new() : Void;
	private function get_leftPeak() : Float;
	private function get_position() : Float;
	private function get_rightPeak() : Float;
	private function get_soundTransform() : SoundTransform;
	private function set_soundTransform(value : SoundTransform) : SoundTransform;
	function stop() : Void;
}
