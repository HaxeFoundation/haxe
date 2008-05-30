package flash.media;

extern class SoundChannel extends flash.events.EventDispatcher {
	var leftPeak(default,null) : Float;
	var position(default,null) : Float;
	var rightPeak(default,null) : Float;
	var soundTransform : SoundTransform;
	function new() : Void;
	function stop() : Void;
}
