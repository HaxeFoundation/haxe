package flash.media;

extern class SoundChannel extends flash.events.EventDispatcher {
	function new() : Void;
	var leftPeak(default,null) : Float;
	var position(default,null) : Float;
	var rightPeak(default,null) : Float;
	var soundTransform : flash.media.SoundTransform;
	function stop() : Void;
}
