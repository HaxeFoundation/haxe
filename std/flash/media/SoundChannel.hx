package flash.media;

@:final extern class SoundChannel extends flash.events.EventDispatcher {
	var leftPeak(default,never) : Float;
	var position(default,never) : Float;
	var rightPeak(default,never) : Float;
	var soundTransform : SoundTransform;
	function new() : Void;
	function stop() : Void;
}
