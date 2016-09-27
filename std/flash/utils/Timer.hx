package flash.utils;

extern class Timer extends flash.events.EventDispatcher {
	var currentCount(default,never) : Int;
	var delay : Float;
	var repeatCount : Int;
	var running(default,never) : Bool;
	function new(delay : Float, repeatCount : Int = 0) : Void;
	function reset() : Void;
	function start() : Void;
	function stop() : Void;
}
