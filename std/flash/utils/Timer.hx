package flash.utils;

extern class Timer extends flash.events.EventDispatcher {
	@:flash.property var currentCount(get,never) : Int;
	@:flash.property var delay(get,set) : Float;
	@:flash.property var repeatCount(get,set) : Int;
	@:flash.property var running(get,never) : Bool;
	function new(delay : Float, repeatCount : Int = 0) : Void;
	private function get_currentCount() : Int;
	private function get_delay() : Float;
	private function get_repeatCount() : Int;
	private function get_running() : Bool;
	function reset() : Void;
	private function set_delay(value : Float) : Float;
	private function set_repeatCount(value : Int) : Int;
	function start() : Void;
	function stop() : Void;
}
