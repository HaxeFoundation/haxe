package flash.utils;

extern class Timer extends flash.events.EventDispatcher {
	function new(delay : Float, ?repeatCount : Int) : Void;
	var currentCount(default,null) : Int;
	var delay : Float;
	var repeatCount : Int;
	function reset() : Void;
	var running(default,null) : Bool;
	function start() : Void;
	function stop() : Void;
	private function _start(delay : Float, closure : Function) : Void;
	private function _timerDispatch() : Void;
	private var m_delay : Float;
	private var m_iteration : Int;
	private var m_repeatCount : Int;
	private function tick() : Void;
}
