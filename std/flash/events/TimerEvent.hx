package flash.events;

extern class TimerEvent extends Event {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false) : Void;
	function updateAfterEvent() : Void;
	static var TIMER(default,never) : String;
	static var TIMER_COMPLETE(default,never) : String;
}
