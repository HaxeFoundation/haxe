package flash.events;

extern class TimerEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool) : Void;
	function updateAfterEvent() : Void;
	static var TIMER : String;
	static var TIMER_COMPLETE : String;
}
