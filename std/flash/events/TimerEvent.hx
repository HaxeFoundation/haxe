package flash.events;

extern class TimerEvent extends Event {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false) : Void;
	function updateAfterEvent() : Void;
	static final TIMER : String;
	static final TIMER_COMPLETE : String;
}
