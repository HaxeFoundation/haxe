package flash.events;

extern class AVPauseAtPeriodEndEvent extends Event {
	var userData(default,null) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, userData : Int = 0) : Void;
	static var AV_PAUSE_AT_PERIOD_END : String;
}
