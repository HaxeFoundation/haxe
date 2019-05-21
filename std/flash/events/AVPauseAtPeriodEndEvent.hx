package flash.events;

extern class AVPauseAtPeriodEndEvent extends Event {
	@:flash.property var userData(get,never) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, userData : Int = 0) : Void;
	private function get_userData() : Int;
	static final AV_PAUSE_AT_PERIOD_END : String;
}
