package flash.events;

extern class StageVideoAvailabilityEvent extends Event {
	@:flash.property var availability(get,never) : String;
	final driver : String;
	final reason : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?availability : String) : Void;
	private function get_availability() : String;
	static final STAGE_VIDEO_AVAILABILITY : String;
}
