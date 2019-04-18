package flash.events;

extern class StageVideoAvailabilityEvent extends Event {
	var availability(default,never) : String;
	final driver : String;
	final reason : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?availability : String) : Void;
	static final STAGE_VIDEO_AVAILABILITY : String;
}
