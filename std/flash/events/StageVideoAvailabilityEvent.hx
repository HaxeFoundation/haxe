package flash.events;

extern class StageVideoAvailabilityEvent extends Event {
	var availability(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?availability : String) : Void;
	static var STAGE_VIDEO_AVAILABILITY : String;
}
