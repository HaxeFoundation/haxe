package flash.events;

extern class StageVideoAvailabilityEvent extends Event {
	var availability(default,never) : String;
	var driver(default,never) : String;
	var reason(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?availability : String) : Void;
	static var STAGE_VIDEO_AVAILABILITY(default,never) : String;
}
