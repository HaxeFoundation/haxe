package flash.events;

@:native("flash.events.ThrottleType") extern enum abstract ThrottleType(String) {
	var PAUSE;
	var RESUME;
	var THROTTLE;
}
