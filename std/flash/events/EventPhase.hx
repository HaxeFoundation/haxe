package flash.events;

@:native("flash.events.EventPhase") extern enum abstract EventPhase(UInt) {
	var AT_TARGET;
	var BUBBLING_PHASE;
	var CAPTURING_PHASE;
}
