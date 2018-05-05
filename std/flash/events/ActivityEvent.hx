package flash.events;

extern class ActivityEvent extends Event {
	var activating : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, activating : Bool = false) : Void;
	static var ACTIVITY(default,never) : String;
}
