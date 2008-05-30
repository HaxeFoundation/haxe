package flash.events;

extern class ActivityEvent extends Event {
	var activating : Bool;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?activating : Bool) : Void;
	static var ACTIVITY : String;
}
