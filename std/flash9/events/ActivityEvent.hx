package flash.events;

extern class ActivityEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?activating : Bool) : Void;
	var activating : Bool;
	private var m_activating : Bool;
	static var ACTIVITY : String;
}
