package flash.events;

extern class ActivityEvent extends Event {
	@:flash.property var activating(get,set) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, activating : Bool = false) : Void;
	private function get_activating() : Bool;
	private function set_activating(value : Bool) : Bool;
	static final ACTIVITY : String;
}
