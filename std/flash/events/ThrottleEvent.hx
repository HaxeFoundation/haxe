package flash.events;

@:require(flash11_2) extern class ThrottleEvent extends Event {
	@:flash.property var state(get,never) : String;
	@:flash.property var targetFrameRate(get,never) : Float;
	function new(type : ThrottleType, bubbles : Bool = false, cancelable : Bool = false, ?state : String, targetFrameRate : Float = 0) : Void;
	private function get_state() : String;
	private function get_targetFrameRate() : Float;
	static final THROTTLE : String;
}
