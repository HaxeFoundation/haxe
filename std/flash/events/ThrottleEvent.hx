package flash.events;

@:require(flash11_2) extern class ThrottleEvent extends Event {
	var state(default,never) : String;
	var targetFrameRate(default,never) : Float;
	function new(type : ThrottleType, bubbles : Bool = false, cancelable : Bool = false, ?state : String, targetFrameRate : Float = 0) : Void;
	static var THROTTLE(default,never) : String;
}
