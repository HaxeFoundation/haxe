package flash.events;

@:require(flash10_1) extern class AccelerometerEvent extends Event {
	var accelerationX : Float;
	var accelerationY : Float;
	var accelerationZ : Float;
	var timestamp : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, timestamp : Float = 0, accelerationX : Float = 0, accelerationY : Float = 0, accelerationZ : Float = 0) : Void;
	static var UPDATE(default,never) : String;
}
