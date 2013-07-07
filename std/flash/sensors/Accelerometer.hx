package flash.sensors;

@:require(flash10_1) extern class Accelerometer extends flash.events.EventDispatcher {
	var muted(default,null) : Bool;
	function new() : Void;
	function setRequestedUpdateInterval(interval : Float) : Void;
	static var isSupported(default,null) : Bool;
}
