package flash.automation;

@:require(flash10_1) extern class StageCaptureEvent extends flash.events.Event {
	var checksum(default,never) : UInt;
	var pts(default,never) : Float;
	var url(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?url : String, checksum : UInt = 0, pts : Float = 0) : Void;
	static var CAPTURE(default,never) : String;
}
