package flash.automation;

@:require(flash10_1) extern class StageCaptureEvent extends flash.events.Event {
	var checksum(default,null) : UInt;
	var url(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?url : String, checksum : UInt = 0) : Void;
	static var CAPTURE : String;
}
