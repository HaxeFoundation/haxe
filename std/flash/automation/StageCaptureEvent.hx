package flash.automation;

@:require(flash10_1) extern class StageCaptureEvent extends flash.events.Event {
	@:flash.property var checksum(get,never) : UInt;
	@:flash.property var pts(get,never) : Float;
	@:flash.property var url(get,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?url : String, checksum : UInt = 0, pts : Float = 0) : Void;
	private function get_checksum() : UInt;
	private function get_pts() : Float;
	private function get_url() : String;
	static final CAPTURE : String;
}
