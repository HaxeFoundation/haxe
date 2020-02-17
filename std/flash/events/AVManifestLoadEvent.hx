package flash.events;

extern class AVManifestLoadEvent extends Event {
	@:flash.property var duration(get,never) : Float;
	@:flash.property var handle(get,never) : Int;
	@:flash.property var result(get,never) : flash.media.AVResult;
	@:flash.property var userData(get,never) : Int;
	function new(?type : String, bubbles : Bool = false, cancelable : Bool = false, inResult : Int = 0, inUserData : Int = 0, inHandle : Int = 0, inDuration : Float = 0) : Void;
	private function get_duration() : Float;
	private function get_handle() : Int;
	private function get_result() : flash.media.AVResult;
	private function get_userData() : Int;
	static final AV_MANIFEST_LOAD : String;
}
