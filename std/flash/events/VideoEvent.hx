package flash.events;

@:require(flash10_2) extern class VideoEvent extends Event {
	final codecInfo : String;
	@:flash.property var status(get,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String) : Void;
	private function get_status() : String;
	static final RENDER_STATE : String;
	static final RENDER_STATUS_ACCELERATED : String;
	static final RENDER_STATUS_SOFTWARE : String;
	static final RENDER_STATUS_UNAVAILABLE : String;
}
