package flash.events;

extern class VideoTextureEvent extends Event {
	final codecInfo : String;
	@:flash.property var colorSpace(get,never) : String;
	@:flash.property var status(get,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?status : String, ?colorSpace : String) : Void;
	private function get_colorSpace() : String;
	private function get_status() : String;
	static final RENDER_STATE : String;
}
