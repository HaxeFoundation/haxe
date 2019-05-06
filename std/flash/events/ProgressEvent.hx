package flash.events;

extern class ProgressEvent extends Event {
	@:flash.property var bytesLoaded(get,set) : Float;
	@:flash.property var bytesTotal(get,set) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, bytesLoaded : Float = 0, bytesTotal : Float = 0) : Void;
	private function get_bytesLoaded() : Float;
	private function get_bytesTotal() : Float;
	private function set_bytesLoaded(value : Float) : Float;
	private function set_bytesTotal(value : Float) : Float;
	static final PROGRESS : String;
	static final SOCKET_DATA : String;
}
