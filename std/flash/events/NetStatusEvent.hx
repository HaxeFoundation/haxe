package flash.events;

extern class NetStatusEvent extends Event {
	@:flash.property var info(get,set) : Dynamic;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?info : Dynamic) : Void;
	private function get_info() : Dynamic;
	private function set_info(value : Dynamic) : Dynamic;
	static final NET_STATUS : String;
}
