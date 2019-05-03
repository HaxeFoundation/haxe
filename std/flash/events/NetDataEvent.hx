package flash.events;

extern class NetDataEvent extends Event {
	var info(get,never) : Dynamic;
	var timestamp(get,never) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, timestamp : Float = 0, ?info : Dynamic) : Void;
	private function get_info() : Dynamic;
	private function get_timestamp() : Float;
	static final MEDIA_TYPE_DATA : String;
}
