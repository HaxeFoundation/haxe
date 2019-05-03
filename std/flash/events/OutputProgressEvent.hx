package flash.events;

extern class OutputProgressEvent extends Event {
	var bytesPending(get,set) : Float;
	var bytesTotal(get,set) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, bytesPending : Float = 0, bytesTotal : Float = 0) : Void;
	private function get_bytesPending() : Float;
	private function get_bytesTotal() : Float;
	private function set_bytesPending(value : Float) : Float;
	private function set_bytesTotal(value : Float) : Float;
	static final OUTPUT_PROGRESS : String;
}
