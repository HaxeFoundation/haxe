package flash.events;

extern class DataEvent extends TextEvent {
	@:flash.property var data(get,set) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?data : String) : Void;
	private function get_data() : String;
	private function set_data(value : String) : String;
	static final DATA : String;
	static final UPLOAD_COMPLETE_DATA : String;
}
