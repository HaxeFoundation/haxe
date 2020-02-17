package flash.events;

extern class StatusEvent extends Event {
	@:flash.property var code(get,set) : String;
	@:flash.property var level(get,set) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?code : String, ?level : String) : Void;
	private function get_code() : String;
	private function get_level() : String;
	private function set_code(value : String) : String;
	private function set_level(value : String) : String;
	static final STATUS : String;
}
