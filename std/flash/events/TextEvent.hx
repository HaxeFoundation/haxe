package flash.events;

extern class TextEvent extends Event {
	@:flash.property var text(get,set) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String) : Void;
	private function get_text() : String;
	private function set_text(value : String) : String;
	static final LINK : String;
	static final TEXT_INPUT : String;
}
