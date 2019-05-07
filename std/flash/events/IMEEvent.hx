package flash.events;

extern class IMEEvent extends TextEvent {
	@:flash.property @:require(flash10_1) var imeClient(get,set) : flash.text.ime.IIMEClient;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, ?imeClient : flash.text.ime.IIMEClient) : Void;
	private function get_imeClient() : flash.text.ime.IIMEClient;
	private function set_imeClient(value : flash.text.ime.IIMEClient) : flash.text.ime.IIMEClient;
	static final IME_COMPOSITION : String;
	@:require(flash10_1) static final IME_START_COMPOSITION : String;
}
