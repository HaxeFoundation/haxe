package flash.events;

extern class IMEEvent extends TextEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var IME_COMPOSITION : String;
}
