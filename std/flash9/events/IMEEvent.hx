package flash.events;

extern class IMEEvent extends flash.events.TextEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var IME_COMPOSITION : String;
}
