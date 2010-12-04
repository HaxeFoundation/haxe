package flash.events;

extern class IMEEvent extends TextEvent {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String) : Void;
	static var IME_COMPOSITION : String;
}
