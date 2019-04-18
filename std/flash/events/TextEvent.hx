package flash.events;

extern class TextEvent extends Event {
	var text : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String) : Void;
	static final LINK : String;
	static final TEXT_INPUT : String;
}
