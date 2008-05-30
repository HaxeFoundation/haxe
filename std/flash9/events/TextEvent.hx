package flash.events;

extern class TextEvent extends Event {
	var text : String;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var LINK : String;
	static var TEXT_INPUT : String;
}
