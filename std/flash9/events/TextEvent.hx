package flash.events;

extern class TextEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	var text : String;
	private var m_text : String;
	static var LINK : String;
	static var TEXT_INPUT : String;
}
