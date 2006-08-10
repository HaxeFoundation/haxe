package flash.events;

extern class ErrorEvent extends flash.events.TextEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var ERROR : String;
}
