package flash.events;

extern class ErrorEvent extends TextEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var ERROR : String;
}
