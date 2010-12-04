package flash.events;

extern class ErrorEvent extends TextEvent {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String) : Void;
	static var ERROR : String;
}
