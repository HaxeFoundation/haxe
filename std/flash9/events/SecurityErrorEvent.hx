package flash.events;

extern class SecurityErrorEvent extends flash.events.ErrorEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var SECURITY_ERROR : String;
}
