package flash.events;

extern class SecurityErrorEvent extends ErrorEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String) : Void;
	static var SECURITY_ERROR : String;
}
