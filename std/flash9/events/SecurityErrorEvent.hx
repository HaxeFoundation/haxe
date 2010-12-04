package flash.events;

extern class SecurityErrorEvent extends ErrorEvent {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String) : Void;
	static var SECURITY_ERROR : String;
}
