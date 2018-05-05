package flash.events;

extern class SecurityErrorEvent extends ErrorEvent {
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, id : Int = 0) : Void;
	static var SECURITY_ERROR(default,never) : String;
}
