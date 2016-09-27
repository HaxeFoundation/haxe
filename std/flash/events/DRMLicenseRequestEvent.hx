package flash.events;

extern class DRMLicenseRequestEvent extends Event {
	var serverURL : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inServerURL : String) : Void;
	static var LICENSE_REQUEST(default,never) : String;
}
