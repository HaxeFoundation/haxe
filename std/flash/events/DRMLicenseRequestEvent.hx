package flash.events;

extern class DRMLicenseRequestEvent extends Event {
	var serverURL(get,set) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inServerURL : String) : Void;
	private function get_serverURL() : String;
	private function set_serverURL(value : String) : String;
	static final LICENSE_REQUEST : String;
}
