package flash.events;

@:require(flash10_1) extern class DRMAuthenticationErrorEvent extends ErrorEvent {
	var domain(get,set) : String;
	var serverURL(get,set) : String;
	var subErrorID(get,set) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inDetail : String, inErrorID : Int = 0, inSubErrorID : Int = 0, ?inServerURL : String, ?inDomain : String) : Void;
	private function get_domain() : String;
	private function get_serverURL() : String;
	private function get_subErrorID() : Int;
	private function set_domain(value : String) : String;
	private function set_serverURL(value : String) : String;
	private function set_subErrorID(value : Int) : Int;
	static final AUTHENTICATION_ERROR : String;
}
