package flash.events;

@:require(flash10_1) extern class DRMAuthenticationCompleteEvent extends Event {
	@:flash.property var domain(get,set) : String;
	@:flash.property var serverURL(get,set) : String;
	@:flash.property var token(get,set) : flash.utils.ByteArray;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inServerURL : String, ?inDomain : String, ?inToken : flash.utils.ByteArray) : Void;
	private function get_domain() : String;
	private function get_serverURL() : String;
	private function get_token() : flash.utils.ByteArray;
	private function set_domain(value : String) : String;
	private function set_serverURL(value : String) : String;
	private function set_token(value : flash.utils.ByteArray) : flash.utils.ByteArray;
	static final AUTHENTICATION_COMPLETE : String;
}
