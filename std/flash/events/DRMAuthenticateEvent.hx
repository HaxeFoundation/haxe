package flash.events;

@:require(flash10_1) extern class DRMAuthenticateEvent extends Event {
	var authenticationType(get,never) : String;
	var header(get,never) : String;
	var netstream(get,never) : flash.net.NetStream;
	var passwordPrompt(get,never) : String;
	var urlPrompt(get,never) : String;
	var usernamePrompt(get,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?header : String, ?userPrompt : String, ?passPrompt : String, ?urlPrompt : String, ?authenticationType : String, ?netstream : flash.net.NetStream) : Void;
	private function get_authenticationType() : String;
	private function get_header() : String;
	private function get_netstream() : flash.net.NetStream;
	private function get_passwordPrompt() : String;
	private function get_urlPrompt() : String;
	private function get_usernamePrompt() : String;
	static final AUTHENTICATION_TYPE_DRM : String;
	static final AUTHENTICATION_TYPE_PROXY : String;
	static final DRM_AUTHENTICATE : String;
}
