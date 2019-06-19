package flash.media;

extern class AVNetworkingParams {
	@:flash.property var appendRandomQueryParameter(get,set) : String;
	@:flash.property var forceNativeNetworking(get,set) : Bool;
	@:flash.property var networkDownVerificationUrl(get,set) : String;
	@:flash.property var readSetCookieHeader(get,set) : Bool;
	@:flash.property var useCookieHeaderForAllRequests(get,set) : Bool;
	function new(init_forceNativeNetworking : Bool = false, init_readSetCookieHeader : Bool = true, init_useCookieHeaderForAllRequests : Bool = false, ?init_networkDownVerificationUrl : String) : Void;
	private function get_appendRandomQueryParameter() : String;
	private function get_forceNativeNetworking() : Bool;
	private function get_networkDownVerificationUrl() : String;
	private function get_readSetCookieHeader() : Bool;
	private function get_useCookieHeaderForAllRequests() : Bool;
	private function set_appendRandomQueryParameter(value : String) : String;
	private function set_forceNativeNetworking(value : Bool) : Bool;
	private function set_networkDownVerificationUrl(value : String) : String;
	private function set_readSetCookieHeader(value : Bool) : Bool;
	private function set_useCookieHeaderForAllRequests(value : Bool) : Bool;
}
