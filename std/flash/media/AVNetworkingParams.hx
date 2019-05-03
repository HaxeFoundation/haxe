package flash.media;

extern class AVNetworkingParams {
	var appendRandomQueryParameter(get,set) : String;
	var forceNativeNetworking(get,set) : Bool;
	var networkDownVerificationUrl(get,set) : String;
	var readSetCookieHeader(get,set) : Bool;
	var useCookieHeaderForAllRequests(get,set) : Bool;
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
