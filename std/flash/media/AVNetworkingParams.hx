package flash.media;

extern class AVNetworkingParams {
	var appendRandomQueryParameter : String;
	var forceNativeNetworking : Bool;
	var networkDownVerificationUrl : String;
	var readSetCookieHeader : Bool;
	var useCookieHeaderForAllRequests : Bool;
	function new(init_forceNativeNetworking : Bool = false, init_readSetCookieHeader : Bool = true, init_useCookieHeaderForAllRequests : Bool = false, ?init_networkDownVerificationUrl : String) : Void;
}
