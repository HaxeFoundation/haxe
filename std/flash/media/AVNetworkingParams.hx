package flash.media;

extern class AVNetworkingParams {
	var forceNativeNetworking : Bool;
	var readSetCookieHeader : Bool;
	var useCookieHeaderForAllRequests : Bool;
	function new(init_forceNativeNetworking : Bool = false, init_readSetCookieHeader : Bool = true, init_useCookieHeaderForAllRequests : Bool = false) : Void;
}
