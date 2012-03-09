package flash.events;

@:require(flash10_1) extern class DRMAuthenticateEvent extends Event {
	var authenticationType(default,null) : String;
	var header(default,null) : String;
	var netstream(default,null) : flash.net.NetStream;
	var passwordPrompt(default,null) : String;
	var urlPrompt(default,null) : String;
	var usernamePrompt(default,null) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?header : String, ?userPrompt : String, ?passPrompt : String, ?urlPrompt : String, ?authenticationType : String, ?netstream : flash.net.NetStream) : Void;
	static var AUTHENTICATION_TYPE_DRM : String;
	static var AUTHENTICATION_TYPE_PROXY : String;
	static var DRM_AUTHENTICATE : String;
}
