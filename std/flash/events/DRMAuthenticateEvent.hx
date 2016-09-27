package flash.events;

@:require(flash10_1) extern class DRMAuthenticateEvent extends Event {
	var authenticationType(default,never) : String;
	var header(default,never) : String;
	var netstream(default,never) : flash.net.NetStream;
	var passwordPrompt(default,never) : String;
	var urlPrompt(default,never) : String;
	var usernamePrompt(default,never) : String;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?header : String, ?userPrompt : String, ?passPrompt : String, ?urlPrompt : String, ?authenticationType : String, ?netstream : flash.net.NetStream) : Void;
	static var AUTHENTICATION_TYPE_DRM(default,never) : String;
	static var AUTHENTICATION_TYPE_PROXY(default,never) : String;
	static var DRM_AUTHENTICATE(default,never) : String;
}
