package flash.events;

@:require(flash10_1) extern class DRMAuthenticationCompleteEvent extends Event {
	var domain : String;
	var serverURL : String;
	var token : flash.utils.ByteArray;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inServerURL : String, ?inDomain : String, ?inToken : flash.utils.ByteArray) : Void;
	static var AUTHENTICATION_COMPLETE(default,never) : String;
}
