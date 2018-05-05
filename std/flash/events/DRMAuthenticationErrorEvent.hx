package flash.events;

@:require(flash10_1) extern class DRMAuthenticationErrorEvent extends ErrorEvent {
	var domain : String;
	var serverURL : String;
	var subErrorID : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?inDetail : String, inErrorID : Int = 0, inSubErrorID : Int = 0, ?inServerURL : String, ?inDomain : String) : Void;
	static var AUTHENTICATION_ERROR(default,never) : String;
}
