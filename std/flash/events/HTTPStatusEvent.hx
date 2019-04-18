package flash.events;

extern class HTTPStatusEvent extends Event {
	var redirected : Bool;
	@:require(flash10_1) var responseHeaders : Array<Dynamic>;
	@:require(flash10_1) var responseURL : String;
	var status(default,never) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, status : Int = 0, redirected : Bool = false) : Void;
	@:require(flash10_1) static final HTTP_RESPONSE_STATUS : String;
	static final HTTP_STATUS : String;
}
