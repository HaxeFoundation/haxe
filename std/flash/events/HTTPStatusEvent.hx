package flash.events;

extern class HTTPStatusEvent extends Event {
	@:require(flash10_1) var responseHeaders : Array<Dynamic>;
	@:require(flash10_1) var responseURL : String;
	var status(default,null) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, status : Int = 0) : Void;
	@:require(flash10_1) static var HTTP_RESPONSE_STATUS : String;
	static var HTTP_STATUS : String;
}
