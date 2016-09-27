package flash.events;

extern class AVHTTPStatusEvent extends Event {
	var responseHeaders : Array<Dynamic>;
	var responseURL : String;
	var status(default,never) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, status : Int = 0, ?responseUrl : String, ?responseHeaders : Array<Dynamic>) : Void;
	static var AV_HTTP_RESPONSE_STATUS(default,never) : String;
}
