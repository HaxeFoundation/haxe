package flash.events;

extern class HTTPStatusEvent extends Event {
	var status(default,null) : Int;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, status : Int = 0) : Void;
	static var HTTP_STATUS : String;
}
