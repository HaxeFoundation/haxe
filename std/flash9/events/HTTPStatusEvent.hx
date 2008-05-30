package flash.events;

extern class HTTPStatusEvent extends Event {
	var status(default,null) : Int;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?status : Int) : Void;
	static var HTTP_STATUS : String;
}
