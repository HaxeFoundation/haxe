package flash.events;

extern class HTTPStatusEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?status : Int) : Void;
	var status(default,null) : Int;
	private var m_status : Int;
	static var HTTP_STATUS : String;
}
