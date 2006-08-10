package flash.events;

extern class AsyncErrorEvent extends flash.events.ErrorEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String, ?error : Error) : Void;
	var error : Error;
	static var ASYNC_ERROR : String;
}
