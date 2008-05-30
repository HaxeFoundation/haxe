package flash.events;

extern class AsyncErrorEvent extends ErrorEvent {
	var error : flash.Error;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String, ?error : flash.Error) : Void;
	static var ASYNC_ERROR : String;
}
