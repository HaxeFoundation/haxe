package flash.events;

extern class AsyncErrorEvent extends ErrorEvent {
	var error : flash.errors.Error;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, ?error : flash.errors.Error) : Void;
	static var ASYNC_ERROR(default,never) : String;
}
