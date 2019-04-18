package flash.events;

extern class AsyncErrorEvent extends ErrorEvent {
	var error : flash.errors.Error;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?text : String, ?error : flash.errors.Error) : Void;
	static final ASYNC_ERROR : String;
}
