package flash.events;

extern class AsyncErrorEvent extends flash.events.ErrorEvent {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?text : String, ?error : Dynamic) : Void;
	var error : Dynamic;
	static var ASYNC_ERROR : String;
}
