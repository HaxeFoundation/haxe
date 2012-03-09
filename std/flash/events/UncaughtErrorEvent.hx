package flash.events;

@:require(flash10_1) extern class UncaughtErrorEvent extends ErrorEvent {
	var error(default,null) : Dynamic;
	function new(?type : String, bubbles : Bool = true, cancelable : Bool = true, ?error_in : Dynamic) : Void;
	static var UNCAUGHT_ERROR : String;
}
