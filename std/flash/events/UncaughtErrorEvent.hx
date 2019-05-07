package flash.events;

@:require(flash10_1) extern class UncaughtErrorEvent extends ErrorEvent {
	@:flash.property var error(get,never) : Dynamic;
	function new(?type : String, bubbles : Bool = true, cancelable : Bool = true, ?error_in : Dynamic) : Void;
	private function get_error() : Dynamic;
	static final UNCAUGHT_ERROR : String;
}
