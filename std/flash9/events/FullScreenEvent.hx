package flash.events;

extern class FullScreenEvent extends ActivityEvent {
	var fullScreen(default,null) : Bool;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?fullScreen : Bool) : Void;
	static var FULL_SCREEN : String;
}
