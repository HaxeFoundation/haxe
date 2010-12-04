package flash.events;

extern class FullScreenEvent extends ActivityEvent {
	var fullScreen(default,null) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, fullScreen : Bool = false) : Void;
	static var FULL_SCREEN : String;
}
