package flash.events;

extern class FullScreenEvent extends ActivityEvent {
	var fullScreen(default,null) : Bool;
	@:require(flash11_3) var interactive(default,null) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, fullScreen : Bool = false, interactive : Bool = false) : Void;
	static var FULL_SCREEN : String;
	@:require(flash11_3) static var FULL_SCREEN_INTERACTIVE_ACCEPTED : String;
}
