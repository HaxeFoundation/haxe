package flash.events;

extern class FullScreenEvent extends ActivityEvent {
	var fullScreen(default,never) : Bool;
	@:require(flash11_3) var interactive(default,never) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, fullScreen : Bool = false, interactive : Bool = false) : Void;
	static var FULL_SCREEN(default,never) : String;
	@:require(flash11_3) static var FULL_SCREEN_INTERACTIVE_ACCEPTED(default,never) : String;
}
