package flash.events;

extern class FullScreenEvent extends ActivityEvent {
	var fullScreen(default,never) : Bool;
	@:require(flash11_3) var interactive(default,never) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, fullScreen : Bool = false, interactive : Bool = false) : Void;
	static final FULL_SCREEN : String;
	@:require(flash11_3) static final FULL_SCREEN_INTERACTIVE_ACCEPTED : String;
}
