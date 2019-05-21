package flash.events;

extern class FullScreenEvent extends ActivityEvent {
	@:flash.property var fullScreen(get,never) : Bool;
	@:flash.property @:require(flash11_3) var interactive(get,never) : Bool;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, fullScreen : Bool = false, interactive : Bool = false) : Void;
	private function get_fullScreen() : Bool;
	private function get_interactive() : Bool;
	static final FULL_SCREEN : String;
	@:require(flash11_3) static final FULL_SCREEN_INTERACTIVE_ACCEPTED : String;
}
