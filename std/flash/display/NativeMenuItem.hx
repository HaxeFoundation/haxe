package flash.display;

@:require(flash10_1) extern class NativeMenuItem extends flash.events.EventDispatcher {
	@:flash.property var enabled(get,set) : Bool;
	function new() : Void;
	private function get_enabled() : Bool;
	private function set_enabled(value : Bool) : Bool;
}
