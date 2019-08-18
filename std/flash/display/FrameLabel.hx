package flash.display;

extern final class FrameLabel extends flash.events.EventDispatcher {
	@:flash.property var frame(get,never) : Int;
	@:flash.property var name(get,never) : String;
	function new(name : String, frame : Int) : Void;
	private function get_frame() : Int;
	private function get_name() : String;
}
