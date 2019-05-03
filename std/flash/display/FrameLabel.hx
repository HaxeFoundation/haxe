package flash.display;

extern final class FrameLabel extends flash.events.EventDispatcher {
	var frame(get,never) : Int;
	var name(get,never) : String;
	function new(name : String, frame : Int) : Void;
	private function get_frame() : Int;
	private function get_name() : String;
}
