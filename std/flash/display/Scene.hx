package flash.display;

extern final class Scene {
	var labels(get,never) : Array<FrameLabel>;
	var name(get,never) : String;
	var numFrames(get,never) : Int;
	function new(name : String, labels : Array<FrameLabel>, numFrames : Int) : Void;
	private function get_labels() : Array<FrameLabel>;
	private function get_name() : String;
	private function get_numFrames() : Int;
}
