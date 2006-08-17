package flash.display;

extern class Scene {
	function new(name : String, labels : Array<FrameLabel>, numFrames : Int) : Void;
	var labels(default,null) : Array<FrameLabel>;
	var name(default,null) : String;
	var numFrames(default,null) : Int;
	private var _labels : Array<FrameLabel>;
	private var _name : String;
	private var _numFrames : Int;
}
