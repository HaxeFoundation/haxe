package flash.display;

extern class Scene {
	var labels(default,null) : Array<FrameLabel>;
	var name(default,null) : String;
	var numFrames(default,null) : Int;
	function new(name : String, labels : Array<FrameLabel>, numFrames : Int) : Void;
}
