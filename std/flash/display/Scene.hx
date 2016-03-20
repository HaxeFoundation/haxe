package flash.display;

@:final extern class Scene {
	var labels(default,never) : Array<FrameLabel>;
	var name(default,never) : String;
	var numFrames(default,never) : Int;
	function new(name : String, labels : Array<FrameLabel>, numFrames : Int) : Void;
}
