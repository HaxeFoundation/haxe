package flash.display;

extern class Scene {
	function new(name : String, labels : Array<Dynamic>, numFrames : Int) : Void;
	var labels(default,null) : Array<Dynamic>;
	var name(default,null) : String;
	var numFrames(default,null) : Int;
	private var _labels : Array<Dynamic>;
	private var _name : String;
	private var _numFrames : Int;
}
