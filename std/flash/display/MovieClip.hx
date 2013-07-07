package flash.display;

extern class MovieClip extends Sprite #if !flash_strict implements Dynamic #end {
	var currentFrame(default,null) : Int;
	@:require(flash10) var currentFrameLabel(default,null) : String;
	var currentLabel(default,null) : String;
	var currentLabels(default,null) : Array<FrameLabel>;
	var currentScene(default,null) : Scene;
	var enabled : Bool;
	var framesLoaded(default,null) : Int;
	@:require(flash11) var isPlaying(default,null) : Bool;
	var scenes(default,null) : Array<Scene>;
	var totalFrames(default,null) : Int;
	var trackAsMenu : Bool;
	function new() : Void;
	function addFrameScript(?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic) : Void;
	function gotoAndPlay(frame : flash.utils.Object, ?scene : String) : Void;
	function gotoAndStop(frame : flash.utils.Object, ?scene : String) : Void;
	function nextFrame() : Void;
	function nextScene() : Void;
	function play() : Void;
	function prevFrame() : Void;
	function prevScene() : Void;
	function stop() : Void;
}
