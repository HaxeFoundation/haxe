package flash.display;

extern class MovieClip extends Sprite #if !flash_strict implements Dynamic #end {
	var currentFrame(default,never) : Int;
	@:require(flash10) var currentFrameLabel(default,never) : String;
	var currentLabel(default,never) : String;
	var currentLabels(default,never) : Array<FrameLabel>;
	var currentScene(default,never) : Scene;
	var enabled : Bool;
	var framesLoaded(default,never) : Int;
	@:require(flash11) var isPlaying(default,never) : Bool;
	var scenes(default,never) : Array<Scene>;
	var totalFrames(default,never) : Int;
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
