package flash.display;

extern class MovieClip extends flash.display.Sprite {
	function new() : Void;
	function addFrameScript( ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	var currentFrame(default,null) : Int;
	var currentLabel(default,null) : String;
	var currentLabels(default,null) : Array<Dynamic>;
	var currentScene(default,null) : flash.display.Scene;
	var enabled : Bool;
	var framesLoaded(default,null) : Int;
	function gotoAndPlay(frame : Dynamic, ?scene : String) : Void;
	function gotoAndStop(frame : Dynamic, ?scene : String) : Void;
	function nextFrame() : Void;
	function nextScene() : Void;
	function play() : Void;
	function prevFrame() : Void;
	function prevScene() : Void;
	var scenes(default,null) : Array<Dynamic>;
	function stop() : Void;
	var totalFrames(default,null) : Int;
	var trackAsMenu : Bool;
}
