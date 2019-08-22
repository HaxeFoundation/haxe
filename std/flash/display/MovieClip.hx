package flash.display;

extern class MovieClip extends Sprite #if !flash_strict implements Dynamic #end {
	@:flash.property var currentFrame(get,never) : Int;
	@:flash.property @:require(flash10) var currentFrameLabel(get,never) : String;
	@:flash.property var currentLabel(get,never) : String;
	@:flash.property var currentLabels(get,never) : Array<FrameLabel>;
	@:flash.property var currentScene(get,never) : Scene;
	@:flash.property var enabled(get,set) : Bool;
	@:flash.property var framesLoaded(get,never) : Int;
	@:flash.property @:require(flash11) var isPlaying(get,never) : Bool;
	@:flash.property var scenes(get,never) : Array<Scene>;
	@:flash.property var totalFrames(get,never) : Int;
	@:flash.property var trackAsMenu(get,set) : Bool;
	function new() : Void;
	function addFrameScript(restArgs : haxe.extern.Rest<Dynamic>) : Void;
	private function get_currentFrame() : Int;
	private function get_currentFrameLabel() : String;
	private function get_currentLabel() : String;
	private function get_currentLabels() : Array<FrameLabel>;
	private function get_currentScene() : Scene;
	private function get_enabled() : Bool;
	private function get_framesLoaded() : Int;
	private function get_isPlaying() : Bool;
	private function get_scenes() : Array<Scene>;
	private function get_totalFrames() : Int;
	private function get_trackAsMenu() : Bool;
	function gotoAndPlay(frame : flash.utils.Object, ?scene : String) : Void;
	function gotoAndStop(frame : flash.utils.Object, ?scene : String) : Void;
	function nextFrame() : Void;
	function nextScene() : Void;
	function play() : Void;
	function prevFrame() : Void;
	function prevScene() : Void;
	private function set_enabled(value : Bool) : Bool;
	private function set_trackAsMenu(value : Bool) : Bool;
	function stop() : Void;
}
