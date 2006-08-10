package flash.media;

extern class Camera extends flash.events.EventDispatcher {
	function new() : Void;
	var activityLevel(default,null) : Float;
	var bandwidth(default,null) : Int;
	var currentFPS(default,null) : Float;
	var fps(default,null) : Float;
	var height(default,null) : Int;
	var index(default,null) : Int;
	var keyFrameInterval(default,null) : Int;
	var loopback(default,null) : Bool;
	var motionLevel(default,null) : Int;
	var motionTimeout(default,null) : Int;
	var muted(default,null) : Bool;
	var name(default,null) : String;
	var quality(default,null) : Int;
	function setCursor(value : Bool) : Void;
	function setKeyFrameInterval(keyFrameInterval : Int) : Void;
	function setLoopback(?compress : Bool) : Void;
	function setMode(width : Int, height : Int, fps : Float, ?favorArea : Bool) : Void;
	function setMotionLevel(motionLevel : Int, ?timeout : Int) : Void;
	function setQuality(bandwidth : Int, quality : Int) : Void;
	var width(default,null) : Int;
	static function getCamera(?name : String) : flash.media.Camera;
	static var names(default,null) : Array<Dynamic>;
}
