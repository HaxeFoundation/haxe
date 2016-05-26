package flash.media;

@:final extern class Camera extends flash.events.EventDispatcher {
	var activityLevel(default,never) : Float;
	var bandwidth(default,never) : Int;
	var currentFPS(default,never) : Float;
	var fps(default,never) : Float;
	var height(default,never) : Int;
	var index(default,never) : Int;
	var keyFrameInterval(default,never) : Int;
	var loopback(default,never) : Bool;
	var motionLevel(default,never) : Int;
	var motionTimeout(default,never) : Int;
	var muted(default,never) : Bool;
	var name(default,never) : String;
	@:require(flash11_2) var position(default,never) : String;
	var quality(default,never) : Int;
	var width(default,never) : Int;
	function new() : Void;
	@:require(flash11_4) function copyToByteArray(rect : flash.geom.Rectangle, destination : flash.utils.ByteArray) : Void;
	@:require(flash11_4) function copyToVector(rect : flash.geom.Rectangle, destination : flash.Vector<UInt>) : Void;
	@:require(flash11_4) function drawToBitmapData(destination : flash.display.BitmapData) : Void;
	function setCursor(value : Bool) : Void;
	function setKeyFrameInterval(keyFrameInterval : Int) : Void;
	function setLoopback(compress : Bool = false) : Void;
	function setMode(width : Int, height : Int, fps : Float, favorArea : Bool = true) : Void;
	function setMotionLevel(motionLevel : Int, timeout : Int = 2000) : Void;
	function setQuality(bandwidth : Int, quality : Int) : Void;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
	static var names(default,never) : Array<Dynamic>;
	@:require(flash10_1) static function _scanHardware() : Void;
	static function getCamera(?name : String) : Camera;
}
