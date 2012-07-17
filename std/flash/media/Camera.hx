package flash.media;

@:final extern class Camera extends flash.events.EventDispatcher {
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
	@:require(flash11_2) var position(default,null) : String;
	var quality(default,null) : Int;
	var width(default,null) : Int;
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
	@:require(flash10_1) static var isSupported(default,null) : Bool;
	static var names(default,null) : Array<Dynamic>;
	@:require(flash10_1) static function _scanHardware() : Void;
	static function getCamera(?name : String) : Camera;
}
