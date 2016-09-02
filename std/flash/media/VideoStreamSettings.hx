package flash.media;

extern class VideoStreamSettings {
	var bandwidth(default,never) : Int;
	var codec(default,never) : String;
	var fps(default,never) : Float;
	var height(default,never) : Int;
	var keyFrameInterval(default,never) : Int;
	var quality(default,never) : Int;
	var width(default,never) : Int;
	function new() : Void;
	function setKeyFrameInterval(keyFrameInterval : Int) : Void;
	function setMode(width : Int, height : Int, fps : Float) : Void;
	function setQuality(bandwidth : Int, quality : Int) : Void;
}
