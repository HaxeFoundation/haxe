package flash.media;

extern class VideoStreamSettings {
	var bandwidth(default,null) : Int;
	var codec(default,null) : String;
	var fps(default,null) : Float;
	var height(default,null) : Int;
	var keyFrameInterval(default,null) : Int;
	var quality(default,null) : Int;
	var width(default,null) : Int;
	function new() : Void;
	function setKeyFrameInterval(keyFrameInterval : Int) : Void;
	function setMode(width : Int, height : Int, fps : Float) : Void;
	function setQuality(bandwidth : Int, quality : Int) : Void;
}
