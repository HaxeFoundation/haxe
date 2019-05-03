package flash.media;

extern class VideoStreamSettings {
	var bandwidth(get,never) : Int;
	var codec(get,never) : String;
	var fps(get,never) : Float;
	var height(get,never) : Int;
	var keyFrameInterval(get,never) : Int;
	var quality(get,never) : Int;
	var width(get,never) : Int;
	function new() : Void;
	private function get_bandwidth() : Int;
	private function get_codec() : String;
	private function get_fps() : Float;
	private function get_height() : Int;
	private function get_keyFrameInterval() : Int;
	private function get_quality() : Int;
	private function get_width() : Int;
	function setKeyFrameInterval(keyFrameInterval : Int) : Void;
	function setMode(width : Int, height : Int, fps : Float) : Void;
	function setQuality(bandwidth : Int, quality : Int) : Void;
}
