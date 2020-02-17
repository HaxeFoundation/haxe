package flash.media;

extern class VideoStreamSettings {
	@:flash.property var bandwidth(get,never) : Int;
	@:flash.property var codec(get,never) : String;
	@:flash.property var fps(get,never) : Float;
	@:flash.property var height(get,never) : Int;
	@:flash.property var keyFrameInterval(get,never) : Int;
	@:flash.property var quality(get,never) : Int;
	@:flash.property var width(get,never) : Int;
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
