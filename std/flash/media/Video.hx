package flash.media;

extern class Video extends flash.display.DisplayObject {
	@:flash.property var deblocking(get,set) : Int;
	@:flash.property var smoothing(get,set) : Bool;
	@:flash.property var videoHeight(get,never) : Int;
	@:flash.property var videoWidth(get,never) : Int;
	function new(width : Int = 320, height : Int = 240) : Void;
	function attachCamera(camera : Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
	function clear() : Void;
	private function get_deblocking() : Int;
	private function get_smoothing() : Bool;
	private function get_videoHeight() : Int;
	private function get_videoWidth() : Int;
	private function set_deblocking(value : Int) : Int;
	private function set_smoothing(value : Bool) : Bool;
}
