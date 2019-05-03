package flash.media;

@:require(flash10_2) extern class StageVideo extends flash.events.EventDispatcher {
	var colorSpaces(get,never) : flash.Vector<String>;
	var depth(get,set) : Int;
	var pan(get,set) : flash.geom.Point;
	var videoHeight(get,never) : Int;
	var videoWidth(get,never) : Int;
	var viewPort(get,set) : flash.geom.Rectangle;
	var zoom(get,set) : flash.geom.Point;
	function new() : Void;
	@:require(flash11_7) function attachAVStream(avStream : AVStream) : Void;
	@:require(flash11_4) function attachCamera(theCamera : Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
	private function get_colorSpaces() : flash.Vector<String>;
	private function get_depth() : Int;
	private function get_pan() : flash.geom.Point;
	private function get_videoHeight() : Int;
	private function get_videoWidth() : Int;
	private function get_viewPort() : flash.geom.Rectangle;
	private function get_zoom() : flash.geom.Point;
	private function set_depth(value : Int) : Int;
	private function set_pan(value : flash.geom.Point) : flash.geom.Point;
	private function set_viewPort(value : flash.geom.Rectangle) : flash.geom.Rectangle;
	private function set_zoom(value : flash.geom.Point) : flash.geom.Point;
}
