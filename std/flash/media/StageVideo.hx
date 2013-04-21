package flash.media;

@:require(flash10_2) extern class StageVideo extends flash.events.EventDispatcher {
	var colorSpaces(default,null) : flash.Vector<String>;
	var depth : Int;
	var pan : flash.geom.Point;
	var videoHeight(default,null) : Int;
	var videoWidth(default,null) : Int;
	var viewPort : flash.geom.Rectangle;
	var zoom : flash.geom.Point;
	function new() : Void;
	@:require(flash11_7) function attachAVStream(avStream : AVStream) : Void;
	@:require(flash11_4) function attachCamera(theCamera : Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
}
