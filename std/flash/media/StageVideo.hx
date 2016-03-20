package flash.media;

@:require(flash10_2) extern class StageVideo extends flash.events.EventDispatcher {
	var colorSpaces(default,never) : flash.Vector<String>;
	var depth : Int;
	var pan : flash.geom.Point;
	var videoHeight(default,never) : Int;
	var videoWidth(default,never) : Int;
	var viewPort : flash.geom.Rectangle;
	var zoom : flash.geom.Point;
	function new() : Void;
	@:require(flash11_7) function attachAVStream(avStream : AVStream) : Void;
	@:require(flash11_4) function attachCamera(theCamera : Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
}
