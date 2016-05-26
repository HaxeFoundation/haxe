package flash.media;

extern class Video extends flash.display.DisplayObject {
	var deblocking : Int;
	var smoothing : Bool;
	var videoHeight(default,never) : Int;
	var videoWidth(default,never) : Int;
	function new(width : Int = 320, height : Int = 240) : Void;
	function attachCamera(camera : Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
	function clear() : Void;
}
