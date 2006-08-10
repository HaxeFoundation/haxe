package flash.media;

extern class Video extends flash.display.DisplayObject {
	function new(?width : Int, ?height : Int) : Void;
	function attachCamera(camera : flash.media.Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
	function clear() : Void;
	var deblocking : Int;
	var smoothing : Bool;
	var videoHeight(default,null) : Int;
	var videoWidth(default,null) : Int;
}
