package flash.display3D.textures;

extern final class VideoTexture extends TextureBase {
	var videoHeight(get,never) : Int;
	var videoWidth(get,never) : Int;
	function new() : Void;
	function attachCamera(theCamera : flash.media.Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
	private function get_videoHeight() : Int;
	private function get_videoWidth() : Int;
}
