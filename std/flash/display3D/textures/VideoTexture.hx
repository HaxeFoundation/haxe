package flash.display3D.textures;

@:final extern class VideoTexture extends TextureBase {
	var videoHeight(default,null) : Int;
	var videoWidth(default,null) : Int;
	function new() : Void;
	function attachCamera(theCamera : flash.media.Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
}
