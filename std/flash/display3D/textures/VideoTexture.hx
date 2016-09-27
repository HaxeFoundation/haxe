package flash.display3D.textures;

@:final extern class VideoTexture extends TextureBase {
	var videoHeight(default,never) : Int;
	var videoWidth(default,never) : Int;
	function new() : Void;
	function attachCamera(theCamera : flash.media.Camera) : Void;
	function attachNetStream(netStream : flash.net.NetStream) : Void;
}
