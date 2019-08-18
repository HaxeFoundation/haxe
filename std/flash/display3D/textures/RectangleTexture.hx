package flash.display3D.textures;

extern final class RectangleTexture extends TextureBase {
	function new() : Void;
	function uploadFromBitmapData(source : flash.display.BitmapData) : Void;
	function uploadFromByteArray(data : flash.utils.ByteArray, byteArrayOffset : UInt) : Void;
}
