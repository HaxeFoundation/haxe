package flash.display3D.textures;

@:final extern class Texture extends TextureBase {
	function uploadCompressedTextureFromByteArray(data : flash.utils.ByteArray, byteArrayOffset : UInt, async : Bool = false) : Void;
	function uploadFromBitmapData(source : flash.display.BitmapData, miplevel : UInt = 0) : Void;
	function uploadFromByteArray(data : flash.utils.ByteArray, byteArrayOffset : UInt, miplevel : UInt = 0) : Void;
}
