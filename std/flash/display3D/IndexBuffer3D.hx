package flash.display3D;

extern final class IndexBuffer3D {
	function dispose() : Void;
	function uploadFromByteArray(data : flash.utils.ByteArray, byteArrayOffset : Int, startOffset : Int, count : Int) : Void;
	function uploadFromVector(data : flash.Vector<UInt>, startOffset : Int, count : Int) : Void;
}
