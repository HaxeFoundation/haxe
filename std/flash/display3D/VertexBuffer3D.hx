package flash.display3D;

extern class VertexBuffer3D {
	function dispose() : Void;
	function uploadFromByteArray(data : flash.utils.ByteArray, byteArrayOffset : Int, startVertex : Int, numVertices : Int) : Void;
	function uploadFromVector(data : flash.Vector<Float>, startVertex : Int, numVertices : Int) : Void;
}
