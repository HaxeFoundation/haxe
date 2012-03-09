package flash.display3D;

@:final extern class Program3D {
	function dispose() : Void;
	function upload(vertexProgram : flash.utils.ByteArray, fragmentProgram : flash.utils.ByteArray) : Void;
}
