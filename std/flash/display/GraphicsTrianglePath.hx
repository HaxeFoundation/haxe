package flash.display;

@:final extern class GraphicsTrianglePath implements IGraphicsData implements IGraphicsPath {
	var culling : TriangleCulling;
	var indices : flash.Vector<Int>;
	var uvtData : flash.Vector<Float>;
	var vertices : flash.Vector<Float>;
	function new(?vertices : flash.Vector<Float>, ?indices : flash.Vector<Int>, ?uvtData : flash.Vector<Float>, ?culling : TriangleCulling) : Void;
}
