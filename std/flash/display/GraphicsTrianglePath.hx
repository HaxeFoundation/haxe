package flash.display;

extern final class GraphicsTrianglePath implements IGraphicsData implements IGraphicsPath {
	@:flash.property var culling(get,set) : TriangleCulling;
	var indices : flash.Vector<Int>;
	var uvtData : flash.Vector<Float>;
	var vertices : flash.Vector<Float>;
	function new(?vertices : flash.Vector<Float>, ?indices : flash.Vector<Int>, ?uvtData : flash.Vector<Float>, ?culling : TriangleCulling) : Void;
	private function get_culling() : TriangleCulling;
	private function set_culling(value : TriangleCulling) : TriangleCulling;
}
