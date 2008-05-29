package flash.display;

extern class GraphicsShaderFill implements IGraphicsFill, implements IGraphicsData {

	var matrix : flash.geom.Matrix;
	var shader : Shader;

	public function new( ?shader : Shader, ?matrix : flash.geom.Matrix ) : Void;
}
