package flash.display;

extern final class GraphicsShaderFill implements IGraphicsData implements IGraphicsFill {
	var matrix : flash.geom.Matrix;
	var shader : Shader;
	function new(?shader : Shader, ?matrix : flash.geom.Matrix) : Void;
}
