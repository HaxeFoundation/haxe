package flash.filters;

extern class ShaderFilter extends BitmapFilter {
	var bottomExtension : Int;
	var leftExtension : Int;
	var rightExtension : Int;
	var shader : flash.display.Shader;
	var topExtension : Int;
	function new(?shader : flash.display.Shader) : Void;
}
