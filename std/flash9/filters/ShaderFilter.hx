package flash.filters;

extern class ShaderFilter extends BitmapFilter {

	var leftExtension : Int;
	var rightExtension : Int;
	var topExtension : Int;
	var bottomExtension : Int;
	var shader : flash.display.Shader;

	function new( ?shader : flash.display.Shader ) : Void;

}
