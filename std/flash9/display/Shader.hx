package flash.display;

extern class Shader {
	function new(?code : flash.utils.ByteArray) : Void;
	var byteCode(null,default) : flash.utils.ByteArray;
	var data : flash.display.ShaderData;
	var precisionHint : ShaderPrecision;
}
