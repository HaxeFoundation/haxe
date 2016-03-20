package flash.display;

@:require(flash10) extern class Shader {
	var byteCode(never,default) : flash.utils.ByteArray;
	var data : ShaderData;
	var precisionHint : ShaderPrecision;
	function new(?code : flash.utils.ByteArray) : Void;
}
