package flash.display;

@:require(flash10) extern class Shader {
	@:flash.property var byteCode(never,set) : flash.utils.ByteArray;
	@:flash.property var data(get,set) : ShaderData;
	@:flash.property var precisionHint(get,set) : ShaderPrecision;
	function new(?code : flash.utils.ByteArray) : Void;
	private function get_data() : ShaderData;
	private function get_precisionHint() : ShaderPrecision;
	private function set_byteCode(value : flash.utils.ByteArray) : flash.utils.ByteArray;
	private function set_data(value : ShaderData) : ShaderData;
	private function set_precisionHint(value : ShaderPrecision) : ShaderPrecision;
}
