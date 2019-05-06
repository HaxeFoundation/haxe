package flash.filters;

extern class ShaderFilter extends BitmapFilter {
	@:flash.property var bottomExtension(get,set) : Int;
	@:flash.property var leftExtension(get,set) : Int;
	@:flash.property var rightExtension(get,set) : Int;
	@:flash.property var shader(get,set) : flash.display.Shader;
	@:flash.property var topExtension(get,set) : Int;
	function new(?shader : flash.display.Shader) : Void;
	private function get_bottomExtension() : Int;
	private function get_leftExtension() : Int;
	private function get_rightExtension() : Int;
	private function get_shader() : flash.display.Shader;
	private function get_topExtension() : Int;
	private function set_bottomExtension(value : Int) : Int;
	private function set_leftExtension(value : Int) : Int;
	private function set_rightExtension(value : Int) : Int;
	private function set_shader(value : flash.display.Shader) : flash.display.Shader;
	private function set_topExtension(value : Int) : Int;
}
