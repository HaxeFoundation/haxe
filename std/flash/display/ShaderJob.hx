package flash.display;

extern class ShaderJob extends flash.events.EventDispatcher {
	@:flash.property var height(get,set) : Int;
	@:flash.property var progress(get,never) : Float;
	@:flash.property var shader(get,set) : Shader;
	@:flash.property var target(get,set) : Dynamic;
	@:flash.property var width(get,set) : Int;
	function new(?shader : Shader, ?target : Dynamic, width : Int = 0, height : Int = 0) : Void;
	function cancel() : Void;
	private function get_height() : Int;
	private function get_progress() : Float;
	private function get_shader() : Shader;
	private function get_target() : Dynamic;
	private function get_width() : Int;
	private function set_height(value : Int) : Int;
	private function set_shader(value : Shader) : Shader;
	private function set_target(value : Dynamic) : Dynamic;
	private function set_width(value : Int) : Int;
	function start(waitForCompletion : Bool = false) : Void;
}
