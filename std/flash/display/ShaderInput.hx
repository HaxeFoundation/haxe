package flash.display;

extern final class ShaderInput implements Dynamic {
	@:flash.property var channels(get,never) : Int;
	@:flash.property var height(get,set) : Int;
	@:flash.property var index(get,never) : Int;
	@:flash.property var input(get,set) : Dynamic;
	@:flash.property var width(get,set) : Int;
	function new() : Void;
	private function get_channels() : Int;
	private function get_height() : Int;
	private function get_index() : Int;
	private function get_input() : Dynamic;
	private function get_width() : Int;
	private function set_height(value : Int) : Int;
	private function set_input(value : Dynamic) : Dynamic;
	private function set_width(value : Int) : Int;
}
