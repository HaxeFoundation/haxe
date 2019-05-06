package flash.display;

extern final class ShaderParameter implements Dynamic {
	@:flash.property var index(get,never) : Int;
	@:flash.property var type(get,never) : ShaderParameterType;
	@:flash.property var value(get,set) : Array<Dynamic>;
	private function get_index() : Int;
	private function get_type() : ShaderParameterType;
	private function get_value() : Array<Dynamic>;
	private function set_value(value : Array<Dynamic>) : Array<Dynamic>;
}
