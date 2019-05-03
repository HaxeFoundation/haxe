package flash.display;

extern final class ShaderParameter implements Dynamic {
	var index(get,never) : Int;
	var type(get,never) : ShaderParameterType;
	var value(get,set) : Array<Dynamic>;
	private function get_index() : Int;
	private function get_type() : ShaderParameterType;
	private function get_value() : Array<Dynamic>;
	private function set_value(value : Array<Dynamic>) : Array<Dynamic>;
}
