package flash.xml;

extern final class XMLTag {
	var attrs(get,set) : Dynamic;
	var empty(get,set) : Bool;
	var type(get,set) : XMLNodeType;
	var value(get,set) : String;
	function new() : Void;
	private function get_attrs() : Dynamic;
	private function get_empty() : Bool;
	private function get_type() : XMLNodeType;
	private function get_value() : String;
	private function set_attrs(value : Dynamic) : Dynamic;
	private function set_empty(value : Bool) : Bool;
	private function set_type(value : XMLNodeType) : XMLNodeType;
	private function set_value(value : String) : String;
}
