package flash.net;

extern final class FileFilter {
	@:flash.property var description(get,set) : String;
	@:flash.property var extension(get,set) : String;
	@:flash.property var macType(get,set) : String;
	function new(description : String, extension : String, ?macType : String) : Void;
	private function get_description() : String;
	private function get_extension() : String;
	private function get_macType() : String;
	private function set_description(value : String) : String;
	private function set_extension(value : String) : String;
	private function set_macType(value : String) : String;
}
