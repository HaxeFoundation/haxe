package flash.automation;

@:require(flash10_1) extern class AutomationAction {
	@:flash.property var type(get,set) : String;
	function new() : Void;
	private function get_type() : String;
	private function set_type(value : String) : String;
}
