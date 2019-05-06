package flash.automation;

@:require(flash10_1) extern class KeyboardAutomationAction extends AutomationAction {
	@:flash.property var keyCode(get,set) : UInt;
	function new(type : String, keyCode : UInt = 0) : Void;
	private function get_keyCode() : UInt;
	private function set_keyCode(value : UInt) : UInt;
	static final KEY_DOWN : String;
	static final KEY_UP : String;
}
