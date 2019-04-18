package flash.automation;

@:require(flash10_1) extern class KeyboardAutomationAction extends AutomationAction {
	var keyCode : UInt;
	function new(type : String, keyCode : UInt = 0) : Void;
	static final KEY_DOWN : String;
	static final KEY_UP : String;
}
