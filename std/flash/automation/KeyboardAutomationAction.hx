package flash.automation;

@:require(flash10_1) extern class KeyboardAutomationAction extends AutomationAction {
	var keyCode : UInt;
	function new(type : String, keyCode : UInt = 0) : Void;
	static var KEY_DOWN(default,never) : String;
	static var KEY_UP(default,never) : String;
}
