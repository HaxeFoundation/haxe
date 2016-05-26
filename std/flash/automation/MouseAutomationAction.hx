package flash.automation;

@:require(flash10_1) extern class MouseAutomationAction extends AutomationAction {
	var delta : Int;
	var stageX : Float;
	var stageY : Float;
	function new(type : String, stageX : Float = 0, stageY : Float = 0, delta : Int = 0) : Void;
	static var MIDDLE_MOUSE_DOWN(default,never) : String;
	static var MIDDLE_MOUSE_UP(default,never) : String;
	static var MOUSE_DOWN(default,never) : String;
	static var MOUSE_MOVE(default,never) : String;
	static var MOUSE_UP(default,never) : String;
	static var MOUSE_WHEEL(default,never) : String;
	static var RIGHT_MOUSE_DOWN(default,never) : String;
	static var RIGHT_MOUSE_UP(default,never) : String;
}
