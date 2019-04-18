package flash.automation;

@:require(flash10_1) extern class MouseAutomationAction extends AutomationAction {
	var delta : Int;
	var stageX : Float;
	var stageY : Float;
	function new(type : String, stageX : Float = 0, stageY : Float = 0, delta : Int = 0) : Void;
	static final MIDDLE_MOUSE_DOWN : String;
	static final MIDDLE_MOUSE_UP : String;
	static final MOUSE_DOWN : String;
	static final MOUSE_MOVE : String;
	static final MOUSE_UP : String;
	static final MOUSE_WHEEL : String;
	static final RIGHT_MOUSE_DOWN : String;
	static final RIGHT_MOUSE_UP : String;
}
