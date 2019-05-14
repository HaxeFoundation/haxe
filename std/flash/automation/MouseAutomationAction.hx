package flash.automation;

@:require(flash10_1) extern class MouseAutomationAction extends AutomationAction {
	@:flash.property var delta(get,set) : Int;
	@:flash.property var stageX(get,set) : Float;
	@:flash.property var stageY(get,set) : Float;
	function new(type : String, stageX : Float = 0, stageY : Float = 0, delta : Int = 0) : Void;
	private function get_delta() : Int;
	private function get_stageX() : Float;
	private function get_stageY() : Float;
	private function set_delta(value : Int) : Int;
	private function set_stageX(value : Float) : Float;
	private function set_stageY(value : Float) : Float;
	static final MIDDLE_MOUSE_DOWN : String;
	static final MIDDLE_MOUSE_UP : String;
	static final MOUSE_DOWN : String;
	static final MOUSE_MOVE : String;
	static final MOUSE_UP : String;
	static final MOUSE_WHEEL : String;
	static final RIGHT_MOUSE_DOWN : String;
	static final RIGHT_MOUSE_UP : String;
}
