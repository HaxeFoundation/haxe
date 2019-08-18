package flash.events;

extern class MouseEvent extends Event {
	@:flash.property var altKey(get,set) : Bool;
	@:flash.property var buttonDown(get,set) : Bool;
	@:flash.property var ctrlKey(get,set) : Bool;
	@:flash.property var delta(get,set) : Int;
	@:flash.property @:require(flash10) var isRelatedObjectInaccessible(get,set) : Bool;
	@:flash.property var localX(get,set) : Float;
	@:flash.property var localY(get,set) : Float;
	@:flash.property @:require(flash11_2) var movementX(get,set) : Float;
	@:flash.property @:require(flash11_2) var movementY(get,set) : Float;
	@:flash.property var relatedObject(get,set) : flash.display.InteractiveObject;
	@:flash.property var shiftKey(get,set) : Bool;
	@:flash.property var stageX(get,never) : Float;
	@:flash.property var stageY(get,never) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, localX : Null<Float> = 0, localY : Null<Float> = 0, ?relatedObject : flash.display.InteractiveObject, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false, buttonDown : Bool = false, delta : Int = 0) : Void;
	private function get_altKey() : Bool;
	private function get_buttonDown() : Bool;
	private function get_ctrlKey() : Bool;
	private function get_delta() : Int;
	private function get_isRelatedObjectInaccessible() : Bool;
	private function get_localX() : Float;
	private function get_localY() : Float;
	private function get_movementX() : Float;
	private function get_movementY() : Float;
	private function get_relatedObject() : flash.display.InteractiveObject;
	private function get_shiftKey() : Bool;
	private function get_stageX() : Float;
	private function get_stageY() : Float;
	private function set_altKey(value : Bool) : Bool;
	private function set_buttonDown(value : Bool) : Bool;
	private function set_ctrlKey(value : Bool) : Bool;
	private function set_delta(value : Int) : Int;
	private function set_isRelatedObjectInaccessible(value : Bool) : Bool;
	private function set_localX(value : Float) : Float;
	private function set_localY(value : Float) : Float;
	private function set_movementX(value : Float) : Float;
	private function set_movementY(value : Float) : Float;
	private function set_relatedObject(value : flash.display.InteractiveObject) : flash.display.InteractiveObject;
	private function set_shiftKey(value : Bool) : Bool;
	function updateAfterEvent() : Void;
	static final CLICK : String;
	@:require(flash11_2) static final CONTEXT_MENU : String;
	static final DOUBLE_CLICK : String;
	@:require(flash11_2) static final MIDDLE_CLICK : String;
	@:require(flash11_2) static final MIDDLE_MOUSE_DOWN : String;
	@:require(flash11_2) static final MIDDLE_MOUSE_UP : String;
	static final MOUSE_DOWN : String;
	static final MOUSE_MOVE : String;
	static final MOUSE_OUT : String;
	static final MOUSE_OVER : String;
	static final MOUSE_UP : String;
	static final MOUSE_WHEEL : String;
	@:require(flash11_3) static final RELEASE_OUTSIDE : String;
	@:require(flash11_2) static final RIGHT_CLICK : String;
	@:require(flash11_2) static final RIGHT_MOUSE_DOWN : String;
	@:require(flash11_2) static final RIGHT_MOUSE_UP : String;
	static final ROLL_OUT : String;
	static final ROLL_OVER : String;
}
