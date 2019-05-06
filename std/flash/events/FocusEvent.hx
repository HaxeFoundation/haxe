package flash.events;

extern class FocusEvent extends Event {
	@:flash.property @:require(flash10) var isRelatedObjectInaccessible(get,set) : Bool;
	@:flash.property var keyCode(get,set) : UInt;
	@:flash.property var relatedObject(get,set) : flash.display.InteractiveObject;
	@:flash.property var shiftKey(get,set) : Bool;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?relatedObject : flash.display.InteractiveObject, shiftKey : Bool = false, keyCode : UInt = 0) : Void;
	private function get_isRelatedObjectInaccessible() : Bool;
	private function get_keyCode() : UInt;
	private function get_relatedObject() : flash.display.InteractiveObject;
	private function get_shiftKey() : Bool;
	private function set_isRelatedObjectInaccessible(value : Bool) : Bool;
	private function set_keyCode(value : UInt) : UInt;
	private function set_relatedObject(value : flash.display.InteractiveObject) : flash.display.InteractiveObject;
	private function set_shiftKey(value : Bool) : Bool;
	static final FOCUS_IN : String;
	static final FOCUS_OUT : String;
	static final KEY_FOCUS_CHANGE : String;
	static final MOUSE_FOCUS_CHANGE : String;
}
