package flash.events;

extern class SoftKeyboardEvent extends Event {
	@:flash.property var relatedObject(get,set) : flash.display.InteractiveObject;
	@:flash.property var triggerType(get,never) : String;
	function new(type : String, bubbles : Bool, cancelable : Bool, relatedObjectVal : flash.display.InteractiveObject, triggerTypeVal : String) : Void;
	private function get_relatedObject() : flash.display.InteractiveObject;
	private function get_triggerType() : String;
	private function set_relatedObject(value : flash.display.InteractiveObject) : flash.display.InteractiveObject;
	static final SOFT_KEYBOARD_ACTIVATE : String;
	static final SOFT_KEYBOARD_ACTIVATING : String;
	static final SOFT_KEYBOARD_DEACTIVATE : String;
}
