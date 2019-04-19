package flash.events;

extern class SoftKeyboardEvent extends Event {
	var relatedObject : flash.display.InteractiveObject;
	var triggerType(default,never) : String;
	function new(type : String, bubbles : Bool, cancelable : Bool, relatedObjectVal : flash.display.InteractiveObject, triggerTypeVal : String) : Void;
	static final SOFT_KEYBOARD_ACTIVATE : String;
	static final SOFT_KEYBOARD_ACTIVATING : String;
	static final SOFT_KEYBOARD_DEACTIVATE : String;
}
