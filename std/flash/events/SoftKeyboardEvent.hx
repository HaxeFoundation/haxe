package flash.events;

extern class SoftKeyboardEvent extends Event {
	var relatedObject : flash.display.InteractiveObject;
	var triggerType(default,null) : String;
	function new(type : String, bubbles : Bool, cancelable : Bool, relatedObjectVal : flash.display.InteractiveObject, triggerTypeVal : String) : Void;
	static var SOFT_KEYBOARD_ACTIVATE(default,never) : String;
	static var SOFT_KEYBOARD_ACTIVATING(default,never) : String;
	static var SOFT_KEYBOARD_DEACTIVATE(default,never) : String;
}
