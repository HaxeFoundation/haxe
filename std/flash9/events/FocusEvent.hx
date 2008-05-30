package flash.events;

extern class FocusEvent extends Event {
	var keyCode : UInt;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?relatedObject : flash.display.InteractiveObject, ?shiftKey : Bool, ?keyCode : UInt) : Void;
	static var FOCUS_IN : String;
	static var FOCUS_OUT : String;
	static var KEY_FOCUS_CHANGE : String;
	static var MOUSE_FOCUS_CHANGE : String;
}
