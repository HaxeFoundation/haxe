package flash.events;

extern class FocusEvent extends Event {
	@:require(flash10) var isRelatedObjectInaccessible : Bool;
	var keyCode : UInt;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?relatedObject : flash.display.InteractiveObject, shiftKey : Bool = false, keyCode : UInt = 0) : Void;
	static var FOCUS_IN(default,never) : String;
	static var FOCUS_OUT(default,never) : String;
	static var KEY_FOCUS_CHANGE(default,never) : String;
	static var MOUSE_FOCUS_CHANGE(default,never) : String;
}
