package flash.events;

extern class FocusEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?relatedObject : flash.display.InteractiveObject, ?shiftKey : Bool, ?keyCode : UInt) : Void;
	var keyCode : UInt;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	private var m_keyCode : UInt;
	private var m_relatedObject : flash.display.InteractiveObject;
	private var m_shiftKey : Bool;
	static var FOCUS_IN : String;
	static var FOCUS_OUT : String;
	static var KEY_FOCUS_CHANGE : String;
	static var MOUSE_FOCUS_CHANGE : String;
}
