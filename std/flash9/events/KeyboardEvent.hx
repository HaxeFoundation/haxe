package flash.events;

extern class KeyboardEvent extends flash.events.Event {
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?charCode : UInt, ?keyCode : UInt, ?keyLocation : UInt, ?ctrlKey : Bool, ?altKey : Bool, ?shiftKey : Bool) : Void;
	var altKey : Bool;
	var charCode : UInt;
	var ctrlKey : Bool;
	var keyCode : UInt;
	var keyLocation : UInt;
	var shiftKey : Bool;
	function updateAfterEvent() : Void;
	private var m_altKey : Bool;
	private var m_charCode : UInt;
	private var m_ctrlKey : Bool;
	private var m_keyCode : UInt;
	private var m_keyLocation : UInt;
	private var m_shiftKey : Bool;
	static var KEY_DOWN : String;
	static var KEY_UP : String;
}
