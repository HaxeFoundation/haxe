package flash.events;

extern class KeyboardEvent extends Event {
	var altKey : Bool;
	var charCode : UInt;
	var ctrlKey : Bool;
	var keyCode : UInt;
	var keyLocation : flash.ui.KeyLocation;
	var shiftKey : Bool;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, charCode : UInt = 0, keyCode : UInt = 0, keyLocation : flash.ui.KeyLocation = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	function updateAfterEvent() : Void;
	static var KEY_DOWN : String;
	static var KEY_UP : String;
}
