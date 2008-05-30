package flash.events;

extern class KeyboardEvent extends Event {
	var altKey : Bool;
	var charCode : UInt;
	var ctrlKey : Bool;
	var keyCode : UInt;
	var keyLocation : flash.ui.KeyLocation;
	var shiftKey : Bool;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?charCode : UInt, ?keyCode : UInt, ?keyLocation : flash.ui.KeyLocation, ?ctrlKey : Bool, ?altKey : Bool, ?shiftKey : Bool) : Void;
	function updateAfterEvent() : Void;
	static var KEY_DOWN : String;
	static var KEY_UP : String;
}
