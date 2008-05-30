package flash.events;

extern class MouseEvent extends Event {
	var altKey : Bool;
	var buttonDown : Bool;
	var ctrlKey : Bool;
	var delta : Int;
	var localX : Float;
	var localY : Float;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	var stageX(default,null) : Float;
	var stageY(default,null) : Float;
	function new(type : String, ?bubbles : Bool, ?cancelable : Bool, ?localX : Float, ?localY : Float, ?relatedObject : flash.display.InteractiveObject, ?ctrlKey : Bool, ?altKey : Bool, ?shiftKey : Bool, ?buttonDown : Bool, ?delta : Int) : Void;
	function updateAfterEvent() : Void;
	static var CLICK : String;
	static var DOUBLE_CLICK : String;
	static var MOUSE_DOWN : String;
	static var MOUSE_MOVE : String;
	static var MOUSE_OUT : String;
	static var MOUSE_OVER : String;
	static var MOUSE_UP : String;
	static var MOUSE_WHEEL : String;
	static var ROLL_OUT : String;
	static var ROLL_OVER : String;
}
