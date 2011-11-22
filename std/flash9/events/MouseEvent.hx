package flash.events;

extern class MouseEvent extends Event {
	var altKey : Bool;
	var buttonDown : Bool;
	var ctrlKey : Bool;
	var delta : Int;
	@:require(flash10) var isRelatedObjectInaccessible : Bool;
	var localX : Float;
	var localY : Float;
	@:require(flash11_2) var movementX : Float;
	@:require(flash11_2) var movementY : Float;
	var relatedObject : flash.display.InteractiveObject;
	var shiftKey : Bool;
	var stageX(default,null) : Float;
	var stageY(default,null) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?localX : Float, ?localY : Float, ?relatedObject : flash.display.InteractiveObject, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false, buttonDown : Bool = false, delta : Int = 0) : Void;
	function updateAfterEvent() : Void;
	static var CLICK : String;
	@:require(flash11_2) static var CONTEXT_MENU : String;
	static var DOUBLE_CLICK : String;
	@:require(flash11_2) static var MIDDLE_CLICK : String;
	@:require(flash11_2) static var MIDDLE_MOUSE_DOWN : String;
	@:require(flash11_2) static var MIDDLE_MOUSE_UP : String;
	static var MOUSE_DOWN : String;
	static var MOUSE_MOVE : String;
	static var MOUSE_OUT : String;
	static var MOUSE_OVER : String;
	static var MOUSE_UP : String;
	static var MOUSE_WHEEL : String;
	@:require(flash11_2) static var RIGHT_CLICK : String;
	@:require(flash11_2) static var RIGHT_MOUSE_DOWN : String;
	@:require(flash11_2) static var RIGHT_MOUSE_UP : String;
	static var ROLL_OUT : String;
	static var ROLL_OVER : String;
}
