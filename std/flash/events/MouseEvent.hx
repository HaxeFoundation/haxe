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
	var stageX(default,never) : Float;
	var stageY(default,never) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, localX : Null<Float> = 0, localY : Null<Float> = 0, ?relatedObject : flash.display.InteractiveObject, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false, buttonDown : Bool = false, delta : Int = 0) : Void;
	function updateAfterEvent() : Void;
	static var CLICK(default,never) : String;
	@:require(flash11_2) static var CONTEXT_MENU(default,never) : String;
	static var DOUBLE_CLICK(default,never) : String;
	@:require(flash11_2) static var MIDDLE_CLICK(default,never) : String;
	@:require(flash11_2) static var MIDDLE_MOUSE_DOWN(default,never) : String;
	@:require(flash11_2) static var MIDDLE_MOUSE_UP(default,never) : String;
	static var MOUSE_DOWN(default,never) : String;
	static var MOUSE_MOVE(default,never) : String;
	static var MOUSE_OUT(default,never) : String;
	static var MOUSE_OVER(default,never) : String;
	static var MOUSE_UP(default,never) : String;
	static var MOUSE_WHEEL(default,never) : String;
	@:require(flash11_3) static var RELEASE_OUTSIDE(default,never) : String;
	@:require(flash11_2) static var RIGHT_CLICK(default,never) : String;
	@:require(flash11_2) static var RIGHT_MOUSE_DOWN(default,never) : String;
	@:require(flash11_2) static var RIGHT_MOUSE_UP(default,never) : String;
	static var ROLL_OUT(default,never) : String;
	static var ROLL_OVER(default,never) : String;
}
