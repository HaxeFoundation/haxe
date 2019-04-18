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
	static final CLICK : String;
	@:require(flash11_2) static final CONTEXT_MENU : String;
	static final DOUBLE_CLICK : String;
	@:require(flash11_2) static final MIDDLE_CLICK : String;
	@:require(flash11_2) static final MIDDLE_MOUSE_DOWN : String;
	@:require(flash11_2) static final MIDDLE_MOUSE_UP : String;
	static final MOUSE_DOWN : String;
	static final MOUSE_MOVE : String;
	static final MOUSE_OUT : String;
	static final MOUSE_OVER : String;
	static final MOUSE_UP : String;
	static final MOUSE_WHEEL : String;
	@:require(flash11_3) static final RELEASE_OUTSIDE : String;
	@:require(flash11_2) static final RIGHT_CLICK : String;
	@:require(flash11_2) static final RIGHT_MOUSE_DOWN : String;
	@:require(flash11_2) static final RIGHT_MOUSE_UP : String;
	static final ROLL_OUT : String;
	static final ROLL_OVER : String;
}
