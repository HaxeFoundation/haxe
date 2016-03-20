package flash.events;

@:require(flash10_1) extern class GestureEvent extends Event {
	var altKey : Bool;
	var ctrlKey : Bool;
	var localX : Float;
	var localY : Float;
	var phase : String;
	var shiftKey : Bool;
	var stageX(default,never) : Float;
	var stageY(default,never) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?phase : String, localX : Float = 0, localY : Float = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	function updateAfterEvent() : Void;
	static var GESTURE_TWO_FINGER_TAP(default,never) : String;
}
