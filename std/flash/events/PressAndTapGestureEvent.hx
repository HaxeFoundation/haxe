package flash.events;

@:require(flash10_1) extern class PressAndTapGestureEvent extends GestureEvent {
	var tapLocalX : Float;
	var tapLocalY : Float;
	var tapStageX(default,never) : Float;
	var tapStageY(default,never) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?phase : String, localX : Float = 0, localY : Float = 0, tapLocalX : Float = 0, tapLocalY : Float = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	static var GESTURE_PRESS_AND_TAP(default,never) : String;
}
