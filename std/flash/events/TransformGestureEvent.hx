package flash.events;

@:require(flash10_1) extern class TransformGestureEvent extends GestureEvent {
	var offsetX : Float;
	var offsetY : Float;
	var rotation : Float;
	var scaleX : Float;
	var scaleY : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?phase : String, localX : Float = 0, localY : Float = 0, scaleX : Float = 1, scaleY : Float = 1, rotation : Float = 0, offsetX : Float = 0, offsetY : Float = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	static var GESTURE_PAN(default,never) : String;
	static var GESTURE_ROTATE(default,never) : String;
	static var GESTURE_SWIPE(default,never) : String;
	static var GESTURE_ZOOM(default,never) : String;
}
