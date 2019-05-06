package flash.events;

@:require(flash10_1) extern class TransformGestureEvent extends GestureEvent {
	@:flash.property var offsetX(get,set) : Float;
	@:flash.property var offsetY(get,set) : Float;
	@:flash.property var rotation(get,set) : Float;
	@:flash.property var scaleX(get,set) : Float;
	@:flash.property var scaleY(get,set) : Float;
	@:flash.property var velocity(get,set) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?phase : String, localX : Float = 0, localY : Float = 0, scaleX : Float = 1, scaleY : Float = 1, rotation : Float = 0, offsetX : Float = 0, offsetY : Float = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	private function get_offsetX() : Float;
	private function get_offsetY() : Float;
	private function get_rotation() : Float;
	private function get_scaleX() : Float;
	private function get_scaleY() : Float;
	private function get_velocity() : Float;
	private function set_offsetX(value : Float) : Float;
	private function set_offsetY(value : Float) : Float;
	private function set_rotation(value : Float) : Float;
	private function set_scaleX(value : Float) : Float;
	private function set_scaleY(value : Float) : Float;
	private function set_velocity(value : Float) : Float;
	static final GESTURE_DIRECTIONAL_TAP : String;
	static final GESTURE_PAN : String;
	static final GESTURE_ROTATE : String;
	static final GESTURE_SWIPE : String;
	static final GESTURE_ZOOM : String;
}
