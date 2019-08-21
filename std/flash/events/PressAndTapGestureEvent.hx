package flash.events;

@:require(flash10_1) extern class PressAndTapGestureEvent extends GestureEvent {
	@:flash.property var tapLocalX(get,set) : Float;
	@:flash.property var tapLocalY(get,set) : Float;
	@:flash.property var tapStageX(get,never) : Float;
	@:flash.property var tapStageY(get,never) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?phase : String, localX : Float = 0, localY : Float = 0, tapLocalX : Float = 0, tapLocalY : Float = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	private function get_tapLocalX() : Float;
	private function get_tapLocalY() : Float;
	private function get_tapStageX() : Float;
	private function get_tapStageY() : Float;
	private function set_tapLocalX(value : Float) : Float;
	private function set_tapLocalY(value : Float) : Float;
	static final GESTURE_PRESS_AND_TAP : String;
}
