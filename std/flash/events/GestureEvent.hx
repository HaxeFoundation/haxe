package flash.events;

@:require(flash10_1) extern class GestureEvent extends Event {
	@:flash.property var altKey(get,set) : Bool;
	@:flash.property var ctrlKey(get,set) : Bool;
	@:flash.property var localX(get,set) : Float;
	@:flash.property var localY(get,set) : Float;
	@:flash.property var phase(get,set) : String;
	@:flash.property var shiftKey(get,set) : Bool;
	@:flash.property var stageX(get,never) : Float;
	@:flash.property var stageY(get,never) : Float;
	function new(type : String, bubbles : Bool = true, cancelable : Bool = false, ?phase : String, localX : Float = 0, localY : Float = 0, ctrlKey : Bool = false, altKey : Bool = false, shiftKey : Bool = false) : Void;
	private function get_altKey() : Bool;
	private function get_ctrlKey() : Bool;
	private function get_localX() : Float;
	private function get_localY() : Float;
	private function get_phase() : String;
	private function get_shiftKey() : Bool;
	private function get_stageX() : Float;
	private function get_stageY() : Float;
	private function set_altKey(value : Bool) : Bool;
	private function set_ctrlKey(value : Bool) : Bool;
	private function set_localX(value : Float) : Float;
	private function set_localY(value : Float) : Float;
	private function set_phase(value : String) : String;
	private function set_shiftKey(value : Bool) : Bool;
	function updateAfterEvent() : Void;
	static final GESTURE_TWO_FINGER_TAP : String;
}
