package flash.events;

@:require(flash10_1) extern class AccelerometerEvent extends Event {
	@:flash.property var accelerationX(get,set) : Float;
	@:flash.property var accelerationY(get,set) : Float;
	@:flash.property var accelerationZ(get,set) : Float;
	@:flash.property var timestamp(get,set) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, timestamp : Float = 0, accelerationX : Float = 0, accelerationY : Float = 0, accelerationZ : Float = 0) : Void;
	private function get_accelerationX() : Float;
	private function get_accelerationY() : Float;
	private function get_accelerationZ() : Float;
	private function get_timestamp() : Float;
	private function set_accelerationX(value : Float) : Float;
	private function set_accelerationY(value : Float) : Float;
	private function set_accelerationZ(value : Float) : Float;
	private function set_timestamp(value : Float) : Float;
	static final UPDATE : String;
}
