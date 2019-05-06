package flash.events;

@:require(flash10_1) extern class GeolocationEvent extends Event {
	@:flash.property var altitude(get,set) : Float;
	@:flash.property var heading(get,set) : Float;
	@:flash.property var horizontalAccuracy(get,set) : Float;
	@:flash.property var latitude(get,set) : Float;
	@:flash.property var longitude(get,set) : Float;
	@:flash.property var speed(get,set) : Float;
	@:flash.property var timestamp(get,set) : Float;
	@:flash.property var verticalAccuracy(get,set) : Float;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, latitude : Float = 0, longitude : Float = 0, altitude : Float = 0, hAccuracy : Float = 0, vAccuracy : Float = 0, speed : Float = 0, heading : Float = 0, timestamp : Float = 0) : Void;
	private function get_altitude() : Float;
	private function get_heading() : Float;
	private function get_horizontalAccuracy() : Float;
	private function get_latitude() : Float;
	private function get_longitude() : Float;
	private function get_speed() : Float;
	private function get_timestamp() : Float;
	private function get_verticalAccuracy() : Float;
	private function set_altitude(value : Float) : Float;
	private function set_heading(value : Float) : Float;
	private function set_horizontalAccuracy(value : Float) : Float;
	private function set_latitude(value : Float) : Float;
	private function set_longitude(value : Float) : Float;
	private function set_speed(value : Float) : Float;
	private function set_timestamp(value : Float) : Float;
	private function set_verticalAccuracy(value : Float) : Float;
	static final UPDATE : String;
}
