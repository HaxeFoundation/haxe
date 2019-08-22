package flash.ui;

@:require(flash10_1) extern class Multitouch {
	@:flash.property static var inputMode(get,set) : MultitouchInputMode;
	@:flash.property static var mapTouchToMouse(get,set) : Bool;
	@:flash.property static var maxTouchPoints(get,never) : Int;
	@:flash.property static var supportedGestures(get,never) : flash.Vector<String>;
	@:flash.property static var supportsGestureEvents(get,never) : Bool;
	@:flash.property static var supportsTouchEvents(get,never) : Bool;
	private static function get_inputMode() : MultitouchInputMode;
	private static function get_mapTouchToMouse() : Bool;
	private static function get_maxTouchPoints() : Int;
	private static function get_supportedGestures() : flash.Vector<String>;
	private static function get_supportsGestureEvents() : Bool;
	private static function get_supportsTouchEvents() : Bool;
	private static function set_inputMode(value : MultitouchInputMode) : MultitouchInputMode;
	private static function set_mapTouchToMouse(value : Bool) : Bool;
}
