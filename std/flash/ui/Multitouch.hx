package flash.ui;

@:require(flash10_1) extern class Multitouch {
	static var inputMode(get,set) : MultitouchInputMode;
	static var mapTouchToMouse(get,set) : Bool;
	static var maxTouchPoints(get,never) : Int;
	static var supportedGestures(get,never) : flash.Vector<String>;
	static var supportsGestureEvents(get,never) : Bool;
	static var supportsTouchEvents(get,never) : Bool;
	private static function get_inputMode() : MultitouchInputMode;
	private static function get_mapTouchToMouse() : Bool;
	private static function get_maxTouchPoints() : Int;
	private static function get_supportedGestures() : flash.Vector<String>;
	private static function get_supportsGestureEvents() : Bool;
	private static function get_supportsTouchEvents() : Bool;
	private static function set_inputMode(value : MultitouchInputMode) : MultitouchInputMode;
	private static function set_mapTouchToMouse(value : Bool) : Bool;
}
