package flash.ui;

extern class Mouse {
	@:flash.property @:require(flash10) static var cursor(get,set) : Dynamic;
	@:flash.property @:require(flash10_1) static var supportsCursor(get,never) : Bool;
	@:flash.property @:require(flash11) static var supportsNativeCursor(get,never) : Bool;
	private static function get_cursor() : Dynamic;
	private static function get_supportsCursor() : Bool;
	private static function get_supportsNativeCursor() : Bool;
	static function hide() : Void;
	@:require(flash10_2) static function registerCursor(name : String, cursor : MouseCursorData) : Void;
	private static function set_cursor(value : Dynamic) : Dynamic;
	static function show() : Void;
	@:require(flash11) static function unregisterCursor(name : String) : Void;
}
