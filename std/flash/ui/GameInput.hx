package flash.ui;

@:require(flash11_8) extern class GameInput extends flash.events.EventDispatcher {
	static var isSupported(get,never) : Bool;
	static var numDevices(get,never) : Int;
	static function getDeviceAt(index : Int) : GameInputDevice;
	private static function get_isSupported() : Bool;
	private static function get_numDevices() : Int;
}
