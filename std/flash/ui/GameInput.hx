package flash.ui;

@:require(flash11_8) extern class GameInput extends flash.events.EventDispatcher {
	@:flash.property static var isSupported(get,never) : Bool;
	@:flash.property static var numDevices(get,never) : Int;
	static function getDeviceAt(index : Int) : GameInputDevice;
	private static function get_isSupported() : Bool;
	private static function get_numDevices() : Int;
}
