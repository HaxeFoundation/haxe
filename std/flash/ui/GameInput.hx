package flash.ui;

@:require(flash11_8) extern class GameInput extends flash.events.EventDispatcher {
	static var isSupported(default,never) : Bool;
	static var numDevices(default,never) : Int;
	static function getDeviceAt(index : Int) : GameInputDevice;
}
