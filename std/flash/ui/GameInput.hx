package flash.ui;

@:require(flash11_8) extern class GameInput extends flash.events.EventDispatcher {
	static var isSupported(default,null) : Bool;
	static var numDevices(default,null) : Int;
	static function getDeviceAt(index : Int) : GameInputDevice;
}
