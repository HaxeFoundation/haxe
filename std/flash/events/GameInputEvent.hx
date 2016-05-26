package flash.events;

@:final @:require(flash11_8) extern class GameInputEvent extends Event {
	var device(default,never) : flash.ui.GameInputDevice;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?device : flash.ui.GameInputDevice) : Void;
	static var DEVICE_ADDED(default,never) : String;
	static var DEVICE_REMOVED(default,never) : String;
	static var DEVICE_UNUSABLE(default,never) : String;
}
