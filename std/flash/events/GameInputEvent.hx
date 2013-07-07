package flash.events;

@:final @:require(flash11_2) extern class GameInputEvent extends Event {
	var device(default,null) : flash.ui.GameInputDevice;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?device : flash.ui.GameInputDevice) : Void;
	static var DEVICE_ADDED : String;
	static var DEVICE_REMOVED : String;
}
