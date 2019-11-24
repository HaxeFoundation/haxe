package flash.events;

@:require(flash11_8) extern final class GameInputEvent extends Event {
	@:flash.property var device(get,never) : flash.ui.GameInputDevice;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?device : flash.ui.GameInputDevice) : Void;
	private function get_device() : flash.ui.GameInputDevice;
	static final DEVICE_ADDED : String;
	static final DEVICE_REMOVED : String;
	static final DEVICE_UNUSABLE : String;
}
