package flash.ui;

extern class GameInputControl extends flash.events.EventDispatcher implements Dynamic {
	@:flash.property var device(get,never) : GameInputDevice;
	@:flash.property var id(get,never) : String;
	@:flash.property var maxValue(get,never) : Float;
	@:flash.property var minValue(get,never) : Float;
	@:flash.property var value(get,never) : Float;
	function new() : Void;
	private function get_device() : GameInputDevice;
	private function get_id() : String;
	private function get_maxValue() : Float;
	private function get_minValue() : Float;
	private function get_value() : Float;
}
