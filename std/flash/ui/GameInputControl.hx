package flash.ui;

extern class GameInputControl extends flash.events.EventDispatcher implements Dynamic {
	var device(get,never) : GameInputDevice;
	var id(get,never) : String;
	var maxValue(get,never) : Float;
	var minValue(get,never) : Float;
	var value(get,never) : Float;
	function new() : Void;
	private function get_device() : GameInputDevice;
	private function get_id() : String;
	private function get_maxValue() : Float;
	private function get_minValue() : Float;
	private function get_value() : Float;
}
