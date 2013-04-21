package flash.ui;

extern class GameInputControl extends flash.events.EventDispatcher implements Dynamic {
	var device(default,null) : GameInputDevice;
	var id(default,null) : String;
	var maxValue(default,null) : Float;
	var minValue(default,null) : Float;
	var value(default,null) : Float;
	function new() : Void;
}
