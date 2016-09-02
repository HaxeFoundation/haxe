package flash.ui;

extern class GameInputControl extends flash.events.EventDispatcher implements Dynamic {
	var device(default,never) : GameInputDevice;
	var id(default,never) : String;
	var maxValue(default,never) : Float;
	var minValue(default,never) : Float;
	var value(default,never) : Float;
	function new() : Void;
}
