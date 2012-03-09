package flash.ui;

@:final extern class GameInputControl extends flash.events.EventDispatcher {
	var device(default,null) : GameInputDevice;
	var finger(default,null) : GameInputFinger;
	var hand(default,null) : GameInputHand;
	var index(default,null) : Int;
	var numValues(default,null) : Int;
	var relative(default,null) : Bool;
	var type(default,null) : GameInputControlType;
	function new() : Void;
	function getValueAt(index : Int = 0) : Float;
}
