package flash.display;

extern class SimpleButton extends flash.display.InteractiveObject {
	function new(?upState : flash.display.DisplayObject, ?overState : flash.display.DisplayObject, ?downState : flash.display.DisplayObject, ?hitTestState : flash.display.DisplayObject) : Void;
	var downState : flash.display.DisplayObject;
	var enabled : Bool;
	var hitTestState : flash.display.DisplayObject;
	var overState : flash.display.DisplayObject;
	var soundTransform : flash.media.SoundTransform;
	var trackAsMenu : Bool;
	var upState : flash.display.DisplayObject;
	var useHandCursor : Bool;
	private function _updateButton() : Void;
}
