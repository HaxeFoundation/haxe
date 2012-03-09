package flash.display;

extern class SimpleButton extends InteractiveObject {
	var downState : DisplayObject;
	var enabled : Bool;
	var hitTestState : DisplayObject;
	var overState : DisplayObject;
	var soundTransform : flash.media.SoundTransform;
	var trackAsMenu : Bool;
	var upState : DisplayObject;
	var useHandCursor : Bool;
	function new(?upState : DisplayObject, ?overState : DisplayObject, ?downState : DisplayObject, ?hitTestState : DisplayObject) : Void;
}
