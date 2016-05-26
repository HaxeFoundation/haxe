package flash.display;

extern class Sprite extends DisplayObjectContainer {
	var buttonMode : Bool;
	var dropTarget(default,never) : DisplayObject;
	var graphics(default,never) : Graphics;
	var hitArea : Sprite;
	var soundTransform : flash.media.SoundTransform;
	var useHandCursor : Bool;
	function new() : Void;
	function startDrag(lockCenter : Bool = false, ?bounds : flash.geom.Rectangle) : Void;
	@:require(flash10_1) function startTouchDrag(touchPointID : Int, lockCenter : Bool = false, ?bounds : flash.geom.Rectangle) : Void;
	function stopDrag() : Void;
	@:require(flash10_1) function stopTouchDrag(touchPointID : Int) : Void;
}
