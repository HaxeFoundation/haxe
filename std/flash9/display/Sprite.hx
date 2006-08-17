package flash.display;

extern class Sprite extends DisplayObjectContainer {
	function new() : Void;
	var buttonMode : Bool;
	var dropTarget(default,null) : DisplayObject;
	var graphics(default,null) : Graphics;
	var hitArea : Sprite;
	var soundTransform : flash.media.SoundTransform;
	function startDrag(?lockCenter : Bool, ?bounds : flash.geom.Rectangle) : Void;
	function stopDrag() : Void;
	var useHandCursor : Bool;
	private function constructChildren() : Void;
}
