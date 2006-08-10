package flash.display;

extern class Sprite extends flash.display.DisplayObjectContainer {
	function new() : Void;
	var buttonMode : Bool;
	var dropTarget(default,null) : flash.display.DisplayObject;
	var graphics(default,null) : flash.display.Graphics;
	var hitArea : flash.display.Sprite;
	var soundTransform : flash.media.SoundTransform;
	function startDrag(?lockCenter : Bool, ?bounds : flash.geom.Rectangle) : Void;
	function stopDrag() : Void;
	var useHandCursor : Bool;
	private function constructChildren() : Void;
}
