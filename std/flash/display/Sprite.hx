package flash.display;

extern class Sprite extends DisplayObjectContainer {
	@:flash.property var buttonMode(get,set) : Bool;
	@:flash.property var dropTarget(get,never) : DisplayObject;
	@:flash.property var graphics(get,never) : Graphics;
	@:flash.property var hitArea(get,set) : Sprite;
	@:flash.property var soundTransform(get,set) : flash.media.SoundTransform;
	@:flash.property var useHandCursor(get,set) : Bool;
	function new() : Void;
	private function get_buttonMode() : Bool;
	private function get_dropTarget() : DisplayObject;
	private function get_graphics() : Graphics;
	private function get_hitArea() : Sprite;
	private function get_soundTransform() : flash.media.SoundTransform;
	private function get_useHandCursor() : Bool;
	private function set_buttonMode(value : Bool) : Bool;
	private function set_hitArea(value : Sprite) : Sprite;
	private function set_soundTransform(value : flash.media.SoundTransform) : flash.media.SoundTransform;
	private function set_useHandCursor(value : Bool) : Bool;
	function startDrag(lockCenter : Bool = false, ?bounds : flash.geom.Rectangle) : Void;
	@:require(flash10_1) function startTouchDrag(touchPointID : Int, lockCenter : Bool = false, ?bounds : flash.geom.Rectangle) : Void;
	function stopDrag() : Void;
	@:require(flash10_1) function stopTouchDrag(touchPointID : Int) : Void;
}
