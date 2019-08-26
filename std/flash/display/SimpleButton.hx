package flash.display;

extern class SimpleButton extends InteractiveObject {
	@:flash.property var downState(get,set) : DisplayObject;
	@:flash.property var enabled(get,set) : Bool;
	@:flash.property var hitTestState(get,set) : DisplayObject;
	@:flash.property var overState(get,set) : DisplayObject;
	@:flash.property var soundTransform(get,set) : flash.media.SoundTransform;
	@:flash.property var trackAsMenu(get,set) : Bool;
	@:flash.property var upState(get,set) : DisplayObject;
	@:flash.property var useHandCursor(get,set) : Bool;
	function new(?upState : DisplayObject, ?overState : DisplayObject, ?downState : DisplayObject, ?hitTestState : DisplayObject) : Void;
	private function get_downState() : DisplayObject;
	private function get_enabled() : Bool;
	private function get_hitTestState() : DisplayObject;
	private function get_overState() : DisplayObject;
	private function get_soundTransform() : flash.media.SoundTransform;
	private function get_trackAsMenu() : Bool;
	private function get_upState() : DisplayObject;
	private function get_useHandCursor() : Bool;
	private function set_downState(value : DisplayObject) : DisplayObject;
	private function set_enabled(value : Bool) : Bool;
	private function set_hitTestState(value : DisplayObject) : DisplayObject;
	private function set_overState(value : DisplayObject) : DisplayObject;
	private function set_soundTransform(value : flash.media.SoundTransform) : flash.media.SoundTransform;
	private function set_trackAsMenu(value : Bool) : Bool;
	private function set_upState(value : DisplayObject) : DisplayObject;
	private function set_useHandCursor(value : Bool) : Bool;
}
