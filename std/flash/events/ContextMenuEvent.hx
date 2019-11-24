package flash.events;

extern class ContextMenuEvent extends Event {
	@:flash.property var contextMenuOwner(get,set) : flash.display.InteractiveObject;
	@:flash.property @:require(flash10) var isMouseTargetInaccessible(get,set) : Bool;
	@:flash.property var mouseTarget(get,set) : flash.display.InteractiveObject;
	function new(type : String, bubbles : Bool = false, cancelable : Bool = false, ?mouseTarget : flash.display.InteractiveObject, ?contextMenuOwner : flash.display.InteractiveObject) : Void;
	private function get_contextMenuOwner() : flash.display.InteractiveObject;
	private function get_isMouseTargetInaccessible() : Bool;
	private function get_mouseTarget() : flash.display.InteractiveObject;
	private function set_contextMenuOwner(value : flash.display.InteractiveObject) : flash.display.InteractiveObject;
	private function set_isMouseTargetInaccessible(value : Bool) : Bool;
	private function set_mouseTarget(value : flash.display.InteractiveObject) : flash.display.InteractiveObject;
	static final MENU_ITEM_SELECT : String;
	static final MENU_SELECT : String;
}
