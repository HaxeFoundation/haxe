package flash.ui;

extern final class ContextMenuItem extends flash.display.NativeMenuItem {
	@:flash.property var caption(get,set) : String;
	@:flash.property var separatorBefore(get,set) : Bool;
	@:flash.property var visible(get,set) : Bool;
	function new(caption : String, separatorBefore : Bool = false, enabled : Bool = true, visible : Bool = true) : Void;
	function clone() : ContextMenuItem;
	private function get_caption() : String;
	private function get_separatorBefore() : Bool;
	private function get_visible() : Bool;
	private function set_caption(value : String) : String;
	private function set_separatorBefore(value : Bool) : Bool;
	private function set_visible(value : Bool) : Bool;
}
