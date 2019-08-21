package flash.ui;

extern final class ContextMenuClipboardItems {
	@:flash.property var clear(get,set) : Bool;
	@:flash.property var copy(get,set) : Bool;
	@:flash.property var cut(get,set) : Bool;
	@:flash.property var paste(get,set) : Bool;
	@:flash.property var selectAll(get,set) : Bool;
	function new() : Void;
	function clone() : ContextMenuClipboardItems;
	private function get_clear() : Bool;
	private function get_copy() : Bool;
	private function get_cut() : Bool;
	private function get_paste() : Bool;
	private function get_selectAll() : Bool;
	private function set_clear(value : Bool) : Bool;
	private function set_copy(value : Bool) : Bool;
	private function set_cut(value : Bool) : Bool;
	private function set_paste(value : Bool) : Bool;
	private function set_selectAll(value : Bool) : Bool;
}
