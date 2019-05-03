package flash.ui;

extern final class ContextMenuClipboardItems {
	var clear(get,set) : Bool;
	var copy(get,set) : Bool;
	var cut(get,set) : Bool;
	var paste(get,set) : Bool;
	var selectAll(get,set) : Bool;
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
