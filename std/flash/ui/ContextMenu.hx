package flash.ui;

extern final class ContextMenu extends flash.display.NativeMenu {
	@:flash.property var builtInItems(get,set) : ContextMenuBuiltInItems;
	@:flash.property @:require(flash10) var clipboardItems(get,set) : ContextMenuClipboardItems;
	@:flash.property @:require(flash10) var clipboardMenu(get,set) : Bool;
	@:flash.property var customItems(get,set) : Array<Dynamic>;
	@:flash.property @:require(flash10) var link(get,set) : flash.net.URLRequest;
	function new() : Void;
	function clone() : ContextMenu;
	private function get_builtInItems() : ContextMenuBuiltInItems;
	private function get_clipboardItems() : ContextMenuClipboardItems;
	private function get_clipboardMenu() : Bool;
	private function get_customItems() : Array<Dynamic>;
	private function get_link() : flash.net.URLRequest;
	function hideBuiltInItems() : Void;
	private function set_builtInItems(value : ContextMenuBuiltInItems) : ContextMenuBuiltInItems;
	private function set_clipboardItems(value : ContextMenuClipboardItems) : ContextMenuClipboardItems;
	private function set_clipboardMenu(value : Bool) : Bool;
	private function set_customItems(value : Array<Dynamic>) : Array<Dynamic>;
	private function set_link(value : flash.net.URLRequest) : flash.net.URLRequest;
	@:flash.property @:require(flash10_1) static var isSupported(get,never) : Bool;
	private static function get_isSupported() : Bool;
}
