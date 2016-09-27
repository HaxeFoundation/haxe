package flash.ui;

@:final extern class ContextMenu extends flash.display.NativeMenu {
	var builtInItems : ContextMenuBuiltInItems;
	@:require(flash10) var clipboardItems : ContextMenuClipboardItems;
	@:require(flash10) var clipboardMenu : Bool;
	var customItems : Array<Dynamic>;
	@:require(flash10) var link : flash.net.URLRequest;
	function new() : Void;
	function clone() : ContextMenu;
	function hideBuiltInItems() : Void;
	@:require(flash10_1) static var isSupported(default,never) : Bool;
}
