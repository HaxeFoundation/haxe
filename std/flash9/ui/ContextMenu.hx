package flash.ui;

extern class ContextMenu extends flash.events.EventDispatcher {
	var builtInItems : ContextMenuBuiltInItems;
	var customItems : Array<Dynamic>;
	function new() : Void;
	function clone() : ContextMenu;
	function hideBuiltInItems() : Void;
	#if flash10
	var clipboardItems : ContextMenuClipboardItems;
	var clipboardMenu : Bool;
	var link : flash.net.URLRequest;
	#end
}
