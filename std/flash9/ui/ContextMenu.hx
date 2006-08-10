package flash.ui;

extern class ContextMenu extends flash.events.EventDispatcher {
	function new() : Void;
	var builtInItems : flash.ui.ContextMenuBuiltInItems;
	function clone() : flash.ui.ContextMenu;
	var customItems : Array<Dynamic>;
	function hideBuiltInItems() : Void;
}
