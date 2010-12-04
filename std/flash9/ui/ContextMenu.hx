package flash.ui;

@:final extern class ContextMenu extends flash.events.EventDispatcher {
	var builtInItems : ContextMenuBuiltInItems;
	var customItems : Array<Dynamic>;
	function new() : Void;
	function clone() : ContextMenu;
	function hideBuiltInItems() : Void;
}
