package flash;

extern class ContextMenu {
	function new( ?callb:Dynamic->ContextMenu->Void ) : Void;
	function hideBuiltInItems():Void;
	function copy() : ContextMenu;

	var customItems:Array<ContextMenuItem>;
	var builtInItems:Dynamic;
	dynamic function onSelect( v : Dynamic, c : ContextMenu ) : Void;

	private static function __init__() : Void untyped {
		flash.ContextMenu = _global["ContextMenu"];
	}
}
