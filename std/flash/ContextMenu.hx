package flash;

extern class ContextMenu {
	public function new( ?callb:Dynamic->ContextMenu->Void ) : Void;
	public function hideBuiltInItems():Void;
	public function copy() : ContextMenu;

	public var customItems:Array<ContextMenuItem>;
	public var builtInItems:Dynamic;
	public function onSelect( v : Dynamic, c : ContextMenu ) : Void;

	private static function __init__() : Void untyped {
		flash.ContextMenu = _global["ContextMenu"];
	}
}
