package flash;

extern class ContextMenuItem {
	function new(caption:String, callb:Dynamic->ContextMenuItem->Void, ?separatorBefore:Bool, ?enabled:Bool, ?visible:Bool) : Void;
	function copy() : ContextMenuItem;

	dynamic function onSelect( v : Dynamic, c : ContextMenuItem ) : Void;

	var enabled:Bool;
	var visible:Bool;
	var caption:String;
	var separatorBefore:Bool;

	private static function __init__() : Void untyped {
		flash.ContextMenuItem = _global["ContextMenuItem"];
	}
}

