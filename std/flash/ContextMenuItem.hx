package flash;

extern class ContextMenuItem {
	public function new(caption:String, callb:Dynamic->ContextMenuItem->Void, ?separatorBefore:Bool, ?enabled:Bool, ?visible:Bool) : Void;
	public function copy() : ContextMenuItem;

	public function onSelect( v : Dynamic, c : ContextMenuItem ) : Void;

	public var enabled:Bool;
	public var visible:Bool;
	public var caption:String;
	public var separatorBefore:Bool;

	private static function __init__() : Void untyped {
		flash.ContextMenuItem = _global["ContextMenuItem"];
	}
}

