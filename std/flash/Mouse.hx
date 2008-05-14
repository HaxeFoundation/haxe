package flash;

#if flash_strict
typedef MouseListener = {
	function onMouseDown() : Void;
	function onMouseMove() : Void;
	function onMouseUp() : Void;
	function onMouseWheel( delta : Float, scrollTarget : String ) : Void;
}
#end

extern class Mouse
{
	static function show():Int;
	static function hide():Int;
	static dynamic function onMouseDown() : Void;
	static dynamic function onMouseMove() : Void;
	static dynamic function onMouseUp() : Void;

#if flash_strict
	static function addListener(listener:MouseListener):Void;
	static function removeListener(listener:MouseListener):Bool;
#else true
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Bool;
#end

	private static function __init__() : Void untyped {
		flash.Mouse = _global["Mouse"];
		flash.Mouse.addListener(cast flash.Mouse);
	}

}
