package flash;

#if flash_strict
signature KeyListener {
	function onKeyDown() : Void;
	function onKeyUp() : Void;
}
#end

extern class Key
{
	static var ALT : Int = 18;
	static var ENTER : Int = 13;
	static var SPACE : Int = 32;
	static var UP : Int = 38;
	static var DOWN : Int = 40;
	static var LEFT : Int = 37;
	static var RIGHT : Int = 39;
	static var PGUP : Int = 33;
	static var PGDN : Int = 34;
	static var HOME : Int = 36;
	static var END : Int = 35;
	static var TAB : Int = 9;
	static var CONTROL : Int = 17;
	static var SHIFT : Int = 16;
	static var ESCAPE : Int = 27;
	static var INSERT : Int = 45;
	static var DELETEKEY : Int = 46;
	static var BACKSPACE : Int = 8;
	static var CAPSLOCK : Int = 20;

	// hide : static property _listeners(default,null) : Array<Dynamic>;

	static function getAscii():Int;
	static function getCode():Int;
	static function isDown(code:Int):Bool;
	static function isToggled(code:Int):Bool;

#if flash_strict
	static function addListener(listener:KeyListener):Void;
	static function removeListener(listener:KeyListener):Bool;
#else true
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Bool;
#end

	private static function __init__() : Void untyped {
		flash.Key = _global["Key"];
	}

}
