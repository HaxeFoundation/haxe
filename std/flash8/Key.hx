package flash;

#if flash_strict
typedef KeyListener = {
	function onKeyDown() : Void;
	function onKeyUp() : Void;
}
#end

extern class Key
{
	static inline var ALT : Int = 18;
	static inline var ENTER : Int = 13;
	static inline var SPACE : Int = 32;
	static inline var UP : Int = 38;
	static inline var DOWN : Int = 40;
	static inline var LEFT : Int = 37;
	static inline var RIGHT : Int = 39;
	static inline var PGUP : Int = 33;
	static inline var PGDN : Int = 34;
	static inline var HOME : Int = 36;
	static inline var END : Int = 35;
	static inline var TAB : Int = 9;
	static inline var CONTROL : Int = 17;
	static inline var SHIFT : Int = 16;
	static inline var ESCAPE : Int = 27;
	static inline var INSERT : Int = 45;
	static inline var DELETEKEY : Int = 46;
	static inline var BACKSPACE : Int = 8;
	static inline var CAPSLOCK : Int = 20;

	// hide : static property _listeners(default,null) : Array<Dynamic>;

	static function getAscii():Int;
	static function getCode():Int;
#if flash8
	static function isAccessible():Bool;
#end
	static function isDown(code:Int):Bool;
	static function isToggled(code:Int):Bool;

	static dynamic function onKeyDown() : Void;
	static dynamic function onKeyUp() : Void;

#if flash_strict
	static function addListener(listener:KeyListener):Void;
	static function removeListener(listener:KeyListener):Bool;
#else
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Bool;
#end

	private static function __init__() : Void untyped {
		flash.Key = _global["Key"];
		flash.Key.addListener(flash.Key);
	}

}
