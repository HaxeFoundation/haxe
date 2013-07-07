package flash;

extern class System
{
	static var useCodepage:Bool;
	static var exactSettings:Bool;
	static function showSettings(?tabID:Float):Void;
	static function setClipboard(text:String):Void;
	static dynamic function onStatus(infoObject:Dynamic):Void;

	private static function __init__() : Void untyped {
		flash.System = _global["System"];
	}

}
