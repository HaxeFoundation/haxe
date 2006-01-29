package flash;

extern class System
{
	static var useCodepage:Bool;
	static var exactSettings:Bool;
	static function showSettings(tabID:Float):Void;
	static function setClipboard(text:String):Void;
	static function onStatus(infoObject:Dynamic):Void;
}


