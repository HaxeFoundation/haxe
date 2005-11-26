extern class Key
{
	static function getAscii():Int;
	static function getCode():Int;
	static function isDown(code:Int):Bool;
	static function isToggled(code:Int):Bool;

	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Bool;
}
