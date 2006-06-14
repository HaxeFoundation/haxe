package flash;

extern class Selection
{
	static function getBeginIndex():Int;
	static function getEndIndex():Int;
	static function getCaretIndex():Int;
	static function getFocus():String;
	static function setFocus(newFocus:Dynamic):Bool;
	static function setSelection(beginIndex:Int, endIndex:Int):Void;
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Bool;

	private static function __init__() : Void untyped {
 		flash.Selection = _global["Selection"];
	}

}
