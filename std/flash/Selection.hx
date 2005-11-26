extern class Selection
{
	static function getBeginIndex():Float;
	static function getEndIndex():Float;
	static function getCaretIndex():Float;
	static function getFocus():String;
	static function setFocus(newFocus:Dynamic):Bool;
	static function setSelection(beginIndex:Float, endIndex:Float):Void;
	static function addListener(listener:Dynamic):Void;
	static function removeListener(listener:Dynamic):Bool;
}


