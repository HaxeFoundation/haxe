package flash;

extern class TextSnapshot
{
	function findText(startIndex:Int, textToFind:String, caseSensitive:Bool):Float;
	function getCount():Int;
	function getSelected(start:Int, ?end:Int):Bool;
	function getSelectedText(?includeLineEndings:Bool):String;
	function getText(start:Int, end:Int, ?includeLineEndings:Bool):String;
	function hitTestTextNearPos(x:Float, y:Float, ?closeDist:Float):Float;
	function setSelectColor(color:Int):Void;
	function setSelected(start:Int, end:Int, select:Bool):Void;

	private static function __init__() : Void untyped {
		flash.TextSnapshot = _global["TextSnapshot"];
	}

}
