extern class TextSnapshot
{
	function findText(startIndex:Float, textToFind:String, caseSensitive:Bool):Float;
	function getCount():Float;
	function getSelected(start:Float, end:Float):Bool;
	function getSelectedText(includeLineEndings:Bool):String;
	function getText(start:Float, end:Float, includeLineEndings:Bool):String;
	function hitTestTextNearPos(x:Float, y:Float, closeDist:Float):Float;
	function setSelectColor(color:Float):Void;
	function setSelected(start:Float, end:Float, select:Bool):Void;
}


